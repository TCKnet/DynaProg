package v4

// These trait are interfaces for compiled CUDA wrappers
trait ADPWrapper[A,R] {
  def parse(in:Array[A]):R
  def backtrack(in:Array[A]):(R,List[((Int, Int),(Int,List[Int]))])
}
trait TTWrapper[A,R] {
  def parse(in1:Array[A],in2:Array[A]):R
  def backtrack(in1:Array[A],in2:Array[A]):(R,List[((Int, Int),(Int,List[Int]))])
}

trait CodeGen extends BaseParsers { this:Signature => reqJNI
  protected val head = new CodeHeader(this) // User code and helpers store
  private def tpAlphabet = head.parse(tps._1.toString)
  private def tpAnswer = head.parse(tps._2.toString)

  // Additional typing informations required for codegen. We need to use Manifest due to following limitations:
  // - TypeTag: type (i.e. (Int,Int)) not necessary linked to class (examples.Test.Alphabet instead of examples.TestSig$Mat)
  // - ClassTag: provides only the class, no insight of Tuple inner types
  val tps:(Manifest[Alphabet],Manifest[Answer])

  // Cost matrix : cost_t = parser_name -> cost
  // Backtracking: back_t = parser_name -> (rule, positions)
  def btTpe(n:Int) = head.addType("short rule"+(if(n>0)"; short pos["+n+"]"else""),"bt"+n)

  // Normalize the parsers towards code generation
  // Canonical : [Tabulate] > Aggregate > Or > Filter > Map > Concat > (Tabulate | Terminal)
  // C code    : [Tabulate] > Or > ForLoop+Filter > Aggregate1 > Map > (Tabulate | Terminal)
  // Invariant : Aggregate > Map > Concat (due to domain change)
  def norm[T](parser:Parser[T]):Parser[T] = parser match {
    case Aggregate(p0,h) => norm(p0) match {
      case Aggregate(p,h1) if (h1==h) => p0
      case Or(l,r) => Or(norm(Aggregate(l,h)),norm(Aggregate(r,h)))
      case Filter(p,f) => Filter(norm(Aggregate(p,h)),f)
      case p => Aggregate(p,h)
    }
    case Or(l0,r0) => (norm(l0),norm(r0)) match {
      case (Filter(p1,c1),Filter(p2,c2)) if (c1==c2) => Filter(Or(p1,p2),c1)
      case (Or(a,b),c) if (a==c) => norm(Or(b,c))
      case (Or(a,b),c) if (b==c) => norm(Or(a,c))
      case (a,Or(b,c)) if (a==b) => norm(Or(a,c))
      case (a,Or(b,c)) if (a==c) => norm(Or(a,b))
      case (l,r) if (l==r) => l
      case (l,r) => Or(l,r)
    }
    case Map(p0,f) => norm(p0) match {
      case Or(l,r) => Or(norm(Map(l,f)),norm(Map(r,f)))
      case Filter(p,c) => Filter(norm(Map(p,f)),c)
      case p => Map(p,f)
    }
    case Filter(p,c) => Filter(norm(p),c)
    case cc@Concat(l0,r0,t) => // Preserves alternatives numbering and correct min/max indices
      def c[T,U](l:Parser[T],r:Parser[U]) = new Concat(l,r,t){override lazy val indices=cc.indices}
      (norm(l0),norm(r0)) match {
        case (Or(a,b),r) => Or(norm(c(a,r)),norm(c(b,r)))
        case (l,Or(a,b)) => Or(norm(c(l,a)),norm(c(l,b)))
        case (l,r) => c(l,r)
      }
    case _ => parser
  }

  // A simple variable generator that sequentially issues free variables
  class FreeVar(v0:Char) {
    private var v=v0
    def get = { var r=v; v=(v+1).toChar; Var(r,0) }
    def dup = new FreeVar(v0)
  }

  // Optimizations and conditions simplifications, voluntarily not comprehensive as C compiler re-optimize them later
  def simplify(conds: List[Cond]):List[Cond] = {
    var cs = conds.distinct.filter { case CEq(a,b,0) if (a==b) => false case _ => true } // 1. filter x+0=x
    cs = cs.map { case CFor(v,l,ld,u,ud,lD,uD) => var lm=ld; var um=ud; var lM=lD; var uM=uD // 2. minimize the range of for loop
        cs.foreach {
          case CLeq(a,b,d) =>
            if (a==l && b==v && d>lm) lm=d; if (a==v && b==l && -d>lM) lM= -d;
            if (a==v && b==u && d>um) um=d; if (a==u && b==v && -d>uM) uM= -d;
          case _ =>
        }
        CFor(v,l,lm,u,um,lM,uM)
      case x => x
    }
    cs.foreach { // 3. drop useless Leq (either contained by a For or superseded by another constraint on the same pair)
      case CLeq(a,b,x) => cs=cs.filter { case CLeq(c,d,y) if (c==a && d==b && y<x) => false case _ => true }
      case CFor(v,l,_,u,_,_,_) => cs=cs.filter { case CLeq(c,d,_) if (c==l && d==v || c==v && d==u || c==v && d==l || c==u && d==v) => false case _ => true }
      case _ =>
    }; cs
  }

  // Code indentation helper
  def ind(s:String,n:Int=1) = { val i="  "*n; i+s.replaceAll(" +$","").replace("\n","\n"+i)+"\n" }

  // --------------------------------------------------------------------------
  // Tabulation and kernel code generation

  def genFun[T,U](f0:T=>U):String = { val f1 = f0 match { case d:DeTuple => d.f case f => f }
    f1 match { case f:CFun => head.add(f) case _ => sys.error("Unsupported function: "+f1) }
  }

  def genTab(t:Tabulate):String = {
    var aggid=0; // nested aggregation intermediate result id
    def scs(min:Int,max:Int,i:Var,j:Var) = if (min==max) List(i.eq(j,min)) else (if (max==maxN) Nil else List(j.leq(i,-max))):::(if (min>0) List(i.leq(j,min)) else Nil)
    // Generate C parser for subword (i,j): collect conditions, hoisted/content and backtrack indices
    // (parser, i,j,k?, subrule, aggregation_depth) => (Conditions+for loops, hoisted, body, backtrack indices)
    def gen[T](p0:Parser[T],i:Var,j:Var,g:FreeVar,rule:Int,aggr:Int):(List[Cond],String,String,List[String]) = p0 match {
      case Terminal(min,max,f) => val (cs,s)=f(i,j); (scs(min,max,i,j):::cs,"",s,Nil)
      case p:Tabulate => ( (if (p.alwaysValid) Nil else List(CUser("VALID("+i+","+j+","+p.name+")"))):::scs(p.min,p.max,i,j),"","cost[idx("+i+","+j+")]."+p.name,Nil)
      case Aggregate(p,h) => val (c,hb,b,bti)=gen(p,i,j,g,rule,aggr+1);
        def bodyTpe:String = { // fallback to "typeof(body)" if untypable, where body has no fresh variable
          val g0:FreeVar = new FreeVar('0') { override def get=zero; override def dup=this }
          val (_,_,lb,_)=gen(p,zero,zero,g0,0,0); "typeof("+lb+")"
        }
        val tpe = (p,h) match {
          case (_,MinBy(f:CFun)) => head.getType(f.args.head._2)
          case (_,MaxBy(f:CFun)) => head.getType(f.args.head._2)
          case (Map(_,f0),_) => val f1 = f0 match { case d:DeTuple => d.f case f => f }
            f1 match { case f:CFun => head.getType(f.tpe) case _ => bodyTpe }
          case _ => bodyTpe
        }
        val (tc,tb) = if(aggr==0) ("_c0","_b0") else { aggid=aggid+1; ("_c"+aggid, "_b"+aggid) }
        // Generate aggregation body
        val updt = "{ "+tc+"=_c; "+tb+"=("+btTpe(if (aggr==0) t.inner.cat else p.cat)+"){"+rule+(if (p.cat>0)",{"+bti.mkString(",")+"}" else "")+"}; }";
        val or_empty = if (cudaEmpty!=null) "" else " || "+tb+".rule==-1"
        val cc = "{ "+(h.toString match {
          case "$$max$$" => tpe+" _c="+b+"; if (_c>"+tc+or_empty+") "+updt
          case "$$min$$" => tpe+" _c="+b+"; if (_c<"+tc+or_empty+") "+updt
          case "$$count$$" => tc+"+=1;"
          case "$$sum$$" => tc+"+="+b+";"
          case "$$maxBy$$" => val f=genFun(h.asInstanceOf[MaxBy[Any,Any]].f); tpe+" _c="+b+"; if ("+f+"(_c)>"+f+"("+tc+")"+or_empty+") "+updt
          case "$$minBy$$" => val f=genFun(h.asInstanceOf[MinBy[Any,Any]].f); tpe+" _c="+b+"; if ("+f+"(_c)<"+f+"("+tc+")"+or_empty+") "+updt
          case _ => sys.error("Unsupported aggregation: "+b)
        })+" }"
        if (aggr==0) (c,hb,cc,bti) // hoist aggregation if contained
        else { val nv = tpe+" "+tc+"; "+btTpe(p.cat)+" "+tb+"={-1"+(if (p.cat>0)",{}" else "")+"};\n";
          (List(CUser(tb+".rule!=-1")),nv+emit((c,hb,cc,bti)),tc,(0 until p.cat).map{x=>tb+".pos["+x+"]"}.toList)
        }
      case Or(l,r) => (Nil,"",emit(gen(l,i,j,g.dup,rule,aggr))+"\n"+emit(gen(r,i,j,g.dup,rule+l.alt,aggr)),Nil)
      case Map(p,f) => val (c,hb,b,bti)=gen(p,i,j,g,rule,aggr); (c,hb,genFun(f)+"("+b+")",bti)
      case Filter(p,f) => val (c,hb,b,bti)=gen(p,i,j,g,rule,aggr); (CUser(genFun(f)+"("+i+","+j+")")::c,hb,b,bti)
      case cc@Concat(l,r,tt) =>
        def bf1(f:Int, l:Int, u:Int):List[Cond] = { val ls=List(i.leq(j,f+l)); if (u!=maxN) j.leq(i,-f-u)::ls else ls }
        val (c,k):(List[Cond],Var) = (tt,cc.indices) match {
          // low=up in at least one side
          case (0,(iL,iU,jL,jU)) if (iL==iU && jL==jU) => (List(i.eq(j,iL+jL)), i.add(iL))
          case (0,(iL,iU,jL,jU)) if (iL==iU) => (bf1(iL,jL,jU), i.add(iL))
          case (0,(iL,iU,jL,jU)) if (jL==jU) => (bf1(jL,iL,iU), j.add(-jL))
          // most general case
          case (0,(iL,iU,jL,jU)) => val k0=g.get; var cs:List[Cond]=Nil;
            if (jU!=maxN) cs = j.leq(k0,-jU) :: cs
            if (iU!=maxN) cs = k0.leq(i,-iU) :: cs // we might want to simplify if min_k==i || max_k==j
            (CFor(k0.v,i.v,iL,j.v,jL)::cs, k0)
          // concat1,concat2
          case (t,(a,b,c,d)) if (t==1||t==2) => val (l,u,v) = if (t==1) (a,b,i) else (c,d,j)
            if (l==u) (List(zero.leq(v,l)), v.add(-l))
            else { val k0=g.get; val cu=if (u==maxN) Nil else List(k0.leq(v,u)); (CFor(k0.v,'0',0,v.v,l)::cu, k0) }
        }
        val (lc,lhb,lb,lbt) = if (tt==1) gen(l,k,i,g,rule,aggr) else gen(l,i,k,g,rule,aggr)
        val (rc,rhb,rb,rbt) = gen(r,k,j,g,rule,aggr)
        (simplify(c:::lc:::rc), lhb+(if (lhb!=""&&rhb!="") "\n" else "")+rhb, lb+","+rb, lbt:::(if(cc.hasBt) List(k.toString) else Nil):::rbt)
      case _ => sys.error("Unknown parser")
    }
    // Generate the loops and size conditions (conditions, hoisted, body, indices)
    def emit(cb:(List[Cond],String,String,List[String])):String = cb match { case (cs0,hbody,body,_) =>
      val cs = (simplify(cs0).map{
        case CEq(a,b,d) => CUser(Var(a,d)+"=="+b)
        case CLeq(a,b,d) => CUser(Var(a,d)+"<="+b)
        case c => c
      } foldRight List[Cond]()) { case (CUser(c1),CUser(c2)::as) => CUser(c1+" && "+c2)::as case (c,acc)=>c::acc }
      var hb=false; var br=false
      val cc=(cs foldRight body){
        case (CFor(v,l,ld,u,ud,lD,uD),b) => val b2=(if ((hbody+b).contains("_unroll")) "" else "_unroll ")+
          "for(int "+v+"="+
            (if (uD== -1) ""+Var(l,ld) else "MAX("+Var(l,ld)+","+Var(u,-uD)+")")+ // lower bound
            (if (lD!= -1) ","+v+"u=MIN("+u+"-"+ud+","+l+"+"+lD+"); "+v+"<="+v+"u"
             else ud match { case 0 => "; "+v+"<="+u case 1 => "; "+v+"<"+u case _ => ","+v+"u="+u+"-"+ud+"; "+v+"<="+v+"u" }
            )+"; ++"+v+") {\n"+ind((if (!hb&&hbody!="") hbody+"\n" else "")+b)+"}"; hb=true; br=true; b2
        case (CUser(c),b) => val b2="if ("+c+") "+(if (!br) "{\n"+ind(b)+"}" else b); br=true; b2
        case _ => "" // does not happen
      }
      if (hb==false) hbody+cc else cc
    }
    // Generate the whole tabulation
    val body = emit(gen(norm(Aggregate(t.inner,h)),Var('i',0),Var('j',0),new FreeVar('k'),t.id,0))
    "bt"+t.inner.cat+" _b0 = {-1"+(if(t.inner.cat>0) ",{"+(0 until t.inner.cat).map{_=>"0"}.mkString(",")+"}" else "")+"};\n"+
    tpAnswer+" _c0"+(if (cudaEmpty!=null)" = "+cudaEmpty else "")+";\n"+body+"\ncost[idx(i,j)]."+t.name+" = _c0;\nback[idx(i,j)]."+t.name+" = _b0;"
  }

  def genKern(gpu:Boolean=true) = {
    val kern = "#define VALID(I,J,RULE) "+(if (cudaEmpty!=null) "(cost[idx(I,J)].RULE!="+cudaEmpty+")\n" else "(back[idx(I,J)].RULE.rule!=-1)\n")+
               rulesOrder.map{n=> "// ---- "+n+"[i,j] ----\n"+"{\n"+ind(genTab(rules(n)))+"}" }.mkString("\n")
    if (gpu) {
      val loops = "for (unsigned jj=s_start; jj<s_stop; ++jj) {\n"+
        (if (twotracks) "  for (int i=tI; i<M_H; i+=tN) {\n    int j = jj-i; if (j>=0)" else "  for (int ii=tI; ii<M_H; ii+=tN) {\n    int i=M_H-1-ii, j=i+jj;")+"\n"
      "__global__ void gpu_solve(const input_t* in1, const input_t* in2, cost_t* cost, back_t* back, volatile unsigned* lock, unsigned s_start, unsigned s_stop) {\n"+ind(
      "const unsigned tI = threadIdx.x + blockIdx.x * blockDim.x;\n"+
      "const unsigned tN = blockDim.x * gridDim.x;\n"+
      "const unsigned tB = blockIdx.x;\n"+
      (if (cudaSharedInput) "for (unsigned i=threadIdx.x; i<M_W-1; i+=blockDim.x) _in1[i]=__in1[i];\n"+
           (if (twotracks) "for (unsigned i=threadIdx.x; i<M_H-1; i+=blockDim.x) _in2[i]=__in2[i];\n" else "")+"__syncthreads();\n" else "")+
      "unsigned tP=s_start; // block progress\n"+loops+"    if (j<M_W) {\n"+ind(kern,3)+"    }\n  }\n"+
      "  // Sync between blocks, removing __threadfence() here is incorrect but works\n  // __threadfence();\n"+
      "  if (threadIdx.x==0) { lock[tB]=++tP; if (tB) while(lock[tB-1]<tP) {} }\n  __syncthreads();\n}")+"}\n"
    } else {
      val loops = (if (twotracks) "for (int i=0; i<M_H; ++i) {\n  for (int j=0; j<M_W; ++j) {"
                             else "for (int jj=0; jj<M_W; ++jj) {\n  for (int ii=jj; ii<M_H; ++ii) {\n    int i=M_H-1-ii, j=i+jj;")+"\n"
      "void cpu_solve(const input_t* _in1, const input_t* _in2, cost_t* cost, back_t* back) {\n"+ind(loops+ind(kern,2)+"  }\n}")+"}\n"
    }
  }

  // --------------------------------------------------------------------------
  // Backtrack generation

  def genUnapp[T](p0:Parser[T],sw:(String,String),rule:Int,bti:Int) : List[((String,String),Int)] = p0 match { // i,j,rule
    case Terminal(_,_,_) => Nil
    case p:Tabulate => List((sw,p.id))
    case Aggregate(p,h) => genUnapp(p,sw,rule,bti)
    case Filter(p,f) => genUnapp(p,sw,rule,bti)
    case Map(p,f) => genUnapp(p,sw,rule,bti)
    case Or(l,r) => var a=l.alt-1; if (rule<=a) genUnapp(l,sw,rule,bti) else genUnapp(r,sw,rule-a,bti)
    case cc@Concat(left,right,track) => val a:Int=right.alt; val c:Int=left.cat; val kb = "rd->pos["+(bti+c)+"]"
      val (swl,swr):((String,String),(String,String)) = (sw,track,cc.indices) match {
        case ((i,j),0,(lL,lU,rL,rU)) =>
          val k=if (cc.hasBt) kb else if (rU==maxN) i+"+"+lL else "MAX("+i+"+"+lL+","+j+"-"+rU+")"; ((i,k),(k,j))
        case ((i,j),1,(l,u,_,_)) => val k=if (cc.hasBt) kb else i+"-"+l; ((k,i),(k,j)) // tt:concat1
        case ((i,j),2,(_,_,l,u)) => val k=if (cc.hasBt) kb else j+"-"+l; ((i,k),(k,j)) // tt:concat2
        case _ => (("0","0"),("0","0"))
      }
      genUnapp(left,swl,rule/a,bti) ::: genUnapp(right,swr,rule%a,bti+c+(if (cc.hasBt)1 else 0))
    case _ => sys.error("Unknown parser")
  }

  private lazy val catMax = rules.map{case (n,t)=>t.inner.cat}.max
  def genBT(gpu:Boolean=true) = {
    val altMax=rules.map{case (n,t)=>t.inner.alt}.sum
    def switch(f:Int=>String) = "    switch (rd->rule) {\n"+(0 until altMax).map{x=>"      case "+x+":"+f(x)+" break;"}.mkString("\n")+"\n    }\n"
    val trLen = "const unsigned trace_len["+altMax+"] = {"+(0 until altMax).map{x=>findTab(x)._1.inner.cat}.mkString(",")+"};" // subrule->#indices
    head.add("#define MAX(a,b) ({__typeof__(a) _a=(a); __typeof__(b) _b=(b); _a>_b?_a:_b; })") // for 'for' loops and concat's MAX()
    head.add("#define MIN(a,b) ({__typeof__(a) _a=(a); __typeof__(b) _b=(b); _a<_b?_a:_b; })") // for 'for' loops
    head.add("typedef struct { short i,j,rule; "+(if(catMax>0)"short pos["+catMax+"]; " else "")+"} trace_t;")
    head.add(trLen)
    (if (gpu) "__global__ void gpu" else "void cpu")+"_backtrack(trace_t* trace, unsigned* size, back_t* back, int i0, int j0) {\n"+ // start at (i0,j0)
    "  "+(if(catMax>0)trLen+"\n  " else "")+"trace_t *rd=trace, *wr=trace; *size=0;\n"+
    "  #define PUSH_BACK(I,J,RULE) { wr->i=I; wr->j=J; wr->rule=RULE; ++wr; ++(*size); }\n"+
    "  PUSH_BACK(i0,j0,"+axiom.id+");\n"+
    "  for(;rd<wr;++rd) {\n"+
    "    "+btTpe(catMax)+"* bt;\n"+
    switch((x:Int)=>" bt=("+btTpe(catMax)+"*)&back[idx(rd->i,rd->j)]."+findTab(x)._1.name+";")+
    "    rd->rule=bt->rule;\n"+  // parser_id -> actual_subrule
    (if(catMax>0)"    for (int i=0,l=trace_len[rd->rule]; i<l; ++i) rd->pos[i]=bt->pos[i];\n" else "")+ // decode fully the read indices
    switch((x:Int)=>{val t=findTab(x)._1; genUnapp(t.inner,("rd->i","rd->j"),x-t.id,0).map{case((i,j),r)=>" PUSH_BACK("+i+","+j+","+r+");" }.mkString("")})+
    "  }\n}\n"
  }

  // --------------------------------------------------------------------------
  // CUDA helpers

  def genHelpers(stripe:Int=32) = {
    head.addPriv("#include <stdio.h>\n#include <stdlib.h>\n#include \"{file}.h\"\n\n"+
      "#define cuReset cudaDeviceReset()\n"+
      "#define cuDevSync cudaDeviceSynchronize()\n"+
      "#define cuErr(err) cuErr_(err,__FILE__,__LINE__)\n"+
      "__attribute__((unused)) static inline void cuErr_(cudaError_t err, const char *file, int line) {\n  if (err==cudaSuccess) return;\n"+
      "  fprintf(stderr,\"%s:%i CUDA error %d:%s\\n\", file, line, err, cudaGetErrorString(err)); cuReset; exit(EXIT_FAILURE);\n}\n"+
      "#define cuMalloc(ptr,size) cuErr(cudaMalloc((void**)&ptr,size))\n"+
      "#define cuFree(ptr) cuErr(cudaFree(ptr))\n"+
      "#define cuPut(host,dev,size,stream) cuErr(cudaMemcpyAsync(dev,host,size,cudaMemcpyHostToDevice,stream))\n"+
      "#define cuGet(host,dev,size,stream) cuErr(cudaMemcpyAsync(host,dev,size,cudaMemcpyDeviceToHost,stream))\n"+
      "#define cuMap(host,dev,size) { cuErr(cudaHostAlloc((void**)&host,size,cudaHostAllocMapped)); cuErr(cudaHostGetDevicePointer((void**)&dev,host,0)); }\n"+
      "#define cuUnmap(host) cuErr(cudaFreeHost(host))\n"+
      "#define cuStream(stream) cudaStream_t stream; cuErr(cudaStreamCreate(&stream));\n"+
      "#define cuSync(stream) cuErr(cudaStreamSynchronize(stream))\n"+
      "#define cuStreamDestroy(stream) cuErr(cudaStreamDestroy(stream))\n"+
      "#define cuAlloc2(cond,host,dev,size) bool cond = cudaMalloc((void**)&dev,size)==cudaSuccess; if (!cond) { cuMap(host,dev,size); }\n"+
      "#define cuFree2(host,dev) { if (host!=NULL) { cuUnmap(host); host=NULL; } else cuFree(dev); dev=NULL; }"
    )
    head.addPriv("#define _unroll _Pragma(\"unroll "+(if(useRna) 1 else cudaUnroll)+"\")") // experimental optimal
    head.addPriv("#define M_W {MAT_WIDTH}")
    head.addPriv("#define M_H {MAT_HEIGHT}")
    if (twotracks) {
      head.addPriv("#define B_H "+stripe) // blocks stripe height (coalesce diagonal elements along the same stripe)
      head.addPriv("#define MEM_MATRIX (M_W* ((M_H+B_H-1)/B_H)*B_H  +B_H*B_H)")
      head.addPriv("#define idx(i,j) ({ unsigned _i=(i); (B_H*((j)+(_i%B_H)) + (_i%B_H) + (_i/B_H)*M_W*B_H); })")
    } else { // idx(i,j) = MEM_MATRIX-UP_TRI+i, UP_TRI=d*(d+1)/2, d=M_H+1+_i-_j (smallest triangle including position element)
      head.addPriv("#define MEM_MATRIX ((M_H*(M_H+1))/2)") // upper right triangle, including diagonal
      head.addPriv("#define idx(i,j) ({ unsigned _i=(i),_d=M_H+1+_i-(j); MEM_MATRIX - ((_d*(_d-1))>>1) +_i; })")
    }
    head.addPriv("static input_t *g_in1 = NULL, *g_in2 = NULL;\nstatic cost_t *g_cost = NULL;\nstatic back_t *g_back = NULL;")
    val din=if(cudaSharedInput)"__in"else"_in" // book shared memory if necessary
    head.addPriv("__device__ static __attribute__((unused)) input_t *"+din+"1=NULL, *"+din+"2=NULL;\n__global__ void gpu_input(input_t* in1, input_t* in2) { "+din+"1=in1; "+din+"2=in2; }")
    if (cudaSharedInput) head.addPriv("__shared__ input_t _in1[M_H]"+(if (twotracks) ", _in2[M_W]" else "")+";")
    head.add("void my_init(input_t* in1, input_t* in2);\nvoid my_free();\nvoid my_solve();\n"+tpAnswer+" my_backtrack(trace_t** trace, unsigned* size);")

    // Initialize and free device memory for cost and backtrack matrices
    val init = "static cost_t* c_cost=NULL;\nstatic back_t* c_back=NULL;\n\n"+
      "void my_init(input_t* in1, input_t* in2) {\n"+ind(
      "int dev="+cudaDevice+"; cuErr("+(if(cudaDevice>=0)"cudaSetDevice(dev)" else "cudaGetDevice(&dev)")+");\n"+
      "cuMalloc(g_in1,sizeof(input_t)*(M_H-1)); cuPut(in1,g_in1,sizeof(input_t)*(M_H-1),NULL);\n"+
      (if (twotracks) "cuMalloc(g_in2,sizeof(input_t)*(M_W-1)); cuPut(in2,g_in2,sizeof(input_t)*(M_W-1),NULL);\n" else "g_in2=NULL;\n")+
      (if (useRna)"rna_init();\n" else "")+
      "size_t s_cost = sizeof(cost_t)*MEM_MATRIX;\n"+
      "size_t s_back = sizeof(back_t)*MEM_MATRIX;\n"+
      // Test & fail memory allocation (more reliable than estimations)
      "cuAlloc2(costDev,c_cost,g_cost,s_cost); cuAlloc2(backDev,c_back,g_back,s_back);\ngpu_input<<<1,1>>>(g_in1,g_in2);"+
      (if (!benchmark)"" else "\ncudaDeviceProp prop; cuErr(cudaGetDeviceProperties(&prop, dev));\n"+
        "size_t mem = (sizeof(input_t)+sizeof(trace_t))*(M_W+M_H) + s_cost + s_back;\n"+
        "printf(\"%-20s : %.2fMb / %.2fMb [in=%ld,tr=%ld,cost=%ld,back=%ld] -> cost:%s, backtrack:%s\\n\",\"Memory selection\","+
        "mem/1048576.0,prop.totalGlobalMem/1048576.0, sizeof(input_t),sizeof(trace_t),sizeof(cost_t),sizeof(back_t), costDev?\"device\":\"host\", backDev?\"device\":\"host\");")
      )+"}\n\n"+
      "void my_free() {\n"+ind("cuFree(g_in1);"+(if(twotracks) " cuFree(g_in2);" else "")+(if (useRna)" rna_free();" else "")+"\n"+
      "cuFree2(c_cost,g_cost); cuFree2(c_back,g_back); cuReset;")+"}\n\n"

    // Wrapper to run the matrix computation
    val steps = if (!twotracks&&window>0) ""+(window+1) else "(M_W"+(if(twotracks) "+M_H" else "")+")"
    val solve = "void my_solve() {\n"+
    "  #define WARP_SIZE 32 // constant over CUDA devices\n"+
    "  unsigned blk_size = WARP_SIZE;\n"+
    "  unsigned blk_num = (M_H+blk_size-1)/blk_size;\n"+
    "  unsigned* lock; cuMalloc(lock,sizeof(unsigned)*blk_num);\n"+
    "  cuErr(cudaMemset(lock,0,sizeof(unsigned)*blk_num));\n"+
    "  cuStream(stream);\n"+
    "  for (int i=0;i<{SPLITS};++i) {\n"+
    "    unsigned s0=("+steps+"*i)/{SPLITS}, s1=("+steps+"*(i+1))/{SPLITS};\n"+
    "    gpu_solve<<<blk_num, blk_size, 0, stream>>>(g_in1, g_in2, g_cost, g_back, lock, s0, s1);\n"+
    "  }\n"+
    "  cuSync(stream); cuStreamDestroy(stream); cuFree(lock);\n}\n\n"

    // Extra function to compute the best result within the window
    val aggr=h.toString match {
      case "$$count$$"|"$$sum$$" => "true" case "$$max$$" => "c>(*res)" case "$$min$$" => "c<(*res)"
      case "$$maxBy$$" => val f=genFun(h.asInstanceOf[MaxBy[Any,Any]].f); f+"(c)>"+f+"(*res)"
      case "$$minBy$$" => val f=genFun(h.asInstanceOf[MinBy[Any,Any]].f); f+"(c)<"+f+"(*res)"
      case _ => sys.error("Unsupported aggregation: "+h)
    }
    val winbest = if (window==0) "" else
      "__global__ void gpu_window(cost_t* cost, back_t* back, unsigned* pos, "+tpAnswer+"* res) {\n  bool valid=false;\n"+
      "  for (unsigned i=0,j="+window+"; j<M_W; ++i, ++j) "+(if (axiom.alwaysValid)"" else "if (back[idx(i,j)]."+axiom.name+".rule!=-1) ")+"{\n"+
      "    "+tpAnswer+" c = cost[idx(i,j)]."+axiom.name+";\n    if (!valid || "+aggr+") { "+
      "*res"+(h.toString match {case "$$count$$"=>"+=1" case "$$sum$$"=>"+=c" case _=>"=c"})+"; pos[0]=i; pos[1]=j; valid=true; }\n  }\n}\n\n"

    // Backtracking wrapper: read result (get best within window) and produce backward trace from there
    val backtrack = tpAnswer+" my_backtrack(trace_t** trace, unsigned* size) {\n"+
    "  "+tpAnswer+" res; unsigned i0="+(if(twotracks) "M_H-1" else "0")+", j0=M_W-1;\n"+
    (if (window>0)
      "  unsigned* g_pos; cuMalloc(g_pos,sizeof(unsigned)*2);\n"+
      "  "+tpAnswer+"* g_res; cuMalloc(g_res,sizeof("+tpAnswer+"));\n"+
      "  gpu_window<<<1,1,0,NULL>>>(g_cost, g_back, g_pos, g_res);\n"+
      "  cuGet(&res,g_res,sizeof("+tpAnswer+"),NULL); cuFree(g_res);\n"+
      "  unsigned pos[2]; cuGet(pos,g_pos,sizeof(unsigned)*2,NULL); cuFree(g_pos); i0=pos[0]; j0=pos[1];\n"
    else "  cuGet(&res,&g_cost[idx(i0,j0)]."+axiom.name+",sizeof("+tpAnswer+"),NULL);\n")+
    "  if (trace && size) {\n"+
    "    unsigned mem=(M_W+M_H)*sizeof(trace_t);\n"+
    "    trace_t *g_trace=NULL,*c_trace=NULL; cuAlloc2(traceDev,c_trace,g_trace,mem);\n"+
    "    unsigned *g_size=NULL; cuMalloc(g_size,sizeof(unsigned));\n"+
    "    gpu_backtrack<<<1,1,0,NULL>>>(g_trace, g_size, g_back, i0, j0);\n"+
    "    cuGet(size,g_size,sizeof(unsigned),NULL); cuFree(g_size); mem=(*size)*sizeof(trace_t);\n"+
    "    *trace=(trace_t*)malloc(mem); cuGet(*trace,g_trace,mem,NULL); cuFree2(c_trace,g_trace);\n"+
    "  }\n"+
    "  return res;\n}\n"

    winbest+init+solve+backtrack
  }

  // --------------------------------------------------------------------------
  // JNI transfers

  def genJNI = {
    def ctime(b:String,n:String) = if (!benchmark) b else "  gettimeofday(&ts,NULL);\n"+b+"  gettimeofday(&te,NULL);\n"+
      "  delta=(te.tv_sec-ts.tv_sec)*1000.0+(te.tv_usec-ts.tv_usec)/1000.0;\n  printf(\"%-20s : %.3f sec\\n\",\""+n+"\",delta/1000.0);\n"
    val jtp = head.jniTp(tpAlphabet)
    val call = "JNIEXPORT jobject JNICALL Java_{className}_"
    val parm = "(JNIEnv* env, jobject obj, "+jtp+"Array input1"+(if (twotracks) ", "+jtp+"Array input2" else "")+")"
    val flush = if (benchmark) "  fflush(stdout); fflush(stderr);\n" else ""
    val solve = (if(benchmark)"  struct timeval ts,te; double delta;\n" else"")+ctime("  input_t *in1=NULL; jni_read(env,input1,&in1);\n"+
      (if (twotracks) "  input_t *in2=NULL; jni_read(env,input2,&in2);\n  my_init(in1,in2); free(in2);"
                 else "  my_init(in1,NULL);"),"- JNI read")+"  free(in1);\n"+
    ctime("  my_solve();\n","- Compute")

    "#include <jni.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <sys/time.h>\n#include \"{file}.h\"\n#ifdef __cplusplus\n"+
    "extern \"C\" {\n#endif\n"+call+"parse"+parm+";\n"+call+"backtrack"+parm+";\n#ifdef __cplusplus\n}\n#endif\n\n"+
    head.jniRead(tpAlphabet)+"\n"+head.jniWrite(tpAnswer,catMax>0)+"\n"+
    "jobject Java_{className}_parse"+parm+" {\n"+solve+
    "  "+tpAnswer+" score=my_backtrack(NULL,NULL);\n"+
    "  jobject result = jni_write(env, score, NULL, 0);\n"+flush+
    "  my_free(); return result;\n}\n\n"+
    "jobject Java_{className}_backtrack"+parm+" {\n"+solve+
    "  trace_t *trace=NULL; unsigned size=0;\n"+
    ctime("  "+tpAnswer+" score=my_backtrack(&trace,&size);\n","- Backtrack")+
    ctime("  jobject result = jni_write(env, score, trace, size); free(trace);\n","- JNI output")+flush+
    "  my_free(); return result;\n}\n"
  }

  // --------------------------------------------------------------------------
  // RNA folding specific additions

  private def useRna = this match { case s:RNASignature => s.energies case _ => false }
  private def rnaPath = "../src/librna/" // src/librna/ folder relatively to compile folder (bin/)

  def genRNA = this match {
    // XXX: fix path automatically
    // XXX: add way to link pre-existing object to spare compilation phase
    case s:RNASignature if (s.energies) => val pf=s.paramsFile
      import librna.{LibRNA=>l}
      if (pf!=null) l.setParams(pf)
      val consts = l.getConsts
      "// --------------------------------\n"+
      "#include \""+rnaPath+"vienna/vienna.h\"\n"+
      "__constant__ paramT0 param0;\n"+
      consts+ // XXX: move this inside CodeGen for correctness, also this is an input parameter like size RNASignature(params) ??
      "#define my_len (M_H-1)\n"+
      "#define my_seq _in1\n"+
      "#define my_P g_P\n"+
      "#define my_P0 param0\n"+
      "#define my_dev __device__\n"+
      "#include \""+rnaPath+"librna_impl.h\"\n"+
      "#include \""+rnaPath+"vienna/vienna.c\"\n"+
      "static paramT *cg_P=NULL;\n"+
      "__global__ static void _initP(paramT* params) { g_P=params; }\n"+
      "static inline void rna_init() {\n"+
      (if (pf!=null) "  read_parameter_file(\""+pf+"\");\n" else "")+
      "  paramT* P = get_scaled_parameters();\n"+
      "  cudaMemcpyToSymbol(param0,&(P->p0),sizeof(paramT0));"+
      "  cuMalloc(cg_P,sizeof(paramT));\n"+
      "  cuPut(P,cg_P,sizeof(paramT),NULL);\n"+
      "  _initP<<<1,1>>>(cg_P); free(P);\n"+
      "}\n"+
      "static inline void rna_free() { cuFree(cg_P); }\n"+
      "// --------------------------------\n"
    case _ => ""
  }

  // --------------------------------------------------------------------------
  // C code generation and execution

  lazy val (code_h,code_cu,code_c,code_cpx) = {
    time("Analysis"){()=> analyze }
    time("Code generation"){()=>
      // Major structures header
      head.add("#define input_t "+tpAlphabet)
      head.add("typedef struct { "+rulesOrder.map{n=>tpAnswer+" "+n+";"}.mkString(" ")+" } cost_t;")
      head.add("typedef struct { "+rulesOrder.map{n=>val c=rules(n).inner.cat; btTpe(c)+" "+n+";"}.mkString(" ")+" } back_t;")
      // Time complexity of computations: O(n^( max(#unbounded concatenations) + matrix_dimensions=2 ))
      def ucc[T](p0:Parser[T]):Int = p0 match {
        case cc@Concat(l,r,_) => Math.max(ucc(l),ucc(r))+(if (l.max==maxN && r.max==maxN) 1 else 0)
        case Aggregate(p,_)=>ucc(p) case Filter(p,_)=>ucc(p) case Map(p,_)=>ucc(p) case Or(l,r)=>Math.max(ucc(l),ucc(r)) case _=>0
      }
      val cpx = rules.map{case(_,t)=>ucc(t.inner)}.max + 2
      // Code generation informations
      val info="// Type: "+(if (twotracks) "sequence alignment" else "sequence parser" )+(if (window>0) ", window="+window else "")+", complexity=O(n^"+cpx+"), splits={SPLITS}\n"+
        rulesOrder.zipWithIndex.map { case(n,i)=> val p=rules(n); "// Rule #%-2d %-8s : id=%-2d alt=%-2d cat=%-2d min=%-2d max=%-2d\n".format(i,"'"+n+"'",p.id,p.inner.alt,p.inner.cat,p.min,p.max) }.mkString
      // Generate modules code
      val kern=genKern(true); val bt=genBT(true); var hlp=genHelpers(); var jni=genJNI; var rna=genRNA; var (hpub,hpriv,hfun)=head.flush
      (info+hpub, hpriv+"\n"+rna+"\n"+hfun+"\n"+kern+"\n"+bt+"\n"+hlp, jni, cpx)
    }
  }

  // --------------------------------------------------------------------------
  // CPU-specific code generation (might need additional cleanup)
  // XXX: RNA functions currently not supported

  lazy val code_cc = {
    code_h; // make sure this happen after other codegen phases
    val kern=genKern(false); val bt=genBT(false);

    head.addPriv("#define _unroll _Pragma(\"unroll "+(if(useRna) 1 else cudaUnroll)+"\")") // experimental optimal
    head.addPriv("#define M_W {MAT_WIDTH}")
    head.addPriv("#define M_H {MAT_HEIGHT}")
    if (twotracks) {
      head.addPriv("#define MEM_MATRIX (M_W*M_H)") // (M_W*M_H)
      head.addPriv("#define idx(i,j) ((i)*M_W+j)") // ((i)*B_W+(j))
    } else {
      head.addPriv("#define MEM_MATRIX (M_W*M_H)") // ((M_H*(M_H+1))/2) -- upper right triangle, including diagonal
      head.addPriv("#define idx(i,j) ((i)*M_W+j)") // ({ int _i=(i),_d=M_H+1+_i-(j); MEM_MATRIX - ((_d*(_d-1))>>1) +_i; })
    }
    // Extra function to compute the best result within the window
    val aggr=h.toString match {
      case "$$count$$"|"$$sum$$" => "true" case "$$max$$" => "c>res" case "$$min$$" => "c<res"
      case "$$maxBy$$" => val f=genFun(h.asInstanceOf[MaxBy[Any,Any]].f); f+"(c)>"+f+"(res)"
      case "$$minBy$$" => val f=genFun(h.asInstanceOf[MinBy[Any,Any]].f); f+"(c)<"+f+"(res)"
      case _ => sys.error("Unsupported aggregation: "+h)
    }

    head.addPriv("#include <string.h>") // for memcpy
    head.addPriv("static input_t *_in1=NULL, *_in2=NULL;\nstatic cost_t* c_cost=NULL;\nstatic back_t* c_back=NULL;")
    // RNA setup
    val rna = this match {
      case s:RNASignature if (s.energies) => val p=s.paramsFile; if (p!=null) librna.LibRNA.setParams(p)
        head.addPriv("#define my_len (M_H-1)\n#define my_seq _in1\n#define my_P c_P\n#define my_dev")
        List("vienna/vienna.h","librna_impl.h","vienna/vienna.c").foreach{x=>head.addPriv("#include \""+rnaPath+x+"\"")}
        (if (p!=null) "  read_parameter_file(\""+p+"\");\n" else "")+"  c_P = get_scaled_parameters();\n"
      case _ => ""
    }
    // Backtracking wrapper: read result (get best within window) and produce backward trace from there
    val wrap = "void my_init(input_t* in1, input_t* in2) {\n"+
               "  size_t sz1=(M_H-1)*sizeof(input_t); _in1=(input_t*)malloc(sz1); memcpy(_in1,in1,sz1); \n"+
               "  if (in2) { size_t sz2=(M_W-1)*sizeof(input_t); _in2=(input_t*)malloc(sz2); memcpy(_in2,in2,sz2); }\n"+
               "  c_cost=(cost_t*)malloc(sizeof(cost_t)*MEM_MATRIX);\n  c_back=(back_t*)malloc(sizeof(back_t)*MEM_MATRIX);\n"+rna+"}\n\n"+
               "void my_solve() { cpu_solve(_in1, _in2, c_cost, c_back); }\nvoid my_free() { free(_in1); free(_in2); free(c_cost); free(c_back);"+(if (useRna) " free(c_P);" else "")+" }\n"+
    tpAnswer+" my_backtrack(trace_t** trace, unsigned* size) {\n"+
    "  "+tpAnswer+" res; unsigned i0="+(if(twotracks) "M_H-1" else "0")+", j0=M_W-1;\n"+
    (if (window>0) "  bool valid=false;\n"+
      "  for (unsigned i=0,j="+window+"; j<M_W; ++i, ++j) "+(if (axiom.alwaysValid)"" else "if (back[idx(i,j)]."+axiom.name+".rule!=-1) ")+"{\n"+
      "    "+tpAnswer+" c = c_cost[idx(i,j)]."+axiom.name+";\n    if (!valid || "+aggr+") { "+
      "res"+(h.toString match {case "$$count$$"=>"+=1" case "$$sum$$"=>"+=c" case _=>"=c"})+"; i0=i; j0=j; valid=true; }\n  }\n"
    else "  res = c_cost[idx(i0,j0)]."+axiom.name+";\n")+
    "  if (trace && size) { *trace=(trace_t*)malloc((M_W+M_H)*sizeof(trace_t)); cpu_backtrack(*trace,size,c_back,i0,j0); }\n"+
    "  return res;\n}\n"

    val (_,hpriv,hfun)=head.flush
    hpriv+"\n"+hfun.replace("__device__ ","")+"\n"+kern+"\n"+bt+"\n"+wrap+"\n"
  }

  // --------------------------------------------------------------------------
  // Configuration

  val cudaSplit = 1024 // threshold for multiple CUDA kernels
  val cudaDevice = -1  // preferred execution CUDA device
  val cudaUnroll = 5 // experimental unrolling optimal
  val cudaSharedInput = this match { case s:RNASignature => true case _ => false } // store input in shared memory
  val cudaEmpty:String = null // "empty" Answer value (usually zero or infinite). By setting this value, aggregation must be done _ONLY_ on Answer type
  val compiler = new CodeCompiler {
    override val outPath = "bin"
    override val cudaPath = "/usr/local/cuda"
    override val cudaFlags = "-m64 -arch=sm_30 --disable-warnings" // --ptxas-options=-v
    override val ccFlags = "-O3 "+{ val home=System.getenv("JAVA_HOME");
      if (home!=null) "-I"+home+"/include -I"+home+"/include/linux" // for Linux    XXX: relate to System.getProperty("os.name")
      else "-I/System/Library/Frameworks/JavaVM.framework/Headers"  // for MacOS    (path to jni.h and jni_md.h, from the JDK)
    }
    override val ldFlags = "-shared "+{ // disable CUDA library linking if not found
      def dir(path:String,fail:String) = if (new java.io.File(path).isDirectory()) path else fail
      val lib=dir(cudaPath+"/lib64",dir(cudaPath+"/lib",null)); if (lib!=null) "-L"+lib+" -lcudart -Wl,-rpath,"+lib else ""
    }
  }

  // --------------------------------------------------------------------------
  // Memoize reference to the generated code such that we compile only once for a particular problem size
  // Unfortunately this does not seems to help much the CUDA code to 'warm-up'
  abstract class CodeCompiler extends CCompiler with ScalaCompiler {
    import scala.collection.mutable.HashMap
    private val adp = new HashMap[(Int,Boolean),ADPWrapper[Alphabet,Answer]]
    private val tt = new HashMap[(Int,Int,Boolean),TTWrapper[Alphabet,Answer]]
    def getADP(size1:Int,gpu:Boolean):ADPWrapper[Alphabet,Answer] = adp.getOrElseUpdate((size1,gpu),genDP[ADPWrapper[Alphabet,Answer]](size1,size1,gpu,false) )
    def getTT(size1:Int,size2:Int,gpu:Boolean):TTWrapper[Alphabet,Answer] = tt.getOrElseUpdate((size1,size2,gpu),genDP[TTWrapper[Alphabet,Answer]](size1,size2,gpu,true) )

    def genDP[T](size1:Int,size2:Int,gpu:Boolean,tt:Boolean)(implicit mT: scala.reflect.ClassTag[T]):T = {
      val className = "CompWrapper"+CompileCounter.get // fresh namespace for the problem (code+in1+in2)
      // Problems up to 'cudaSplit' can be solve within a single kernel, otherwise, divide running time in splits
      val splits:Int = Math.ceil(Math.pow(Math.max(size1,size2)*1.0/cudaSplit,code_cpx)).intValue
      if (benchmark) println("%-20s : %d x %d / %d".format("Size / splits",size1,size2,splits))

      val map = scala.collection.immutable.Map(("className",className),("MAT_HEIGHT",""+(size1+1)),("MAT_WIDTH",""+(size2+1)),("SPLITS",""+splits))
      if (gpu) time("C+CUDA compilation"){()=> add("h", code_h, map); add("c", code_c, map); add("cu", code_cu, map); gen }
      else time("C compilation"){()=> add("h", code_h, map); add("c", code_c+code_cc, map); gen }
      time("Scala compilation"){()=>
        val cl = (if(tt) classOf[TTWrapper[Alphabet,Answer]] else classOf[ADPWrapper[Alphabet,Answer]]).getCanonicalName
        val al = tpAlphabet match {case head.TPri(s,_,_)=>s case _=>"Any"}
        val args = "(in1:Array["+al+"]"+(if(tt)",in2:Array["+al+"]" else "")+")"
        val wrap = "class "+className+" extends "+cl+"["+al+",Any] {\n"+
          "@native def parse"+args+":Any\n@native def backtrack"+args+":(Any,List[((Int, Int),(Int,List[Int]))])\n}"
        compile[T](className,wrap)
      }
    }
  }

  // Wrappers for CUDA invocation, these are automatically called by ADPParsers/TTParsers
  def parseC(in1:Input,gpu:Boolean) = { val w=compiler.getADP(in1.size,gpu); time("Run"+(if(gpu)"CUDA"else"CPU")){()=> w.parse(in1) } }
  def backtrackC(in1:Input,gpu:Boolean) = { val w=compiler.getADP(in1.size,gpu); time("Run"+(if(gpu)"CUDA"else"CPU")+"+BT"){()=> w.backtrack(in1) } }
  def parseCTT(in1:Input,in2:Input,gpu:Boolean) = { val w=compiler.getTT(in1.size,in2.size,gpu); time("Run"+(if(gpu)"CUDA"else"CPU")){()=> w.parse(in1,in2) } }
  def backtrackCTT(in1:Input,in2:Input,gpu:Boolean) = { val w=compiler.getTT(in1.size,in2.size,gpu); time("Run"+(if(gpu)"CUDA"else"CPU")+"+BT"){()=> w.backtrack(in1,in2) } }

  // Debug
  def gen:String = {
    "----------- headers -----------\n"+code_h+
    "------------- gpu -------------\n"+code_cu+
    "------------- cpu -------------\n"+code_cc+
    "------------- jni -------------\n"+code_c+
    "------------- end -------------\n"
  }
}
