package v4

trait TTParsers extends BaseParsers { this:Signature =>
  // Two-tracks combinators. Subword := (end_in1, end_in2)
  // Main difference with ADP is the concat operators and use of 2 inputs.

  // I/O interface
  override val twotracks = true
  private var input1: Input = null
  private var input2: Input = null
  def in1(k:Int):Alphabet = input1(k)
  def in2(k:Int):Alphabet = input2(k)
  def size1:Int = input1.size
  def size2:Int = input2.size
  def parse(in1:Input,in2:Input,ps:ParserStyle=psCUDA):List[Answer] = this match {
    case c:CodeGen if (ps==psCPU || ps==psCUDA) => List(c.parseCTT(in1.asInstanceOf[c.Input],in2.asInstanceOf[c.Input],ps==psCUDA).asInstanceOf[Answer])
    case _ => run(if (ps==psBottomUp)"ScalaBotUp" else "ScalaTopDown",in1,in2,()=>{ if (ps==psBottomUp) parseBottomUp; axiom(size1,size2).map(_._1)})
  }
  def backtrack(in1:Input,in2:Input,ps:ParserStyle=psCUDA):List[(Answer,Trace)] =  this match {
    case c:CodeGen if (ps==psCPU || ps==psCUDA) => List(c.backtrackCTT(in1.asInstanceOf[c.Input],in2.asInstanceOf[c.Input],ps==psCUDA).asInstanceOf[(Answer,Trace)])
    case _ => run(if (ps==psBottomUp)"ScalaBotUp+BT" else "ScalaTopDown+BT",in1,in2,()=>{ if (ps==psBottomUp) parseBottomUp; axiom.backtrack(size1,size2)})
  }
  def build(in1:Input,in2:Input,bt:Trace):Answer = run("Build",in1,in2,()=>axiom.build(bt))
  protected def run[T](name:String,in1:Input,in2:Input, f:()=>T) = { input1=in1; input2=in2; analyze; tabInit(size1+1,size2+1); val res=time("Run"+name)(f); tabReset; input1=null; input2=null; res }
  private def parseBottomUp { val rs=rulesOrder map {n=>rules(n)}; var i=0; while (i<=size1) { var j=0; while (j<=size2) { for (r<-rs) r((i,j)); j=j+1; }; i=i+1; } }

  // Concat parsers
  import scala.language.implicitConversions
  implicit def parserTT[T](p1:Parser[T]) = new ParserTT(p1)
  class ParserTT[T](p1:Parser[T]) {
    def -~ [U](p2:Parser[U]) = new Concat(p1,p2,1)
    def ~- [U](p2:Parser[U]) = new Concat(p1,p2,2)
  }

  // Terminal parsers
  val empty = new Terminal[Unit](0,0,(i:Var,j:Var) => (List(zero.eq(i,0),zero.eq(j,0)),"")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i==0 && j==0) List(({},bt0)) else Nil }
  }
  val empty1 = new Terminal[Int](0,maxN,(i:Var,j:Var) => (List(zero.eq(i,0)),"("+j+")")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i==0) List((j,bt0)) else Nil }
  }
  val empty2 = new Terminal[Int](0,maxN,(i:Var,j:Var) => (List(zero.eq(j,0)),"("+i+")")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(j==0) List((i,bt0)) else Nil }
  }
  val el1 = new Terminal[Alphabet](1,1,(i:Var,j:Var) => (Nil,"_in1["+i+"]")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i+1==j) List((in1(i),bt0)) else Nil }
  }
  val el2 = new Terminal[Alphabet](1,1,(i:Var,j:Var) => (Nil,"_in2["+i+"]")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i+1==j) List((in2(i),bt0)) else Nil }
  }
  val eli = new Terminal[Int](1,1,(i:Var,j:Var) => (Nil,"("+i+")")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i+1==j) List((i,bt0)) else Nil }
  }
  def seq(min:Int=1,max:Int=maxN) = new Terminal[Subword](min,max,(i:Var,j:Var) => (Nil,"(T2ii){"+i+","+j+"}")) { // in{1,2}[i]..in{1,2}[j-1]
    def apply(sw:Subword) = { val (i,j)=sw; if (i+min<=j && (max==maxN || i+max>=j)) List(((i,j),bt0)) else Nil }
  }

  // Extra niceties
  import scala.language.implicitConversions
  implicit def detupleTT3[A,B,C,D,R](fn:Function3[A,B,C,R]) = new (((A,(B,C)))=>R) with DeTuple { override val f=fn
    def apply(t:(A,(B,C))) = { val (a,(b,c))=t; fn(a,b,c) } }
  implicit def detupleTT4l[A,B,C,D,R](fn:Function4[A,B,C,D,R]) = new ((  ((A,(B,C)),D)  )=>R) with DeTuple { override val f=fn
    def apply(t:((A,(B,C)),D)) = { val ((a,(b,c)),d)=t; fn(a,b,c,d) } }
  implicit def detupleTT4r[A,B,C,D,R](fn:Function4[A,B,C,D,R]) = new ((  (A,(B,(C,D)))  )=>R) with DeTuple { override val f=fn
    def apply(t:(A,(B,(C,D)))) = { val (a,(b,(c,d)))=t; fn(a,b,c,d) } }
  implicit def detupleTT5[A,B,C,D,E,R](fn:Function5[A,B,C,D,E,R]) = new ((  (A,((B,(C,D)),E))  )=>R) with DeTuple { override val f=fn
    def apply(t:(A,((B,(C,D)),E))) = { val (a,((b,(c,d)),e))=t; fn(a,b,c,d,e) } }
  implicit def detupleTT5l[A,B,C,D,E,R](fn:Function5[A,B,C,D,E,R]) = new ((  (((A,(B,C)),D),E)  )=>R) with DeTuple { override val f=fn
    def apply(t:(((A,(B,C)),D),E)) = { val (((a,(b,c)),d),e)=t; fn(a,b,c,d,e) } }
  implicit def detupleTT5r[A,B,C,D,E,R](fn:Function5[A,B,C,D,E,R]) = new ((  (A,(B,(C,(D,E))))  )=>R) with DeTuple { override val f=fn
    def apply(t:(A,(B,(C,(D,E))))) = { val (a,(b,(c,(d,e))))=t; fn(a,b,c,d,e) } }

  private val myself=this
  implicit def swapTT[A,B,R](fn:Function2[A,B,R]) = new (((B,A))=>R) with CFun {
    def apply(t:(B,A)) = { val (b,a)=t; fn(a,b) }
    val (body,args,tpe) = fn match {
      case f:CFun=>(f.body,f.args.reverse,f.tpe)
      case _=> if (myself.isInstanceOf[CodeGen]) sys.error("Cannot swap a plain function"); ("",Nil,"")
    }
  }
}
