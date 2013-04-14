package v4

trait Signature {
  type Alphabet // input type
  type Answer // output type

  val window = 0 // windowing size, 0=disabled
  val h:List[Answer]=>List[Answer] = l=>l // optimization function

  // Predefined CUDA-compatible aggregation functions
  def max[T:Numeric]: List[T]=>List[T]
  def min[T:Numeric]: List[T]=>List[T]
  def sum[T:Numeric]: List[T]=>List[T]
  def count[T:Numeric]: List[T]=>List[T]
  def maxBy[T,U:Numeric](f:T=>U): List[T]=>List[T]
  def minBy[T,U:Numeric](f:T=>U): List[T]=>List[T]

  // helpers to manually write CFun
  def cfun1[A,R](fn:A=>R,as:String,bdy:String)(implicit mA:Manifest[A],mR:Manifest[R]) =
    new Function1[A,R] with CFun { val (args,body,tpe)=((if(as!="") List(as) else Nil) zip List(mA).map{_.toString},bdy,mR.toString); def apply(a:A) = fn(a) }
  def cfun2[A,B,R](fn:(A,B)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mR:Manifest[R]) =
    new Function2[A,B,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB).map{_.toString},bdy,mR.toString); def apply(a:A,b:B) = fn(a,b) }
  def cfun3[A,B,C,R](fn:(A,B,C)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mC:Manifest[C],mR:Manifest[R]) =
    new Function3[A,B,C,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB,mC).map{_.toString},bdy,mR.toString); def apply(a:A,b:B,c:C) = fn(a,b,c) }
  def cfun4[A,B,C,D,R](fn:(A,B,C,D)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mC:Manifest[C],mD:Manifest[D],mR:Manifest[R]) =
    new Function4[A,B,C,D,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB,mC,mD).map{_.toString},bdy,mR.toString); def apply(a:A,b:B,c:C,d:D) = fn(a,b,c,d) }
  def cfun5[A,B,C,D,E,R](fn:(A,B,C,D,E)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mC:Manifest[C],mD:Manifest[D],mE:Manifest[E],mR:Manifest[R]) =
    new Function5[A,B,C,D,E,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB,mC,mD,mE).map{_.toString},bdy,mR.toString); def apply(a:A,b:B,c:C,d:D,e:E) = fn(a,b,c,d,e) }
  def cfun6[A,B,C,D,E,F,R](fn:(A,B,C,D,E,F)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mC:Manifest[C],mD:Manifest[D],mE:Manifest[E],mF:Manifest[F],mR:Manifest[R]) =
    new Function6[A,B,C,D,E,F,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB,mC,mD,mE,mF).map{_.toString},bdy,mR.toString); def apply(a:A,b:B,c:C,d:D,e:E,f:F) = fn(a,b,c,d,e,f) }
  def cfun7[A,B,C,D,E,F,G,R](fn:(A,B,C,D,E,F,G)=>R,as:String,bdy:String)(implicit mA:Manifest[A],mB:Manifest[B],mC:Manifest[C],mD:Manifest[D],mE:Manifest[E],mF:Manifest[F],mG:Manifest[G],mR:Manifest[R]) =
    new Function7[A,B,C,D,E,F,G,R] with CFun { val (args,body,tpe)=(as.split(",").toList zip List(mA,mB,mC,mD,mE,mF,mG).map{_.toString},bdy,mR.toString); def apply(a:A,b:B,c:C,d:D,e:E,f:F,g:G) = fn(a,b,c,d,e,f,g) }

  // Warning about JNI/SBT embedded JVM issues (multiple loading failure, unable to properly load generated code)
  protected def reqJNI { try { this.getClass.getClassLoader.getClass.getDeclaredMethod("addURL", classOf[java.net.URL]); }
    catch { case t:Throwable => println("\n"+
            "+----------------------------------------------------------------------------------+\n"+
            "| WARNING: Using SBT embedded JVM, JNI will fail (java.lang.UnsatisfiedLinkError). |\n"+
            "| See build.sbt tasks to launch a separate JVM.                                    |\n"+
            "+----------------------------------------------------------------------------------+\n\n") }
  }
}

trait BaseParsers { this:Signature =>
  sealed abstract class ParserStyle
  case object psTopDown extends ParserStyle
  case object psBottomUp extends ParserStyle
  case object psCPU extends ParserStyle
  case object psCUDA extends ParserStyle
  case object psScalaLMS extends ParserStyle

  type Input = Array[Alphabet]
  type Subword = (Int, Int)
  type Backtrack = (Int,List[Int]) // (subrule_id, indices)
  type BTItem = (Subword,Answer,Backtrack)
  type Trace = List[(Subword,Backtrack)]

  val axiom:Tabulate // initial parser to be applied
  val twotracks = false // whether grammar is multi-track
  final val bt0 = (0,Nil) // default initial backtrack

  // Benchmarking
  val benchmark = false // enable benchmarking counters
  def time[T](s:String)(u:()=>T):T = { val start=System.currentTimeMillis; val r=u()
    if (benchmark) { val d=System.currentTimeMillis-start; println("%-20s : %d.%03d sec".format(s,d/1000,d%1000)) }; r
  }

  // Helpers for code generation
  case class Var(v:Char,d:Int) { // Variables: name+offset => 'v'+d
    override def toString = if (v=='0') ""+d else if (d==0) ""+v else if (d>0) v+"+"+d else v+""+d
    def add(e:Int) = Var(v,d+e)
    def loop(l:Var,u:Var) = CFor(v,l.v,d-l.d,u.v,d-u.d)
    def leq(t:Var,e:Int) = CLeq(v,t.v,d-t.d+e)
    def eq(t:Var,e:Int) = CEq(v,t.v,d-t.d+e)
  }
  final val zero = Var('0',0) // 0+0
  final val maxN = -1 // infinity for Parser.max

  sealed abstract class Cond // Constraint on variables
  case class CFor(v:Char,l:Char,ld:Int,u:Char,ud:Int, lD:Int=maxN,uD:Int=maxN) extends Cond
    // for ('l'+ld<='v'<='u'-ud) / for ('v' = MAX('l'+ld;'u'-uD), up = MIN('u'-ud,'l'+lD); 'v'<=up; ++'v'))
  case class CLeq(a:Char,b:Char,delta:Int) extends Cond // 'a'+delta<='b'
  case class CEq(a:Char,b:Char,delta:Int) extends Cond // 'a'+delta=='b'
  case class CUser(cond:String) extends Cond // user condition

  // Abstract parser
  sealed abstract class Parser[T] extends (Subword => List[(T,Backtrack)]) {
    def min:Int // subword minimal size
    def max:Int // subword maximal size, -1=infinity
    val alt:Int // alternative (subrule_id)
    val cat:Int // concatenation split (offset in backtrack)

    def apply(sw:Subword): List[(T,Backtrack)]
    def unapply(sw:Subword,bt:Backtrack): List[(T, List[BTItem])]
    def reapply(sw:Subword,bt:Backtrack): T

    final def ^^[U](f: T => U) = new Map(this,f)
    final def |(other: Parser[T]) = new Or(this,other)
    final def aggregate(h: List[T] => List[T]) = new Aggregate(this,h)
    final def filter (f: Subword => Boolean) = new Filter(this,f)
  }

  // Recurrence analysis, done once when grammar is complete, before the computation.
  private var analyzed=false
  def analyze:Boolean = { if (analyzed) return false; analyzed=true
    // Strip away unnecessary tabulations
    var used = Set[String](); use(axiom)
    def use[T](q:Parser[T]):Unit = q match {
      case Aggregate(p,_) => use(p) case Filter(p,_) => use(p) case Map(p,_) => use(p) case Or(l,r) => use(l); use(r) case Concat(l,r,_) => use(l); use(r)
      case t:Tabulate if (!used.contains(t.name)) => used++=Set(t.name); use(t.inner) case _ =>
    }
    for (n <- (rules.keys.toSet -- used)) rules.remove(n)
    // Yield analysis
    for((n,t)<-rules) t.minv=100000 // upper bound on minimum yields
    (0 until rules.size).foreach{ _ => for((n,t) <- rules) t.minv=t.inner.min }
    for((n,t)<-rules) t.maxv=t.minv
    for((n,t)<-rules) t.maxv=rmax(t.inner, rules.size)
    def rmax[T](p0:Parser[T],d:Int):Int = p0 match {
      case Terminal(_,max,_) => max
      case Filter(p,f) => rmax(p,d)
      case Aggregate(p,h) => rmax(p,d)
      case Map(p,f) => rmax(p,d)
      case Or(l,r) => val ml=rmax(l,d); if (ml==maxN) maxN else { val mr=rmax(r,d); if (mr==maxN) maxN else Math.max(ml,mr) }
      case Concat(l,r,_) => val ml=rmax(l,d); if (ml==maxN) maxN else { val mr=rmax(r,d); if (mr==maxN) maxN else ml+mr }
      case p:Tabulate => if (d==1) maxN else rmax(p.inner,d-1)
    }
    // Dependency analyis order: prevents infinite loops, also define an order for bottom up implementations (requires yield analysis)
    def deps[T](q:Parser[T]): List[String] = q match { // (A->B) <=> B(i,j) = ... | A(i,j)
      case Aggregate(p,_) => deps(p) case Filter(p,_) => deps(p) case Map(p,_) => deps(p) case Or(l,r) => deps(l)++deps(r)
      case cc@Concat(l,r,_) => val(lm,_,rm,_)=cc.indices; (if (lm==0) deps(r) else Nil) ::: (if (rm==0) deps(l) else Nil)
      case t:Tabulate => List(t.name) case _ => Nil
    }
    val cs = rules.map{case (n,p)=>(n,deps(p.inner)) }
    while (!cs.isEmpty) { var rem=false;
      cs.foreach { case (n,ds) if (ds.isEmpty||(true/:ds.map{d=>rulesOrder.contains(d)}){case(a,b)=>a&&b}) => rem=true; rulesOrder=n::rulesOrder; cs.remove(n); case _ => }
      if (rem==false) sys.error("Loop between tabulations, error in grammar")
    }
    rulesOrder = rulesOrder.reverse
    // Identify subrules (uniquely within the same grammar as sorted by name)
    var id=0; for((n,p) <- rules.toList.sortBy(_._1)) { p.id=id; id=id+p.inner.alt; }
    true
  }

  // Aggregate on T a (T,U) list, wrt to multiplicity and order
  def aggr[T,U](l:List[(T,U)], h: List[T] => List[T]):List[(T,U)] = {
    if (l.size==0) return Nil; val hs=h(l.map(_._1))
    if (hs.size==1) l.find(_._1==hs.head) match { case Some(x) => List(x) case None => List((hs.head,l.head._2)) } // optimization
    val a=l.toArray; var start=0;
    hs.map { b => val i=a.indexWhere({x=>x._1==b},start);
      if (i== -1) (b,l.head._2) // aggregators such as count, sum do not have matching backtrack
      else { val r=a(i); a(i)=a(start); start=start+1; r }
    }
  }

  // --------------------------------------------------------------------------
  // Memoization through tabulation
  import scala.collection.mutable.HashMap
  var rulesOrder:List[String]=Nil // Order of tabulations evaluation
  val rules = new HashMap[String,Tabulate]
  def tabInit(w:Int,h:Int) = rules.foreach{ case (_,t) => t.init(w,h) }
  def tabReset = rules.foreach{ case (_,t) => t.reset }

  def tabulate(name:String, inner: => Parser[Answer], alwaysValid:Boolean=false) = new Tabulate(inner,name,alwaysValid)
  class Tabulate(in: => Parser[Answer], val name:String, val alwaysValid:Boolean=false) extends Parser[Answer] {
    lazy val inner = in
    val (alt,cat) = (1,0)
    def min = minv; var minv = 0
    def max = maxv; var maxv = 0

    // Matrix storage
    /*private*/ var data:Array[List[(Answer,Backtrack)]] = null
    private var (mW,mH) = (0,0)
    def init(w:Int,h:Int) { mW=w; mH=h; val sz=if (twotracks) w*h else { assert(w==h); h*(h+1)/2 }; data=new Array(sz); }
    def reset { data=null; mW=0; mH=0; }

    if (rules.contains(name)) sys.error("Duplicate tabulation name")
    rules += ((name,this))

    var id:Int = -1 // subrules base index
    @inline private def idx(sw:Subword):Int = if (twotracks) sw._1*mW+sw._2 else { val d=mH+1+sw._1-sw._2; ( mH*(mH+1) - d*(d-1) ) /2 + sw._1 }
    private def get(sw:Subword) = { val i=idx(sw); val v1=data(i); if (v1!=null) v1 else { val v=inner(sw).map{case(c,(r,b))=>(c,(id+r,b))}; data(i)=v; v } }
    private def put(sw:Subword,v:List[(Answer,Backtrack)]) { data(idx(sw))=v; }

    def apply(sw: Subword) = get(sw) map {x=>(x._1,bt0)}
    def unapply(sw:Subword,bt:Backtrack) = get(sw) map { case (c,b) => (c,List((sw,c,b))) }
    def reapply(sw:Subword,bt:Backtrack) = { val v=data(idx(sw)); if (v!=null) v.head._1 else sys.error("Failed reapply"+sw) }

    def backtrack(sw:Subword) = countMap(get(sw), (e:(Answer,Backtrack),n:Int)=>backtrack0(n,Nil,List((sw,e._1,e._2))).map{x=>(e._1,x)} ).flatten
    def build(bt:Trace):Answer = bt match {
      case bh::bs => val (sw,(rule,b))=bh; val (t,rr)=findTab(rule); val a=t.inner.reapply(sw,(rr,b)); if (bs==Nil) a else { t.put(sw,List((a,bt0))); build(bs) }
      case Nil => sys.error("No backtrack provided")
    }

    private def countMap[T,U](ls:List[T],f:((T,Int)=>U)):List[U] = ls.groupBy(x=>x).map{ case(e,l)=>f(e,l.length) }.toList
    private def backtrack0(mult:Int,tail:Trace,pending:List[BTItem]):List[Trace] = pending match {
      case Nil => List(tail)
      case (sw,score,(rule,bt))::ps => val (t,rr) = findTab(rule)
        val res = t.inner.unapply(sw, (rr,bt)).filter{case(r,l)=>r==score}.map{case(r,l)=>l}.take(mult)
        countMap(res,(pl:List[BTItem],mul:Int)=>backtrack0(mul,(sw,(rule,bt))::tail, pl:::ps)).flatten
    }
  }

  def findTab(rule:Int):(Tabulate,Int) = {
    rules.find{ case (n,t)=> val rr=rule-t.id; rr >= 0 && rr < t.inner.alt} match {
      case Some((n,t)) => (t,rule-t.id)
      case None => sys.error("No tabulation for subrule #"+rule)
    }
  }

  // --------------------------------------------------------------------------
  // Terminal abstraction
  abstract case class Terminal[T](min:Int,max:Int, f:(Var,Var)=>(List[Cond],String)) extends Parser[T] {
    final val (alt,cat) = (1,0)
    def unapply(sw:Subword,bt:Backtrack) = apply(sw).map{ case(t,b) => (t,Nil) }
    def reapply(sw:Subword,bt:Backtrack) = {
      if (apply(sw).isEmpty) sys.error("Empty apply"+sw+" for "+bt)
      apply(sw).map{ _._1 }.head
    }
  }

  // Aggregate combinator.
  // Takes a function which modifies the list of a parse. Usually used
  // for max or min functions (but can also be a prettyprint).
  case class Aggregate[T](inner:Parser[T], h: List[T] => List[T]) extends Parser[T] {
    def min = inner.min
    def max = inner.max
    lazy val (alt,cat) = (inner.alt,inner.cat)
    def apply(sw:Subword) = aggr(inner(sw),h)
    def unapply(sw:Subword,bt:Backtrack) = aggr(inner.unapply(sw,bt),h)
    def reapply(sw:Subword,bt:Backtrack) = inner.reapply(sw,bt)
  }

  // Filter combinator.
  // Yields an empty list if the filter does not pass.
  case class Filter[T](inner:Parser[T], pred: Subword => Boolean) extends Parser[T] {
    def min = inner.min
    def max = inner.max
    lazy val (alt,cat) = (inner.alt,inner.cat)
    def apply(sw:Subword) = if(pred(sw)) inner(sw) else Nil
    def unapply(sw:Subword,bt:Backtrack) = inner.unapply(sw,bt) // filter matched at apply
    def reapply(sw:Subword,bt:Backtrack) = inner.reapply(sw,bt) // ditto
  }

  // Mapper. Equivalent of ADP's <<< operator.
  // To separate left and right hand side of a grammar rule
  case class Map[T,U](inner:Parser[T], f: T => U) extends Parser[U] {
    def min = inner.min
    def max = inner.max
    lazy val (alt,cat) = (inner.alt,inner.cat)
    def apply(sw:Subword) = inner(sw) map { case (s,b) => (f(s),b) }
    def unapply(sw:Subword,bt:Backtrack) = inner.unapply(sw,bt).map{ case (s,b) => (f(s),b) }
    def reapply(sw:Subword,bt:Backtrack) = f(inner.reapply(sw,bt))
  }

  // Or combinator. Equivalent of ADP's ||| operator.
  // In ADP semantics we concatenate the results of the parse
  // of 'this' with the parse of 'that'
  case class Or[T](left:Parser[T], right:Parser[T]) extends Parser[T] {
    def min = Math.min(left.min,right.min)
    def max = if (left.max==maxN || right.max==maxN) maxN else Math.max(left.max,right.max)
    lazy val (alt,cat) = (left.alt+right.alt, Math.max(left.cat,right.cat))
    def apply(sw: Subword) = left(sw) ++ right(sw).map{ case (t,(r,b)) => (t,(r+left.alt,b)) }
    def unapply(sw:Subword, bt:Backtrack) = { val (r,idx)=bt; var a=left.alt-1; if (r<=a) left.unapply(sw,(r,idx)) else right.unapply(sw,(r-a,idx)) }
    def reapply(sw:Subword, bt:Backtrack) = { val (r,idx)=bt; var a=left.alt-1; if (r<=a) left.reapply(sw,(r,idx)) else right.reapply(sw,(r-a,idx)) }
  }

  // Concatenate combinator.
  // Parses a concatenation of string " left ~ right " with length(left) in [lL,lU]
  // and length(right) in [rL,rU], lU,rU=0 means unbounded (infinity).
  case class Concat[T,U](left: Parser[T], right:Parser[U], track:Int) extends Parser[(T,U)] {
    lazy val (alt,cat) = (left.alt*right.alt, left.cat+(if(hasBt)1 else 0)+right.cat)
    lazy val hasBt:Boolean = (track,left.min,left.max,right.min,right.max) match {
      case (0,a,b,c,d) if ((a==b && a>=0) || (c==d && c>=0)) => false
      case (1,a,b,_,_) if (a==b && a>=0) => false
      case (2,_,_,a,b) if (a==b && a>=0) => false
      case _ => true
    }
    // Yield: we can only give restrictions hints, but analysis might restrict it further
    val hint = if (left==right && track==0) (1,maxN,1,maxN) else (0,maxN, 0,maxN)
    def min = Math.max(left.min,hint._1)+Math.max(right.min,hint._3)
    def max = { val lm=hm(left.max,hint._2); val rm=hm(right.max,hint._4); if (lm==maxN || rm==maxN) maxN else lm+rm }
    lazy val indices = { assert(analyzed==true); (Math.max(left.min,hint._1),hm(left.max,hint._2),Math.max(right.min,hint._3),hm(right.max,hint._4)) }
    private def hm(a:Int,b:Int) = if (a==maxN) b else if (b==maxN) a else Math.min(a,b) // min with maxN=infinity semantics

    @inline private def bt(bl:Backtrack,br:Backtrack,k:Int):Backtrack = {
      (bl._1*right.alt+br._1, bl._2:::(if (hasBt)List(k) else Nil):::br._2)
    }

    def apply(sw:Subword) = {
      val (i,j) = sw; val (lL,lU,rL,rU) = indices
      if (track==0) {
        val min_k = if (rU==maxN || i+lL > j-rU) i+lL else j-rU
        val max_k = if (lU==maxN || j-rL < i+lU) j-rL else i+lU
        /*
        for(
          k <- (min_k to max_k).toList;
          (x,xb) <- left((i,k));
          (y,yb) <- right((k,j))
        ) yield(((x,y),bt(xb,yb,k)))
        */
        // Optimization: 15.5 -> 11.4 (mm1_512)
        var l=List[((T,U),Backtrack)]()
        var k=min_k;
        while (k<=max_k) { var ll=left((i,k)); val r=right((k,j))
          if (r.size>0) while (ll.size>0) { var rr=r
            while (rr.size>0) { val lh=ll.head; val rh=rr.head
              l=((lh._1,rh._1),bt(lh._2,rh._2,k))::l; rr=rr.tail
            }; ll=ll.tail
          }; k=k+1;
        }; l
        // Optimization end
      } else if (track==1) {
        val min_k = if (lU==maxN || i-lU < 0) 0 else i-lU
        val max_k = i-lL
        /*
        for(
          k <- (min_k to max_k).toList;
          (x,xb) <- left((k,i)); // in1[k..i]
          (y,yb) <- right((k,j)) // M[k,j]
        ) yield(((x,y),bt(xb,yb,k)))
        */
        var l=List[((T,U),Backtrack)]()
        var k=min_k;
        while (k<=max_k) { var ll=left((k,i)); val r=right((k,j))
          if (r.size>0) while (ll.size>0) { var rr=r
            while (rr.size>0) { val lh=ll.head; val rh=rr.head
              l=((lh._1,rh._1),bt(lh._2,rh._2,k))::l; rr=rr.tail
            }; ll=ll.tail
          }; k=k+1;
        }; l
      } else if (track==2) {
        val min_k = if (rU==maxN || j-rU < 0) 0 else j-rU
        val max_k = j-rL
        /*
        for(
          k <- (min_k to max_k).toList;
          (x,xb) <- left((i,k)); // M[i,k]
          (y,yb) <- right((k,j)) // in2[k..j]
        ) yield(((x,y),bt(xb,yb,k)))
        */
        var l=List[((T,U),Backtrack)]()
        var k=min_k;
        while (k<=max_k) { var ll=left((i,k)); val r=right((k,j))
          if (r.size>0) while (ll.size>0) { var rr=r
            while (rr.size>0) { val lh=ll.head; val rh=rr.head
              l=((lh._1,rh._1),bt(lh._2,rh._2,k))::l; rr=rr.tail
            }; ll=ll.tail
          }; k=k+1;
        }; l
      } else Nil
    }

    private def bt_split(bt:Backtrack):(Backtrack,Backtrack,Int) = bt match { case (r,idx) =>
      val a:Int=right.alt; val c:Int=left.cat;
      ((r/a,idx.take(c)), (r%a,idx.drop(c+(if (hasBt)1 else 0))), if (hasBt)idx(c) else -1)
    }
    private def sw_split(sw:Subword,kb:Int) = (sw,track,indices) match {
      case ((i,j),0,(lL,lU,rL,rU)) if i<j => // single track
        val k=if(hasBt)kb else if (rU==maxN)i+lL else Math.max(i+lL,j-rU); ((i,k),(k,j))
      case ((i,j),1,(l,u,_,_)) => val k=if(hasBt)kb else i-l; ((k,i),(k,j)) // tt:concat1
      case ((i,j),2,(_,_,l,u)) => val k=if(hasBt)kb else j-l; ((i,k),(k,j)) // tt:concat2
      case _ => ((0,0),(0,0))
    }

    def unapply(sw:Subword,bt:Backtrack) = {
      val (bl,br,k)=bt_split(bt); val (swl,swr)=sw_split(sw,k)
      for ((l,lb)<-left.unapply(swl,bl); (r,rb)<-right.unapply(swr,br)) yield (((l,r), lb:::rb))
    }
    def reapply(sw:Subword,bt:Backtrack) = {
      val (bl,br,k)=bt_split(bt); val (swl,swr)=sw_split(sw,k)
      (left.reapply(swl,bl), right.reapply(swr,br))
    }
  }

  // --------------------------------------------------------------------------
  // Signature's built-in methods
  override final def max[T:Numeric] = new (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.max)
    override def toString = "$$max$$"
  }
  override final def min[T:Numeric] = new (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.min)
    override def toString = "$$min$$"
  }
  override final def sum[T:Numeric] = new (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.sum)
    override def toString = "$$sum$$"
  }
  override final def count[T:Numeric] = new (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.length.asInstanceOf[T])
    override def toString = "$$count$$"
  }
  case class MinBy[T,U:Numeric](f:T=>U) extends (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.minBy(f))
    override def toString = "$$minBy$$"
  }
  case class MaxBy[T,U:Numeric](f:T=>U) extends (List[T]=>List[T]) {
    def apply(l:List[T]) = if (l.isEmpty) Nil else List(l.maxBy(f))
    override def toString = "$$maxBy$$"
  }
  def maxBy[T,U:Numeric](f:T=>U) = MaxBy(f)
  def minBy[T,U:Numeric](f:T=>U) = MinBy(f)
  // TODO: add co(Min|Max)(By)? => modify the aggr function to filter instead of counting, and k(Min|Max)(By)?

  // --------------------------------------------------------------------------
  // Implicit conversion, to allow 'flat' arguments functions (instead of recursive pairs)
  import scala.language.implicitConversions
  trait DeTuple { val f:Any=null }
  implicit def detuple2[A,B,R](fn:Function2[A,B,R]) = new (((A,B))=>R) with DeTuple { override val f=fn
    def apply(t:(A,B)) = { val (a,b)=t; fn(a,b) } }
  implicit def detuple3[A,B,C,R](fn:Function3[A,B,C,R]) = new ((((A,B),C))=>R) with DeTuple { override val f=fn
    def apply(t:((A,B),C)) = { val ((a,b),c)=t; fn(a,b,c) } }
  implicit def detuple4[A,B,C,D,R](fn:Function4[A,B,C,D,R]) = new (((((A,B),C),D))=>R) with DeTuple { override val f=fn
    def apply(t:(((A,B),C),D)) = { val (((a,b),c),d)=t; fn(a,b,c,d) } }
  implicit def detuple5[A,B,C,D,E,R](fn:Function5[A,B,C,D,E,R]) = new ((((((A,B),C),D),E))=>R) with DeTuple { override val f=fn
    def apply(t:((((A,B),C),D),E)) = { val ((((a,b),c),d),e)=t; fn(a,b,c,d,e) } }
  implicit def detuple6[A,B,C,D,E,F,R](fn:Function6[A,B,C,D,E,F,R]) = new (((((((A,B),C),D),E),F))=>R) with DeTuple { override val f=fn
    def apply(t:(((((A,B),C),D),E),F)) = { val (((((a,b),c),d),e),f)=t; fn(a,b,c,d,e,f) } }
  implicit def detuple7[A,B,C,D,E,F,G,R](fn:Function7[A,B,C,D,E,F,G,R]) = new ((((((((A,B),C),D),E),F),G))=>R) with DeTuple { override val f=fn
    def apply(t:((((((A,B),C),D),E),F),G)) = { val ((((((a,b),c),d),e),f),g)=t; fn(a,b,c,d,e,f,g) } }
  implicit def detuple8[A,B,C,D,E,F,G,H,R](fn:Function8[A,B,C,D,E,F,G,H,R]) = new (((((((((A,B),C),D),E),F),G),H))=>R) with DeTuple { override val f=fn
    def apply(t:(((((((A,B),C),D),E),F),G),H)) = { val (((((((a,b),c),d),e),f),g),h)=t; fn(a,b,c,d,e,f,g,h) } }
  implicit def detuple9[A,B,C,D,E,F,G,H,I,R](fn:Function9[A,B,C,D,E,F,G,H,I,R]) = new ((((((((((A,B),C),D),E),F),G),H),I))=>R) with DeTuple { override val f=fn
    def apply(t:((((((((A,B),C),D),E),F),G),H),I)) = { val ((((((((a,b),c),d),e),f),g),h),i)=t; fn(a,b,c,d,e,f,g,h,i) } }
  implicit def detuple10[A,B,C,D,E,F,G,H,I,J,R](fn:Function10[A,B,C,D,E,F,G,H,I,J,R]) = new (((((((((((A,B),C),D),E),F),G),H),I),J))=>R) with DeTuple { override val f=fn
    def apply(t:(((((((((A,B),C),D),E),F),G),H),I),J)) = { val (((((((((a,b),c),d),e),f),g),h),i),j)=t; fn(a,b,c,d,e,f,g,h,i,j) } }
  implicit def detuple11[A,B,C,D,E,F,G,H,I,J,K,R](fn:Function11[A,B,C,D,E,F,G,H,I,J,K,R]) = new ((((((((((((A,B),C),D),E),F),G),H),I),J),K))=>R) with DeTuple { override val f=fn
    def apply(t:((((((((((A,B),C),D),E),F),G),H),I),J),K)) = { val ((((((((((a,b),c),d),e),f),g),h),i),j),k)=t; fn(a,b,c,d,e,f,g,h,i,j,k) } }
  implicit def detuple12[A,B,C,D,E,F,G,H,I,J,K,L,R](fn:Function12[A,B,C,D,E,F,G,H,I,J,K,L,R]) = new (((((((((((((A,B),C),D),E),F),G),H),I),J),K),L))=>R) with DeTuple { override val f=fn
    def apply(t:(((((((((((A,B),C),D),E),F),G),H),I),J),K),L)) = { val (((((((((((a,b),c),d),e),f),g),h),i),j),k),l)=t; fn(a,b,c,d,e,f,g,h,i,j,k,l) } }
  // def detuple(n:Int) { def ls(c:Char='A') = (0 until n).map{x=>(c+x).toChar}.mkString(",")
  //   def lr(c:Char='A') = ("("*(n-1))+c+","+(1 until n).map{x=>(c+x).toChar}.mkString("),")+")"
  //   println("  implicit def detuple"+n+"["+ls()+",R](fn:Function"+n+"["+ls()+",R]) = new (("+lr()+")=>R) with DeTuple { override val f=fn\n"+
  //           "    def apply(t:"+lr()+") = { val "+lr('a')+"=t; fn("+ls('a')+") } }") }
}
