package v4.fpga
import v4._

// MMAlpha code generator
// See: http://www.irisa.fr/cosi/ALPHA/index.html
//
// Restriction that apply: (we may relax some later)
// - Two-track grammar (TTParsers)
// - We only care about result (aka no backtrack)
// - We have at most 1 rule in the grammar (axiom)
// - That rule _always_ generate some result (aka tabulate.alwaysValid=true)
// - No nested aggregation
// - Only serial dependencies, aka access M[i-k,j-l] with k,l bounded
// XXX: shall we encode them somewhere ?

trait FPGACodeGen extends CodeGen with TTParsers { this:Signature=>
  // Avoid CUDA to automatically kick-in when mixing CodeGen
  override def parse(in1:Input,in2:Input,ps:ParserStyle=psBottomUp):List[Answer] = super.parse(in1,in2,ps)
  override def backtrack(in1:Input,in2:Input,ps:ParserStyle=psBottomUp):List[(Answer,Trace)] = super.backtrack(in1,in2,ps)
  // ---- END ----

  val is = scala.collection.mutable.Set[Int]()
  val js = scala.collection.mutable.Set[Int]()

  def computeDomains[T](p: Parser[T]): Unit = p match {
    case Terminal(min,max,_) => is ++= Set(min,max); js ++= Set(min,max)
    case Filter(p,_) => computeDomains(p)
    case Aggregate(p,_) => computeDomains(p)
    case Map(p,_) => computeDomains(p)
    case Or(l,r) => computeDomains(l); computeDomains(r)
    case c@Concat(l,r,t) => computeDomains(l); computeDomains(r); val x=c.indices;
      t match {
        case 1 => is ++= Set(x._1, x._2)
        case 2 => js ++= Set(x._3, x._4)
        case _ => sys.error("Does not appear in TTParsers")
      }
    case t:Tabulate => ()
  }

  def getRulesFor[T](p: Parser[T], i: Int, j: Int): Option[Parser[T]] = p match {
    case t@Terminal(_,_,_) => t match {
      case `el1` => if(i > 0) Some(t) else None
      case `el2` => if(j > 0) Some(t) else None
      case `empty` => if(i == 0 && j == 0) Some(t) else None
      case _ => Some(t)
    }
    case Filter(p,f) => getRulesFor(p,i,j).map{ p2 => Filter(p2,f)}
    case Aggregate(p,h) => getRulesFor(p,i,j).map{ p2 => Aggregate(p2,h)}
    case Map(p,f) => getRulesFor(p,i,j).map{ p2 => Map(p2,f)}
    case Or(l,r) => (getRulesFor(l,i,j), getRulesFor(r,i,j)) match {
      case (None, None) => None
      case (x@Some(_), None) => x
      case (None, y@Some(_)) => y
      case (Some(x), Some(y)) => Some(Or(x,y))
    }
    case c@Concat(l,r,t) => (getRulesFor(l,i,j), getRulesFor(r,i,j)) match {
      case (None, _) | (_, None) => None
      case (Some(x), Some(y)) => Some(new Concat(x,y,t) { override lazy val indices=c.indices })
    }
    case p:Tabulate => Some(p)
  }

  var ctr=0;
  override def genFun[T,U](f0:T=>U):String = { val f1 = f0 match { case d:DeTuple => d.f case f => f }
    f1 match { case f:CFun => ctr=ctr+1; println("fun"+ctr+" = ("+f.args.map{_._1}.mkString(",")+") => "+f.body); "fun"+ctr case _ => sys.error("Unsupported function: "+f1) }
  }

  def applyFun[T,U](f0:T=>U,args:List[String]):String = { val f1 = f0 match { case d:DeTuple => d.f case f => f }
    f1 match {
      // XXX: work around the if that must be put in another function
      case f:CFun => (f.body /: (f.args.map{_._1} zip args)){(b,a)=> b.replaceAll("(^|[^a-zA-Z0-9_])"+a._1+"([^a-zA-Z0-9_]|$)","$1"+a._2+"$2") }
      case _ => sys.error("Unsupported function: "+f1)
    }
  }

  def genParser[T](p0:Parser[T],i:Var,j:Var):List[String] = p0 match {
    case Aggregate(p,h) => 
      val f = h.toString match { case "$$max$$"=>"max" case "$$min$$"=>"min" case _=>sys.error("Unsupported aggregation: "+h) }
      val l = genParser(p,i,j); List((l.head /: l.tail){(a,b)=>f+"("+a+","+b+")"  })
    case Or(l,r) => genParser(l,i,j):::genParser(r,i,j)
    case Map(p,f) => List(applyFun(f,genParser(p,i,j)))
    case cc@Concat(l,r,t) =>
        val k:Var = (t,cc.indices) match { // only support serial dependencies
          case (0,(l,u,_,_)) if (l==u) => i.add(l)
          case (0,(_,_,l,u)) if (l==u) => j.add(-l)
          case (1,(l,u,_,_)) if (l==u) => i.add(-l)
          case (2,(_,_,l,u)) if (l==u) => j.add(-l)
          case _ => sys.error("Unsupported non-serial dependencies")
        }
        List((if (t==1) genParser(l,k,i) else genParser(l,i,k)).head,genParser(r,k,j).head)
    case `el1` => List("s1["+i+"]")
    case `el2` => List("s2["+i+"]")
    case `empty` => List("")
    case t:Tabulate => List(t.name+"["+i+","+j+"]")
    case _ => sys.error("Unsupported parser "+p0)
  }

  override def gen:String = {
    analyze; val rs=rulesOrder.map{n=>rules(n)}
    // Split the domain in tiles where different set of parsers apply
    computeDomains(axiom.inner) //for (r<-rs) computeDomains(r.inner)
    val (ix,jx) = ((is - (-1)).toArray.sorted,  (js - (-1)).toArray.sorted)

    val sb=new StringBuffer()
    sb.append("""
system sequence :{X,Y | 3<=X<=Y-1}
                (s1 : {i | 1<=i<=X} of integer;
                 s2 : {j | 1<=j<=Y} of integer)
       returns  (res : {j | 1<=j<=Y} of integer);
var
  M : {i,j | 0<=i<=X; 0<=j<=Y} of integer;
  MatchQ : {i,j | 1<=i<=X; 1<=j<=Y} of integer;
let
""");

    // Generate the axiom content
    sb.append("  "+axiom.name+"[i,j] = case\n")
    for(i <- 0 until ix.length; j <- 0 until jx.length){
      sb.append("    { |")
      sb.append(  "i >= " + ix(i) + (if(i == ix.length -1) "" else "; i < " + ix(i+1)))
      sb.append("; j >= " + jx(j) + (if(j == jx.length -1) "" else "; j < " + jx(j+1)))
      sb.append("} : ")
      getRulesFor(axiom.inner, ix(i), jx(j)) match {
        case Some(p) => sb.append(genParser(p,Var('i',0),Var('j',0)).head+";\n")
        case _ => sys.error("No parser applies")
      }
    }
    sb.append("  esac;")
    sb.append("""
  MatchQ[i,j] = if (s1[i] = s2[j]) then 15[] else -12[];
  res[j] = M[X,j];
tel;
""");

    sb.toString
  }

  /*
  def prettyPrint[T](p: Parser[T]):String = p match{
    case el1 => "Seq1[i]"
    case el2 => if(j > 0) Some(el2) else None
    case empty => if(i == 0 && j == 0) Some(empty) else None
    case t : Terminal => Some(t)
    case Filter(p,f) => getRulesFor(p,i,j).map{ p2 => Filter(p2,f)}
    case Aggregate(p,h) => getRulesFor(p,i,j).map{ p2 => Aggregate(p2,h)}
    case Map(p,f) => getRulesFor(p,i,j).map{ p2 => Map(p2,f)}
    case Or(l,r) => (getRulesFor(l,i,j), getRulesFor(r,i,j)) match {
      case (None, None) => None
      case (x : Some, None) => x
      case (None, y : Some) => y
      case (Some(x), Some(y)) => Some(Or(x,y))
    }
    case c@Concat(l,r,t) => (getRulesFor(l,i,j), getRulesFor(r,i,j)) match {
      case (None, _) | (_, None) => None
      case (Some(x), Some(y)) => Some(new Concat(x,y,t) { override lazy val indices=c.indices })
    }
    case p:Tabulate => Some(p)
  }
  */
}
