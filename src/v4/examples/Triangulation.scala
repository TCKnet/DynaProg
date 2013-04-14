package v4.examples
import v4._

// -----------------------------------------------
// Convex polygon triangulation problem
// -----------------------------------------------

trait TriangulationSignature extends Signature {
  override type Alphabet = Char // name of the vertex
  val adj : Int => Answer // Adjacent vertices
  val split : (Answer,Answer) => Answer
}

// Cross-product algebra. This is NOT efficient !
trait TriangulationAlgebra extends TriangulationSignature {
  type Answer = (Int,Int,Int,String) // start_vertex, end_vertex, cost, pretty-print
  override val h = minBy[Answer,Int](_._3)
  val adj = (i:Int) => (i,i+1,0,"")

}

object Triangulation extends LexicalParsers with TriangulationAlgebra {
  def edge(a:Int,b:Int):Int = (in(if(b==size)0 else a),in(if(b==size)a else b)) match {
    case ('a','d') | ('a','e') | ('b','d') => 1
    case _ => 2
  }

  // string explicit concat with "-"
  def ccat(s1:String,s2:String) = if (s1=="") s2 else if (s2=="") s1 else s1+"-"+s2

  val split = (l:Answer,r:Answer) => {
    val (a1,b1,c1,s1)=l;
    val (a2,b2,c2,s2)=r;
    val (s,c) = if (Math.abs(a1%size-b2%size)<=1) ("",0) else (in(a1)+""+in(b2%size), edge(a1,b2))
    ( a1, b2, c1+c2+c, ccat(ccat(s1,s2),s) )
  }

  val axiom:Tabulate = tabulate("M",(
    chari         ^^ adj
  | axiom ~ axiom ^^ split
  ) aggregate h)

  def main(args: Array[String]) = {
    println(parse("abcdef"))
    Utils.printBT(backtrack("abcdef"))
  }
}
