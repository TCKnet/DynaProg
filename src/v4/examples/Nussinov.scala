package v4.examples
import v4._

// -----------------------------------------------
// Nussinov RNA folding
// -----------------------------------------------

trait NussinovSig extends Signature {
  type Alphabet = Char
  val nil   : Unit=>Answer
  val left  : (Alphabet,Answer)=>Answer
  val right : (Answer,Alphabet)=>Answer
  val pair  : (Alphabet,Answer,Alphabet)=>Answer
  val split : (Answer,Answer)=>Answer
}

trait NussinovAlgebra extends NussinovSig {
  type Answer = Int
  val nil   = cfun1((a: Unit) => 0, "", "return 0;")
  val left  = cfun2((c: Alphabet,a: Answer) => a, "c,a", "return a;")
  val right = cfun2((a: Answer,c: Alphabet) => a, "a,c", "return a;")
  val pair  = cfun3((l: Alphabet, a: Answer, r: Alphabet) => a+1, "l,a,r", "return a+1;")
  val split = cfun2((l: Answer, r: Answer) => l + r, "l,r", "return l+r;")
  override val h = max[Answer] _
}

trait NussinovString extends NussinovSig {
  type Answer = String
  val nil = (a:Unit) => ""
  val left = (c: Alphabet,a: Answer) => "."+a
  val right = (a: Answer, c:Alphabet) => a+"."
  val pair = (l: Alphabet, a: Answer, r: Alphabet) => "("+a+")"
  val split = (l: Answer, r: Answer) => l+r
}

/*
// Combining two algebrae: a bad idea as it is inefficient
trait NussinovPrettyAlgebra extends NussinovSig {
  type Answer = (Int, String)
  val nil = (a: Unit) => (0, "")
  val left = (c: Alphabet,a: Answer) => (a._1, "."+a._2)
  val right = (a: Answer, c:Alphabet) => (a._1, a._2+".")
  val pair = (l: Alphabet, a: Answer, r: Alphabet) => (a._1 + 1, "("+a._2+")")
  val split = (l: Answer, r: Answer) => (l._1 + r._1, l._2 + r._2)
  override val h = maxBy((a:Answer)=>a._1)
}
*/

trait NussinovGrammar extends ADPParsers with NussinovSig {
  val basePair = cfun2((i:Int,j:Int) => if (i+2>j) false else (in(i),in(j-1)) match {
    case ('a','u') | ('u','a') | ('g','u') | ('u','g') | ('c','g') | ('g','c') => true
    case _ => false
  },"i,j","if (i+2>j) return false; char a=_in1[i],b=_in1[j-1];"+
    " return (a=='a'&&b=='u') || (a=='u'&&b=='a') || (a=='g'&&b=='u') || (a=='u'&&b=='g') || (a=='c'&&b=='g') || (a=='g'&&b=='c');")

  // Grammar variation
  /*
  val s:Tabulate = tabulate("s",(
    empty  ^^ nil
  | el ~ s ^^ left
  | s ~ el ^^ right
  | s ~ t  ^^ split
  ) aggregate h)
  val t:Tabulate = tabulate("t", (el ~ s ~ el filter basePair) ^^ pair)
  */

  // Nussinov78 grammar
  val s:Tabulate = tabulate("s",(
    empty  ^^ nil
  | el ~ s ^^ left
  | s ~ el ^^ right
  | (el ~ s ~ el filter basePair) ^^ pair
  | s ~ s ^^ split
  ) aggregate h,true)

  val axiom=s
}

object Nussinov extends App {
  object nu extends NussinovGrammar with NussinovAlgebra with CodeGen {
    override val tps = (manifest[Alphabet],manifest[Answer])
    override val benchmark = true
  }
  object pretty extends NussinovGrammar with NussinovString
  /*
  def testSeq(seq:String) = {
    val s=seq.toArray
    val (score,bt) = nu.backtrack(s).head
    println(seq+"\nGPU: "+pretty.build(s,bt)+" ("+score+")")
    val (score2,bt2) = nu.backtrack(s,nu.psTopDown).head
    println("CPU: "+pretty.build(s,bt2)+" ("+score2+")\n")
  }
  for (k<-0 until 30) testSeq(Utils.genRNA(180))
  */

  for (k<-0 until 10) {
    println("OK")
    val s = Utils.genRNA(1000).toArray
    nu.time("CPU")(()=>nu.backtrack(s,nu.psCPU))
  }
}
