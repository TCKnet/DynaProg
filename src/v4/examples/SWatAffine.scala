package v4.examples
import v4._

// -----------------------------------------------
// Smith-Waterman with affine gap cost
// -----------------------------------------------
// Uses 3 tables but has serial dependencies, thus
// algorithmic complexity = O(n^2).

trait SWatSig extends Signature {
  type Alphabet = Char
  val start : Unit=>Answer
  val zero1 : Int => Answer
  val zero2 : Int => Answer
  val open1 : (Alphabet,Answer)=>Answer
  val open2 : (Answer,Alphabet)=>Answer
  val ext1 : (Alphabet,Answer)=>Answer
  val ext2 : (Answer,Alphabet)=>Answer
  val pair : (Alphabet,Answer,Alphabet)=>Answer
}

trait SWatAlgebra extends SWatSig {
  type Answer = Int
  override val h = max[Int] _
  private val open = -3
  private val ext = -1
  val start = cfun1((x:Unit) => 0, "","return 0;")
  val open1 = cfun2((c1:Alphabet,a:Answer) => Math.max(0, a+open), "c1,a", "return a>3 ? a-3 : 0;")
  val open2 = cfun2((a:Answer,c2:Alphabet) => Math.max(0, a+open), "a,c2", "return a>3 ? a-3 : 0;")
  val ext1 = cfun2((c1:Alphabet,a:Answer)=> Math.max(0, a+ext), "c1,a", "return a>1 ? a-1 : 0;")
  val ext2 = cfun2((a:Answer,c2:Alphabet)=> Math.max(0, a+ext), "a,c2", "return a>1 ? a-1 : 0;")
  val pair = cfun3((c1:Char,a:Answer,c2:Char) => Math.max(0, a + (if (c1==c2) 10 else -3)), "c1,a,c2","return a + (c1==c2 ? 10 : -3);")
  val zero1 = cfun1((p:Int) => 0, "p","return 0;")
  val zero2 = cfun1((p:Int) => 0, "p","return 0;")
}

trait SWatPretty extends SWatSig {
  type Answer = (String,String)
  val start = (x:Unit) => (".",".")
  val open1 = (c1:Alphabet,a:Answer) => (a._1+c1,a._2+"-")
  val open2 = (a:Answer,c2:Alphabet) => (a._1+"-",a._2+c2)
  val ext1 = open1
  val ext2 = open2
  val pair = (c1:Char,a:Answer,c2:Char) => (a._1+c1,a._2+c2)

  val zero1 = (p:Int) => (".","."+("-"*p))
  val zero2 = (p:Int) => ("."+("-"*p),".")
}

trait SWatGrammar extends TTParsers with SWatSig {
  val axiom:Tabulate = tabulate("M",(
    empty                 ^^ start
  | el1   -~ axiom ~- el2 ^^ pair
  | g1
  | g2
  ) aggregate h,true)

  val g1:Tabulate = tabulate("g1",(
    empty1       ^^ zero1
  | el1 -~ axiom ^^ open1
  | el1 -~ g1    ^^ ext1
  ) aggregate h,true)

  val g2:Tabulate = tabulate("g2",(
    empty2       ^^ zero2
  | axiom ~- el2 ^^ open2
  | g2    ~- el2 ^^ ext2
  ) aggregate h,true)
}

object SWatAffine extends App {
  object swat extends SWatGrammar with SWatAlgebra with CodeGen {
    override val tps = (manifest[Alphabet],manifest[Answer])
    override val benchmark = true
  }
  object pretty extends SWatGrammar with SWatPretty

  /*
  val s1 = "CGATTACA".toArray
  val s2 = "CCCATTAGAG".toArray
  Utils.printBT(swat.backtrack(s1,s2,true))
  Utils.printBT(swat.backtrack(s1,s2))
  val (score,bt) = swat.backtrack(s1,s2).head
  val (a1,a2) = pretty.build(s1,s2,bt)
  println("- Score: "+score)
  println("- Seq1: "+a1+"\n- Seq2: "+a2+"\n")
  */
  Utils.runBenchmark(
    (n:Int)=>swat.backtrack(Utils.genDNA(n),Utils.genDNA(n)),
    (n:Int)=>swat.backtrack(Utils.genDNA(n),Utils.genDNA(n),swat.psBottomUp)
  )
}
