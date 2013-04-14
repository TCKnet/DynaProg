package v4.examples
import v4._

// -----------------------------------------------
// Sequence alignment problem.
// -----------------------------------------------
// Demonstrates two algebrae for the same problem.
// This grammar description can deal with arbitrary
// gap cost, but has a O(n^3) complexity.
//
// Check out SWatAffine for affine gap cost and
// O(n^2) algorithmic complexity.

trait SeqAlignSig extends Signature {
  type Alphabet = Char
  val start : Unit=>Answer
  val gap1 : ((Int,Int),Answer)=>Answer
  val gap2 : (Answer,(Int,Int))=>Answer
  val pair : (Alphabet,Answer,Alphabet)=>Answer
}

trait SmithWatermanAlgebra extends SeqAlignSig {
  type Answer = Int
  override val h = max[Int] _
  private val open = -3
  private val extend = -1
  val start = cfun1((x:Unit) => 0, "","return 0;")
  private val gap = "int s=a+("+open+")+("+extend+")*(g._2-g._1-1); return s>0?s:0;"
  val gap1 = cfun2((g:(Int,Int),a:Int) => gap2(a,g), "g,a",gap) // by symmetry
  val gap2 = cfun2((a:Int,g:(Int,Int))=>{ val size=g._2-g._1; Math.max(0, a + ( open + (size-1)*extend )) }, "a,g",gap)
  val pair = cfun3((c1:Char,a:Int,c2:Char) => a + (if (c1==c2) 10 else -3), "c1,a,c2","return a+(c1==c2?10:-3);")
}

trait SmithWatermanAlgebraLMS extends SeqAlignSig with LMSGenTT {
  type Answer = Int
  val tps=(manifest[Alphabet],manifest[Answer])
  override val cudaEmpty = "-100000" // necessary to avoid codegen issues
  override val h = max[Int] _
  private val open = -3
  private val extend = -1
  val start = lfun{(x:Rep[Unit]) => unit(0)} 
  val gap1 = lfun2{(x:Rep[((Int,Int),Int)]) => { val g=x._1; val r=x._2+unit(open-extend)+(g._2-g._1)*unit(extend); if (r>unit(0)) r else unit(0) }} 
  val gap2 = lfun2{(x:Rep[(Int,(Int,Int))]) => { val g=x._2; val r=x._1+unit(open-extend)+(g._2-g._1)*unit(extend); if (r>unit(0)) r else unit(0) }} 
  val pair = lfun11{(x:Rep[(Char,(Int,Char))]) => x._2._1 + (if (x._1==x._2._2) unit(10) else unit(-3)) }
}

trait NeedlemanWunschAlgebra extends SeqAlignSig {
  type Answer = Int
  override val h = max[Int] _
  private val open = -15
  private val extend = -1
  val start = (x:Unit) => 0
  val gap1 = (g:(Int,Int),a:Int) => gap2(a,g) // by symmetry
  val gap2 = (a:Int,g:(Int,Int)) => { val size=g._2-g._1; a + ( open + (size-1)*extend ) }
  val pair = (c1:Char,a:Int,c2:Char) => a + (if (c1==c2) 4 else -3)
}

trait SeqPrettyPrint extends SeqAlignSig {
  type Answer = (String,String)
  def in1(k:Int):Alphabet; def in2(k:Int):Alphabet // make it visible
  private def gap(sw:(Int,Int),in:Function1[Int,Char]) = { val g=(sw._1 until sw._2).toList; (g.map{x=>in(x)}.mkString,g.map{x=>"-"}.mkString) }
  val start = (x:Unit) => (".",".")
  val gap1 = (g:(Int,Int),a:Answer) => { val (g1,g2)=gap(g,in1); (a._1+g1,a._2+g2) }
  val gap2 = (a:Answer,g:(Int,Int)) => { val (g2,g1)=gap(g,in2); (a._1+g1,a._2+g2) }
  val pair = (c1:Char,a:Answer,c2:Char) => (a._1+c1,a._2+c2)
}

trait SeqAlignGrammar extends TTParsers with SeqAlignSig {
  val axiom:Tabulate = tabulate("M",(
    empty                   ^^ start
  | seq() -~ axiom          ^^ gap1
  |          axiom ~- seq() ^^ gap2
  | el1   -~ axiom ~- el2   ^^ pair
  ) aggregate h,true)
}

object SeqAlign extends App {
  object SWat extends SeqAlignGrammar with SmithWatermanAlgebra with CodeGen { val tps=(manifest[Alphabet],manifest[Answer]) }
  object SWatLMS extends SeqAlignGrammar with SmithWatermanAlgebraLMS
  object NWun extends SeqAlignGrammar with NeedlemanWunschAlgebra
  object pretty extends SeqAlignGrammar with SeqPrettyPrint

  def demo {
    val seq1 = "CGATTACA"
    val seq2 = "CCCATTAGAG"
    def align(name:String,s1:String,s2:String,g:SeqAlignGrammar) = {
      val (score,bt) = g.backtrack(s1.toArray,s2.toArray).head
      val (a1,a2) = pretty.build(s1.toArray,s2.toArray,bt)
      println(name+" alignment\n- Score: "+score)
      println("- Seq1: "+a1+"\n- Seq2: "+a2+"\n")
    }
    align("Smith-Waterman",seq1,seq2,SWat) // By default, this runs on GPU as CodeGen is enabled
    align("Needleman-Wunsch",seq1,seq2,NWun)
    println(SWat.backtrack(seq1.toArray,seq2.toArray,SWat.psCPU))
  }

  def bench(size:Int,num:Int=1) {
    val run = Utils.bench(num,()=>(Utils.genDNA(size).toArray,Utils.genDNA(size).toArray))(_,_)
    println("Benchmarks: sequences of sizes "+size+", median of "+num+" samples (backtrack enabled)")
    run("Scala-TopDown",s=>SWat.backtrack(s._1,s._2,SWat.psTopDown))
    run("Scala-BottomUp",s=>SWat.backtrack(s._1,s._2,SWat.psBottomUp))
    run("CPU",s=>SWat.backtrack(s._1,s._2,SWat.psCPU))
    run("CUDA",s=>SWat.backtrack(s._1,s._2,SWat.psCUDA))
    run("LMS-Scala",s=>SWatLMS.backtrack(s._1,s._2,SWatLMS.psScalaLMS))
    run("LMS-CPU",s=>SWatLMS.backtrack(s._1,s._2,SWatLMS.psCPU))
    run("LMS-CUDA",s=>SWatLMS.backtrack(s._1,s._2,SWatLMS.psCUDA))
  }
  //demo
  bench(128,10)
}

/*
// +--------------------------------------------------------+
// | This example is VERY BAD as the sequences are computed |
// | in the forward computation (and eat additional space). |
// +--------------------------------------------------------+

trait SeqAlignSignature extends Signature {
  override type Alphabet = Char
  type Answer = (Int,String,String) // score, sequence1, sequence2

  def gap(current:Int, g:(Int,Int)):Int
  def pair(a:Alphabet,b:Alphabet):Int
}

// Smith-Waterman algebra
trait SmithWatermanAlgebra extends SeqAlignSignature {
  private val open = -3
  private val extend = -1

  def gap(score:Int, g:(Int,Int)) = { val size=g._2-g._1; Math.max(0, score + ( open + (size-1) * extend )) }
  def pair(a:Char,b:Char) = if (a==b) 10 else -3
  override val h = (l:List[Answer]) => if(l.isEmpty) Nil else List(l.maxBy(_._1))
}

// Needleman-Wunsch algebra
trait NeedlemanWunschAlgebra extends SeqAlignSignature {
  private val open = -15
  private val extend = -1

  def gap(score:Int, g:(Int,Int)) = { val size=g._2-g._1; score + ( open + (size-1) * extend ) }
  def pair(a:Char,b:Char) = if (a==b) 4 else -3
  override val h = (l:List[Answer]) => if(l.isEmpty) Nil else List(l.maxBy(_._1))
}

// Sequence alignment grammar
trait SeqAlignGrammar extends TTParsers with SeqAlignSignature {
  private def prettyGap(sw:Subword,in:Function1[Int,Char]):(String,String) = {
    val g=(sw._1 until sw._2).toList; (g.map{x=>in(x)}.mkString(""),g.map{x=>"-"}.mkString(""))
  }

  import scala.language.implicitConversions
  implicit def toCharArray(s:String):Array[Char] = s.toArray

  def align(s1:String,s2:String) = parse(s1,s2)
  def trace(s1:String,s2:String) = backtrack(s1,s2)
  val alignment:Tabulate = tabulate("M",(
    empty                       ^^ { _ => (0,".",".") }
  | seq() -~ alignment          ^^ { case (g,(score,s1,s2)) => val (g1,g2)=prettyGap(g,in1); (gap(score,g),s1+g1,s2+g2) }
  |          alignment ~- seq() ^^ { case ((score,s1,s2),g) => val (g1,g2)=prettyGap(g,in2); (gap(score,g),s1+g2,s2+g1) }
  | el1   -~ alignment ~- el2   ^^ { case (c1,((score,s1,s2),c2)) => (score+pair(c1,c2),s1+c1, s2+c2) }
  ) aggregate h)

  val axiom = alignment
}

// User program
object SeqAlign extends App {
  object SWat extends SeqAlignGrammar with SmithWatermanAlgebra   // Smith-Waterman
  object NWun extends SeqAlignGrammar with NeedlemanWunschAlgebra // Needleman-Wunsch
  val seq1 = "CGATTACA"
  val seq2 = "CCCATTAGAG"

  // Usage
  val (swScore,sw1,sw2) = SWat.align(seq1,seq2).head
  println("Smith-Waterman alignment\n- Score: "+swScore+"\n- Seq1: "+sw1+"\n- Seq2: "+sw2+"\n")
  val bt = SWat.trace(seq1,seq2)
  Utils.printBT(bt)
  println("Needleman-Wunsch score : "+NWun.build(seq1.toArray,seq2.toArray,bt.head._2)._1)
  println("--------------------------------------------\n")
  val (nwScore,nw1,nw2) = NWun.align(seq1,seq2).head
  println("Needleman-Wunsch alignment\n- Score: "+nwScore+"\n- Seq1: "+nw1+"\n- Seq2: "+nw2+"\n")
}
*/