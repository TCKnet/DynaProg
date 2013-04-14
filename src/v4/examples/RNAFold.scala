package v4.examples
import v4._

// -----------------------------------------------
// RNAfolding minimizing free energy
// -----------------------------------------------
// Folding algorithm from Unafold package in the function hybrid-ss-min
// Using the grammar described in paper "GPU accelerated RNA folding algorithm"

trait RNAFoldSig extends RNASignature {
  val hairpin : ((Int,Int))=>Answer
  val stack : (Int,Answer,Int)=>Answer
  val iloop : ((Int,Int),Answer,(Int,Int))=>Answer
  val mloop : (Int,Answer,Int)=>Answer
  val left  : (Answer,Int)=>Answer
  val right : (Int,Answer)=>Answer
  val join  : (Answer,Answer)=>Answer
}

trait RNAFoldAlgebra extends RNAFoldSig {
  type Answer = Int
  override val h = min[Answer] _

  import librna.LibRNA._ // indexing convention: first base,last base
  val hairpin = cfun1((ij:(Int,Int)) => hl_energy(ij._1,ij._2-1), "ij", "return hl_energy(ij._1,ij._2-1);") // Eh
  val stack = cfun3((i:Int,s:Int,j:Int) => sr_energy(i,j) + s, "i,s,j", "return sr_energy(i,j) + s;") // Es
  val iloop = cfun3((ik:(Int,Int),s:Int,lj:(Int,Int)) => il_energy(ik._1,ik._2,lj._1-1,lj._2-1) + s, "ik,s,lj", "return il_energy(ik._1,ik._2,lj._1-1,lj._2-1) + s;") // Ei
  val mloop = cfun3((i:Int,s:Int,j:Int) => s, "i,s,j", "return s;")
  val left  = cfun2((l:Int,r:Int) => l,   "l,r", "return l;")
  val right = cfun2((l:Int,r:Int) => r,   "l,r", "return r;")
  val join  = cfun2((l:Int,r:Int) => l+r, "l,r", "return l+r;")
}

trait RNAFoldPrettyPrint extends RNAFoldSig {
  type Answer = String
  private def dots(n:Int,c:Char='.') = (0 until n).map{_=>c}.mkString
  val hairpin = (ij:(Int,Int)) => "("+dots(ij._2-ij._1-2)+")"
  val stack = (i:Int,s:String,j:Int) => "("+s+")"
  val iloop = (ik:(Int,Int),s:String,lj:(Int,Int)) => "("+dots(ik._2-1-ik._1)+s+dots(lj._2-1-lj._1)+")"
  val mloop = (i:Int,s:String,j:Int) => "("+s+")"
  val left  = (l:String,r:Int) => l+"."
  val right = (l:Int,r:String) => "."+r
  val join  = (l:String,r:String) => l+r
}

trait RNAFoldGrammar extends ADPParsers with RNAFoldSig {
  lazy val Qp:Tabulate = tabulate("Qp",(
    seq(3,maxN)                ^^ hairpin
  | eli       ~ Qp ~ eli       ^^ stack
  | seq(1,30) ~ Qp ~ seq(1,30) ^^ iloop
  | eli       ~ QM ~ eli       ^^ mloop
  ) aggregate h filter basepairing)

  lazy val QM:Tabulate = tabulate("QM",(Q ~ Q ^^ join) filter(length(4)) aggregate h)

  lazy val Q:Tabulate = tabulate("Q",(
    QM
  | Q ~ eli ^^ left
  | eli ~ Q ^^ right
  | Qp
  ) filter(length(2)) aggregate h)

  override val axiom = Q
}

object RNAFold extends App {
  object fold extends RNAFoldGrammar with RNAFoldAlgebra with CodeGen {
    override val tps=(manifest[Alphabet],manifest[Answer])
    override val benchmark = true
    override val cudaSplit = 160
  }
  object pretty extends RNAFoldGrammar with RNAFoldPrettyPrint

  fold.setParams("src/librna/vienna/rna_turner2004.par")
  def parse(seq:String) = {
    val s = fold.convert(seq)
    val (score,bt) = fold.backtrack(s).head;
    val res = pretty.build(s,bt)
    (score,bt,res)
  }

  /*
  val seq="aaaaaagggaaaagaacaaaggagacucuucuccuuuuucaaaggaagaggagacucuuucaaaaaucccucuuuu"
  val (score,bt,res)=parse(seq)
  //println("Score     : "+score);
  //println("Backtrack : "+bt);
  //println("Result    : "+res);
  println("Seq: "+seq)
  if (seq=="aaaaaagggaaaagaacaaaggagacucuucuccuuuuucaaaggaagaggagacucuuucaaaaaucccucuuuu")
    println("Ref: ((((.(((((...(((.((((((((....)))))))))))...(((((((....))))))).....))))).)))) (-24.5)")
  println("Our: "+res+" (%6.2f)".format(score/100.0))
  */
  /*
  val seq="aaaaaagggaaaagaacaaaggagacucuucuccuuuuucaaaggaagagg"
  val (score,bt) = fold.backtrack(seq.toArray).head
  val res = pretty.build(seq.toArray,bt)
  println("Folding : "+res+" (%5.2f)".format(score/100.0));
  */
  Utils.runBenchmark(
    (n:Int)=>fold.backtrack(fold.convert(Utils.genRNA(n))),
    (n:Int)=>fold.backtrack(fold.convert(Utils.genRNA(n)),fold.psTopDown)
  )
}
