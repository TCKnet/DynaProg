package v4.examples
import v4._

// -----------------------------------------------
// Matrix chain multiplication variant
// -----------------------------------------------
// This grammar variant is slightly faster for CUDA
// slower in all other cases

trait MatrixSig2 extends Signature {
  type Alphabet = (Int,Int) // matrix as (rows, columns)
  type Answer = Int
  override val h = min[Int] _
  val tps=(manifest[Alphabet],manifest[Answer])

  def in(n:Int):Alphabet
  val single = cfun1((i: Alphabet) => 0,"i","return 0;")
  val mult = cfun5((i:Int,l:Answer,k:Int,r:Answer,j:Int) => l+r + in(i)._1*in(k)._1*in(j-1)._2,
                                        "i,l,k,r,j","return l+r + _in1[i]._1*_in1[k]._1*_in1[j-1]._2;")
}

trait MatrixGrammar2 extends ADPParsers with MatrixSig2 {
  val axiom:Tabulate = tabulate("M",(
    el ^^ single
  | (emptyi ~ axiom ~ emptyi ~ axiom ~ emptyi) ^^ mult
  ) aggregate h,true)
}

object MatrixMult2 extends MatrixGrammar2 with MatrixSig2 with CodeGen with App {
  override val cudaSharedInput=true

  def bench(size:Int,num:Int=1) {
    val run = Utils.bench(num,()=>Utils.genMats(size)) _
    println("Benchmarks: chain of "+size+" matrices, median of "+num+" samples (backtrack enabled)")
    run("Scala-TopDown",mats=>backtrack(mats,psTopDown))
    run("Scala-BottomUp",mats=>backtrack(mats,psBottomUp))
    run("CPU",mats=>backtrack(mats,psCPU))
    run("CUDA",mats=>backtrack(mats,psCUDA))
    run("LMS-Scala",mats=>backtrack(mats,psScalaLMS))
    run("LMS-CPU",mats=>backtrack(mats,psCPU))
    run("LMS-CUDA",mats=>backtrack(mats,psCUDA))
  }
  bench(128,10)

}
