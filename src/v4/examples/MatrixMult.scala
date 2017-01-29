package v4.examples
import v4._

// -----------------------------------------------
// Matrix chain multiplication
// -----------------------------------------------
// Demonstrates how to backtrack and apply
// efficiently the result to another domain/algebra.

trait MatrixSig extends Signature {
  type Alphabet = (Int,Int) // Input matrices as (rows, columns)
  val single:Alphabet=>Answer
  val mult:(Answer,Answer)=>Answer
}

// Grammar
trait MatrixGrammar extends ADPParsers with MatrixSig {
  val chain:Tabulate = tabulate("M",(
    el              ^^ single
  | (chain ~ chain) ^^ mult
  ) aggregate h,true)

  val axiom=chain
}

// -----------------------------------------------

// Computation cost algebra (in # of multiplications)
trait MatrixAlgebra extends MatrixSig {
  type Answer = (Int,Int,Int)              // Elements (rows, cost[#mult], columns)
  override val h = minBy[Answer,Int](_._2) // Aggregation

  val single = (i: Alphabet) => (i._1, 0, i._2)
  val mult = (l:Answer,r:Answer) => (l,r) match {
    case((r1,m1,c1),(r2,m2,c2)) => (r1, m1 + m2 + r1*c1*c2, c2)
  }
}

// Pretty-print algebra (could also be concrete multiplication)
// Note that the string grows along with the number of multiplications
trait MatrixPrettyPrint extends MatrixSig {
  type Answer = String
  val single = (i:Alphabet) => "|"+i._1+"x"+i._2+"|"
  val mult = (l:Answer,r:Answer) => "("+l+"*"+r+")"
}

// -----------------------------------------------

// Combining two algebrae manually (inefficient, requires O(n^3) storage)
trait MatrixPrettyAlgebra extends MatrixSig {
  object m extends MatrixGrammar with MatrixAlgebra
  object p extends MatrixGrammar with MatrixPrettyPrint
  type Answer = (m.Answer,p.Answer)
  val single = (i: Alphabet) => (m.single(i), p.single(i))
  val mult = (l:Answer,r:Answer) => (m.mult(l._1,r._1), p.mult(l._2,r._2))
  override val h = (l :List[Answer]) => { val s=m.h(l.map(_._1)).toSet; l.filter(e=>s.contains(e._1)) }
}

// -----------------------------------------------

// Cost algebra with manual description of the matching C functions
trait MatrixAlgebraC extends MatrixSig {
  type Answer = (Int,Int,Int) // rows, cost, columns
  val tps=(manifest[Alphabet],manifest[Answer])
  override val h = minBy(cfun1((a:Answer)=>a._2,"a","return a._2;"))
  val single = cfun1((m:Alphabet)=>(m._1,0,m._2),"m","return (T3iii){m._1,0,m._2};")
  val mult = cfun2((l:Answer,r:Answer) => (l._1, l._2+r._2 + l._1*l._3*r._3, r._3),
                    "l,r", "return (T3iii){l._1, l._2+r._2 + l._1*l._3*r._3, r._3};")
}

/*
// Cost algebra for LMS code generation
trait MatrixAlgebraLMS extends MatrixSig with LMSGenADP {
  type Answer = (Int,Int,Int) // rows, cost, columns
  val tps=(manifest[Alphabet],manifest[Answer])
  override val h = minBy(lfun((a:Rep[Answer])=>a._2))
  val single = lfun((m:Rep[Alphabet])=>(m._1,unit(0),m._2) )
  val mult = lfun2((p:Rep[(Answer,Answer)])=>{ val l=p._1; val r=p._2; (l._1, l._2+r._2 + l._1*l._3*r._3, r._3) })
}
*/

// -----------------------------------------------

// Our application
object MatrixMult extends App {
  object mmc extends MatrixGrammar with MatrixAlgebraC with CodeGen // CodeGen enabled
  // object mml extends MatrixGrammar with MatrixAlgebraLMS // LMS enabled

  // Demonstration of grammar and algebrae usage
  def demo = {
    object pr extends MatrixGrammar with MatrixPrettyPrint    // pretty print elements
    object mm1 extends MatrixGrammar with MatrixAlgebra       // simple costing algebra
    object mmp extends MatrixGrammar with MatrixPrettyAlgebra // cross product (vanilla ADP)
    object mmco extends MatrixGrammar with MatrixAlgebra {    // co-optimal results
      override val h = (l:List[Answer]) => { val m=l.map{_._2}.min; l.filter{x=>x._2==m} }
    }

    val mats = Array((3,2),(2,4),(4,2),(2,5)) // master report + paper example --> 3x5 matrix, 58 multiplications, single optimal
    println("\nOriginally, we computed matrix cost, with ad-hoc backtrack")
    println("  "+mm1.parse(mats).head+" --> how to backtrack ?\n")
    println("Then ADP proposed cross product, computing two results at the same time")
    println("  "+mmp.parse(mats).head+"\n")
    println("Unfortunately, this is not most efficient ...")
    println("- ADPfusion propose to compute string(result) during backtrack")
    println("- Instead, we propose backtraces that are algebra-agnostic")
    println("A backtraces is a sequence of (position[i,j],(rule,concat_indices))")
    val bt = mm1.backtrack(mats).head._2
    println("  "+bt+"\n")
    println("Now, we can use it with another algebra to compute solution")
    println("  "+pr.build(mats,bt)+"\n")
    println("Also we can build backtraces on multiple devices:")
    println("We convert our parser into efficient C code")
    val bt2 = mmc.backtrack(mats,mmc.psCPU).head._2; println("  "+bt2)
    println("Or even run it on the GPU (CUDA)")
    val bt3 = mmc.backtrack(mats,mmc.psCUDA).head._2; println("  "+bt3+"\n")
    println("But wait, these are not the same backtraces !!")
    println("-> Backtrace has a partial ordering, different traces for the same result")
    println("    "+pr.build(mats,bt)+" = "+pr.build(mats,bt2)+" = "+pr.build(mats,bt3))
    println("-> Or different results with the same cost")
    for((r,i)<-mmco.backtrack(mats).zipWithIndex) println("   "+(i+1)+": "+r._1+" <-- "+r._2)
    /*
    println
    println("You think that's all ? No, we can use LMS to generate more efficient ...")
    println("Scala (converting into generators)")
    println("  "+mml.parse(mats))
    println("C code")
    println("  "+mml.parse(mats,mml.psCPU))
    println("GPU code (CUDA)")
    println("  "+mml.parse(mats,mml.psCUDA))
    */
  }

  // Correctness verification (simple tests due to the problem nature)
  def check {
    val mats = Array((1,2),(2,20),(20,2),(2,4),(4,2),(2,1),(1,7),(7,3)) // --> 1x3 matrix, 122 multiplications, co-optimal solutions
    def test(name:String,tr:mmc.Trace) = print(if (mmc.build(mats,tr)==(1,122,3)) "." else " FAIL("+name+") ") // reference result
    print("MatrixMult: ")
    test("ScalaTopDown",mmc.backtrack(mats,mmc.psTopDown).head._2)
    test("ScalaBottomUp",mmc.backtrack(mats,mmc.psBottomUp).head._2)
    test("CPU",mmc.backtrack(mats,mmc.psCPU).head._2)
    test("CUDA",mmc.backtrack(mats,mmc.psCUDA).head._2)
    /*
    test("LMS-Scala",mml.backtrack(mats,mml.psScalaLMS).head._2)
    test("LMS-CPU",mml.backtrack(mats,mml.psCPU).head._2)
    test("LMS-CUDA",mml.backtrack(mats,mml.psCUDA).head._2)
    */
  }

  def bench(size:Int,num:Int=1) {
    val run = Utils.bench(num,()=>Utils.genMats(size)) _
    println("Benchmarks: chain of "+size+" matrices, median of "+num+" samples")
    run("Scala-TopDown",mats=>mmc.parse(mats,mmc.psTopDown))
    run("Scala-TopDown+BT",mats=>mmc.backtrack(mats,mmc.psTopDown))
    run("Scala-BottomUp",mats=>mmc.parse(mats,mmc.psBottomUp))
    run("Scala-BottomUp+BT",mats=>mmc.backtrack(mats,mmc.psBottomUp))
    run("CPU",mats=>mmc.parse(mats,mmc.psCPU))
    run("CPU+BT",mats=>mmc.backtrack(mats,mmc.psCPU))
    run("CUDA",mats=>mmc.parse(mats,mmc.psCUDA))
    run("CUDA+BT",mats=>mmc.backtrack(mats,mmc.psCUDA))
    /*
    run("LMS-Scala",mats=>mml.parse(mats,mml.psScalaLMS))
    run("LMS-Scala+BT",mats=>mml.backtrack(mats,mml.psScalaLMS))
    run("LMS-CPU",mats=>mml.parse(mats,mml.psCPU))
    run("LMS-CPU+BT",mats=>mml.backtrack(mats,mml.psCPU))
    run("LMS-CUDA",mats=>mml.parse(mats,mml.psCUDA))
    run("LMS-CUDA+BT",mats=>mml.backtrack(mats,mml.psCUDA))
    */
  }
  //demo
  //check
  bench(128,10)
}
