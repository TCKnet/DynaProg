package v4.examples
import v4._

// -----------------------------------------------
// Matrix chain multiplication
// with outrageously complicated structures.
// -----------------------------------------------

case class Num(i:Int,f:Float)
case class Mat(rows:Num,cols:Num)

trait MatrixSig3 extends Signature {
  type Alphabet = Mat
  type Answer = (Num,Mat) // cost, (rows,columns)

  override val h = minBy(cfun1((a:Answer)=>a._1.i, "a","return a._1.i;"))
  val single = cfun1((i:Alphabet)=>(Num(0,0),i), "i","num_t z={0,0}; return (T2_num_mat){z,i};")
  val mult = cfun2((l:Answer,r:Answer) => { (Num(l._1.i+r._1.i+ l._2.rows.i*l._2.cols.i*r._2.cols.i, 0),Mat(l._2.rows,r._2.cols)) },
             "l,r","return (T2_num_mat){ (num_t){l._1.i+r._1.i+ l._2.rows.i*l._2.cols.i*r._2.cols.i, 0},(mat_t){l._2.rows,r._2.cols} };")
}

// Matrix multiplication grammar (rules)
trait MatrixGrammar3 extends ADPParsers with MatrixSig3 {
  val matrixGrammar:Tabulate = tabulate("m1",(
    el ^^ single
  | (matrixGrammar ~ matrixGrammar) ^^ mult
  ) aggregate h,true)

  // Let us be more fancy and define testing
  val fooBar:Tabulate = tabulate("m2",matrixGrammar aggregate h)

  val ps = new ((Int,Int)=>Boolean) with CFun {
    def apply(i:Int,j:Int) = i%2==j%2
    val args=List(("i","Int"),("j","Int"))
    val body = "return i%2==j%2;"
    val tpe = "Boolean"
  }

  val ffail = new Terminal[Answer](0,0,(i:Var,j:Var)=>(List(CUser("1==0")),"(T2_num_mat){}") ) { def apply(sw:Subword) = Nil }
  val nestedAggr = tabulate("aggr",( // complexity = O(n^2) / element
    (ffail ~ matrixGrammar ^^ mult aggregate h) ~
    ((matrixGrammar filter ps filter ps) ~ matrixGrammar ^^ mult aggregate h) ^^ mult
  ) aggregate h)

  val axiom=matrixGrammar
}

// Code generator only
object MatrixMult3 extends MatrixGrammar3 with CodeGen with App {
  //val input = List((10,100),(100,5),(5,50)).toArray
  val input = List((1,2),(2,20),(20,2),(2,4),(4,2),(2,1),(1,7),(7,3)).map{case (a,b)=>Mat(Num(a,0),Num(b,0)) }.toArray // -> 1x3, 122 multiplications
  override val window = 3 // only 3 consecutive matrices (aka 2x4, 4x2, 2x1 => 16)

  // ------- Extra codegen initialization
  override val benchmark = true
  override val tps = (manifest[Alphabet],manifest[Answer])
  // ------- Extra codegen initialization

  println("------ SCALA -------------------")
  val (res1,bt1) = backtrack(input,psBottomUp).head
  println("--> "+res1)
  println("--> "+build(input,bt1))
  println("------ PLAIN C  ----------------")
  val (res2,bt2) = backtrack(input,psCPU).head
  println("--> "+res2)
  println("--> "+build(input,bt2))
  println("------ CUDA  -------------------")
  val (res3,bt3) = backtrack(input).head
  println("--> "+res3)
  println("--> "+build(input,bt3))
}
