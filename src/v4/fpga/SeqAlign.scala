package v4.fpga
import v4._

trait SeqSig extends Signature {
  type Alphabet = Char
  val start : Unit=>Answer
  val gap1 : (Char,Answer)=>Answer
  val gap2 : (Answer,Char)=>Answer
  val pair : (Alphabet,Answer,Alphabet)=>Answer
}

trait SWat extends SeqSig {
  type Answer = Int
  override val h = max[Int] _
  //def start(x:Unit) = 0
  val start = cfun1((x:Unit) => 0, "","0[]")
  val gap1 = cfun2((g:Char,a:Int) => Math.max(0, a-3), "g,a","max(0,a-8[])")
  val gap2 = cfun2((a:Int,g:Char) => Math.max(0, a-3), "a,g","max(0,a-8[])")
  val pair = cfun3((c1:Char,a:Int,c2:Char) => if (c1==c2) a+10 else Math.max(0,a-3), "c1,a,c2", "if (c1==c2) a+15[] else a-12[]") 

//  val start = cfun1((x:Unit) => 0, "","return 0;")
//  val open1 = cfun2((c1:Alphabet,a:Answer) => Math.max(0, a+open), "c1,a", "return a>3 ? a-3 : 0;")
//  val open2 = cfun2((a:Answer,c2:Alphabet) => Math.max(0, a+open), "a,c2", "return a>3 ? a-3 : 0;")

}

trait PrettyPrint extends SeqSig {
  type Answer = (String,String)
  val start = (x:Unit) => ("","")
  val gap1 = (g:Char,a:Answer) => (a._1+g,a._2+"-")
  val gap2 = (a:Answer,g:Char) => (a._1+"-",a._2+g)
  val pair = (c1:Char,a:Answer,c2:Char) => (a._1+c1,a._2+c2)
}

trait SeqAlignGrammar extends TTParsers with SeqSig {
  val axiom:Tabulate = tabulate("M",(
    empty               ^^ start
  | el1 -~ axiom        ^^ gap1
  |        axiom ~- el2 ^^ gap2
  | el1 -~ axiom ~- el2 ^^ pair
  ) aggregate h,true)
}

object SeqAlign extends SeqAlignGrammar with SWat with FPGACodeGen with App {
  override val tps=(manifest[Alphabet],manifest[Answer])

  // Testing Scala parsers
  object pretty extends SeqAlignGrammar with PrettyPrint
  val s1 = "CGATTACA".toArray
  val s2 = "CCCATTAGAG".toArray
  val (score,bt) = backtrack(s1,s2).head
  val (a1,a2) = pretty.build(s1,s2,bt)
  println("Alignment score: "+score)
  println("  [ "+a1+" ]\n  [ "+a2+" ]\n")

  // Generating MMAlpha code
  println(gen)
}

/*
system sequence :{X,Y | 3<=X<=Y-1}
                (QS : {i | 1<=i<=X} of integer;
                 DB : {j | 1<=j<=Y} of integer)
       returns  (res : {j | 1<=j<=Y} of integer);
var
  M : {i,j | 0<=i<=X; 0<=j<=Y} of integer;
  MatchQ : {i,j | 1<=i<=X; 1<=j<=Y} of integer;
let
  M[i,j] =
      case
        { | i=0} : 0[];
        { | 1<=i; j=0} : 0[];
        { | 1<=i; 1<=j} : max(max(0[], (M[i,j-1] - 8[])), max((M[i-1,j] - 8[]), (M[i-1,j-1] + MatchQ[i,j])));
      esac;
  MatchQ[i,j] = if (QS[i] = DB[j]) then 15[] else -12[];
  res[j] = M[X,j];
tel;
*/
