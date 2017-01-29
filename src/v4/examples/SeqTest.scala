package v4.examples
import v4._

// -----------------------------------------------
// Demonstrates the correct parenthesizing for two-track grammars
// -----------------------------------------------

object SeqTest extends Signature with TTParsers with App {
  type Alphabet = Char
  type Answer = (Int,String)
  override val h = maxBy((x:Answer)=>x._1)

  def f2l(a:Answer,b:Int) = a
  def f2r(a:Int,b:Answer) = b

  def f3l(a:Answer,b:Int,c:Int) = a
  def f3(a:Int,b:Answer,c:Int) = b
  def f3r(a:Int,b:Int,c:Answer) = c

  def f4ll(a:Answer,b:Int,c:Int,d:Int) = a
  def f4l(a:Int,b:Answer,c:Int,d:Int) = b
  def f4r(a:Int,b:Int,c:Answer,d:Int) = c
  def f4rr(a:Int,b:Int,c:Int,d:Answer) = d

  def f5ll(a:Answer,b:Int,c:Int,d:Int,e:Int) = a
  def f5l(a:Int,b:Answer,c:Int,d:Int,e:Int) = b
  def f5(a:Int,b:Int,c:Answer,d:Int,e:Int) = c
  def f5r(a:Int,b:Int,c:Int,d:Answer,e:Int) = d
  def f5rr(a:Int,b:Int,c:Int,d:Int,e:Answer) = e


  val axiom:Tabulate = tabulate("a",(
    empty ^^ { _ => (0,"X") }

  | axiom ~- eli ^^ f2l
  | eli -~ axiom ^^ f2r

  | (axiom ~- eli) ~- eli ^^ f3l
  | eli -~ axiom ~- eli ^^ f3
  | eli -~ (eli -~ axiom) ^^ f3r

  | (axiom ~- eli) ~- eli ~- eli ^^ f4ll
  | (eli -~ axiom ~- eli) ~- eli ^^ f4l
  | eli -~ (eli -~ axiom ~- eli) ^^ f4r
  | eli -~ (eli -~ (eli -~ axiom)) ^^ f4rr

  | (((axiom ~- eli) ~- eli) ~- eli) ~- eli ^^ f5ll
  | ((eli -~ axiom ~- eli) ~- eli) ~- eli ^^ f5l
  | eli -~ (eli -~ axiom ~- eli) ~- eli ^^ f5
  | eli -~ (eli -~ (eli -~ axiom ~- eli)) ^^ f5r
  | eli -~ (eli -~ (eli -~ (eli -~ axiom))) ^^ f5rr

  | eli -~ (eli -~ axiom ~- eli) ~- eli ^^ { case (e1,((e2,(x,e3)),e4)) => ((in1(e2)+in2(e3)) + (in1(e1)+in2(e4))*100 + x._1, x._2+"("+(0+e1)+"-"+(0+e4)+","+(0+e2)+"-"+(0+e3)+")"  ) }
  ) aggregate h)

  val in1="10101010".map{case '1'=>'\u0001' case _=>'\u0000'}.toArray
  val in2="10101010".map{case '1'=>'\u0001' case _=>'\u0000'}.toArray
  Utils.printBT(backtrack(in1,in2))
}
