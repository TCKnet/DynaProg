package v4.examples
import v4._

// -----------------------------------------------
// El Mamun's "oldest" DP problem.
// -----------------------------------------------
// Parenthesizes placement to maximize or minimize the result of a given formula
// Full story: http://bibiserv.techfak.uni-bielefeld.de/adp/ps/elmamun.pdf

trait Bill extends Signature {
  override type Answer = Int
  def add(l:Int, c:Char, r:Int) = l + r
  def mul(l:Int, c:Char, r:Int) = l * r
}

// Algebrae
trait BuyerAlgebra extends Bill {
  override val h = min[Answer] _
}

trait SellerAlgebra extends Bill {
  override val h = max[Answer] _
}

// Common grammar
trait BillGrammar extends LexicalParsers with Bill {
  override type Alphabet = Char

  def plus = charf('+')
  def times = charf('*')

  val billGrammar:Tabulate = tabulate("M",(
    digit
  | (billGrammar ~ plus ~ billGrammar) ^^ add
  | (billGrammar ~ times ~ billGrammar) ^^ mul
  ) aggregate h)
  
  val axiom=billGrammar

  def bargain(in:String)=parse(in)
}

// User program
object ElMamun extends App {
  object buyer extends BillGrammar with BuyerAlgebra
  object seller extends BillGrammar with SellerAlgebra

  val input = "1+2*3*4+5"
  
  println("Buyer : "+buyer.bargain(input).head)
  //println(buyer.gen)
  println
  println("Seller: "+seller.bargain(input).head)
  //println(buyer.gen)
}
