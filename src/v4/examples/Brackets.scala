package v4.examples
import v4._

// -----------------------------------------------
// Correct parenthesization problem
// -----------------------------------------------
// Also examplifies the windowing

trait BracketsSignature extends Signature {
  type Answer = Int
  override type Alphabet = Char
}

trait BracketsAlgebra extends BracketsSignature {
  override val h = max[Answer] _
}

trait BracketsGrammar extends LexicalParsers with BracketsSignature {
  def areBrackets(i:Int, j:Int) = j > i+1 && in(i) == '(' && in(j-1) == ')'

  // Limits the size of the problem to 8 at most
  override val window = 8

  val axiom:Tabulate = tabulate("M",(
      digit
    | (char ~ axiom ~ char).filter(areBrackets _) ^^ { case ((c1,i),c2) => i }
    | axiom ~ axiom ^^ { case (x,y) => x+y }
    // Sum all digits of the string
    // | char~axiom ^^ {case (c,i)=>i} | axiom~char ^^ {case (i,c)=>i}
  ) aggregate h)

  // Test correct parentesization: good => List(0), bad => List()
  val paren:Tabulate = tabulate("N",(
      empty ^^ { _=>0 }
    | (char ~ paren ~ char).filter(areBrackets _) ^^ { case ((c1,i),c2) => i }
    | paren ~ paren ^^ { case (x,y) => x+y }
  ) aggregate h)

}

object Brackets extends App with BracketsGrammar with BracketsAlgebra {
  val str = "(((3)))((2))(1)((6))((((8))))(3)((4))"
  println("Parse result = "+parse(str))
  Utils.printBT(backtrack(str))
}
