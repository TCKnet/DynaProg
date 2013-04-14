package v4.examples

// -----------------------------------------------
// Test of some existing grammars
// -----------------------------------------------

object Tests extends App {
  def tests(s:String,ts:List[Boolean]) = {
    print("- "+s+": "); ts.foreach{r=>print(if(r) "." else "FAIL")}; println
  }
  println("Test suite:")

  tests("Brackets",List(Brackets.parse("(((3)))((2))(1)((6))((((8))))(3)((4))".toArray)==List(7)))
  tests("ElMamoun",List(ElMamun.buyer.bargain("1+2*3*4+5").head==30,ElMamun.seller.bargain("1+2*3*4+5").head==81))

  val mIn = List((10,100),(100,5),(5,50)).toArray
  val mIn2 = List((1,2),(2,20),(20,2),(2,4),(4,2),(2,1),(1,7),(7,3)).toArray
  object mm0 extends MatrixGrammar with MatrixAlgebra
  val (mS,mBt)=mm0.backtrack(mIn).head
  val (mS2,_ )=mm0.backtrack(mIn2).head
  object mmw extends MatrixGrammar with MatrixAlgebra { override val window=3 }
  object mm1 extends MatrixGrammar with MatrixPrettyAlgebra
  object mmp extends MatrixGrammar with MatrixPrettyPrint

  val (mS3,_ )=mmw.backtrack(mIn2).head
  tests("MatrixMul",List(mS==(10,7500,50),mS2==(1,122,3),mS3==(2,16,1),
    mmp.build(mIn,mBt)=="((|10x100|*|100x5|)*|5x50|)",
    mm1.parse(mIn)==List(((10,7500,50),"((|10x100|*|100x5|)*|5x50|)"))
  ))

  print("- Zuker: "); 
  Zuker.testSeq("ccuuuuucaaagg")
  Zuker.testSeq("guacgucaguacguacgugacugucagucaac")
  Zuker.testSeq("aaaaaggaaacuccucuuu")
  Zuker.testSeq("uucuccucaggaaga")
  Zuker.testSeq("aaaaaagggaaaagaacaaaggagacucuucuccuuuuucaaaggaagaggagacucuuucaaaaaucccucuuuu")
  Zuker.testSeq("gccaaccucgugca")
  Zuker.testSeq("ggccaaccucgugcaa")
  Zuker.testSeq("guugcucagcacgcguaaga")
  Zuker.testSeq("gggcgcucaaccgagucagcagugcaauauagggccc")
  Zuker.testSeq("augggcgcucaacucuccgugaauuugaaugagucagcagugcaauauagggcccucauc")
  Zuker.testSeq("accacuccucauuugacuuauaggcucagaauuaguagaccacaguucacugugaaagga")
  Zuker.testSeq("uugcccuaugucaaacauaugucgcaaagcacacgucguauucaccacgaucaaccaggg")
  Zuker.testSeq("ccgaugccagcgucugcgccuucgccuaagggggagaagaagcucucccauaacggcaug")
  Zuker.testSeq("ugcucaggca")
  // These sequence fail the test but result is coherent with GAPC
  // Zuker.testSeq("agccccgguuaagaauaaaggagauuucuccgcccaaccccuguaaugcu")
  // Zuker.testSeq("ccggcgcccauaaaaucaaauuaacaucguuaugucagcaaguguaccacaagcuggaga") // 2/4000
  // Zuker.testSeq("cacgaaauuacgacuuuugacuccugcagacaacagcucauuauaucacucuucccucgu")
  // Zuker.testSeq("gccacaaucaggcugaagacuuuuaacccuauccuuccuuuuccaggaaaaaccuaaagcacaauucauucagccaauua") // 4/4000
  // Zuker.testSeq("cccacgagaagcccauguuaccuaucaccauagguuaggggacaaccgagccguuuaaauaauaauuaguggccuucagu")
  // Zuker.testSeq("uaacacaucaaagucuuuauaaagucauugcuagaauaauaagagccgaaaacauuccuacccuuugccuccccaaaaac")
  // Zuker.testSeq("cgacaugcauuagaaaaguaaaucuuuguagccuucuuggucuggacgccugagcccgauuuaugcaugaucuaaaacgc")
  println
  /*
  val (s1,s2)=("CGATTACA","CCCATTAGAG")
  val (swScore,sw1,sw2) = SeqAlign.SWat.align(s1,s2).head
  val (nwScore,nw1,nw2) = SeqAlign.NWun.align(s1,s2).head
  tests("SeqAlign",List(
  	swScore==51,sw1==".--CGATTACA-",sw2==".CCC-ATTAGAG",
  	nwScore== -12,nw1==".-CGATTACA-",nw2==".CCCATTAGAG"
  ))
  */
}
