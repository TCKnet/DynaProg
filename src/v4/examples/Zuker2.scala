package v4.examples
import v4._

// -----------------------------------------------
// Variant of Zuker folding (Minimum Free Energy)
// -----------------------------------------------
// Variant based on Haskell RNAFold-1.99
// Formulae are according to TCK's empirical guess

// -----------------------------------------------
//
//             CURRENTLY NOT WORKING !
//
// -----------------------------------------------

trait Zuker2Sig extends RNASignature {
  val iloopI : ((Int,Int),Answer,(Int,Int)) => Answer
  val multiI : (Answer,Answer) => Answer
  // weak
  val multiO : (Int,Answer,Int) => Answer
  val iloopO : (Int,Answer,Int) => Answer
  val iloop1N : ((Int,Int),Answer,(Int,Int)) => Answer
  val iloopN1 : ((Int,Int),Answer,(Int,Int)) => Answer
  val bulgeL : ((Int,Int),Answer,Int) => Answer
  val bulgeR : (Int,Answer,(Int,Int)) => Answer
  val tinyloop : ((Int,Int),Answer,(Int,Int)) => Answer
  val hairpin : (Int,(Int,Int),Int) => Answer
  // block
  val justStem : (Int,Answer,Int) => Answer
  val regionStem : (Int,Answer) => Answer
  // comps
  val bs : (Answer,Int) => Answer
  val bc : (Answer,Answer) => Answer
  // struct
  val iD : Answer => Answer
  val cm : (Answer,Answer) => Answer
  val rS : (Int,Answer) => Answer
  val nil : Unit => Answer
}

trait Zuker2MFE extends Zuker2Sig {
  type Answer = Int
  import librna.LibRNA._
  override val h = min[Int] _

  val iloopI = cfun3((r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1),
                                        "r1,x,r2","return x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1);")

  val multiI = cfun2((l:Int,r:Int) => l+r, "l,r","return l+r;")
  // weak
  val multiO = cfun3((l:Int,e:Int,r:Int) => sr_energy(l,r)+e, "l,e,r","return sr_energy(l,r)+e;")
  val iloopO = cfun3((l:Int,e:Int,r:Int) => sr_energy(l,r)+e, "l,e,r","return sr_energy(l,r)+e;")

  val iloop1N = cfun3((r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1),
                                         "r1,x,r2","return x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1);")

  val iloopN1 = cfun3((r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1),
                                         "r1,x,r2","return x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1);")

  val bulgeL = cfun3((b:(Int,Int),x:Int,r:Int) => x + bl_energy(b._1,b._1+1,b._2-1,r,r-1),
                                  "b,x,r","return x + bl_energy(b._1,b._1+1,b._2-1,r,r-1);")

  val bulgeR = cfun3((i:Int,x:Int,b:(Int,Int)) => x + br_energy(i,b._1,b._2-2,b._2-1,i+1),
                                  "i,x,b","return x + br_energy(i,b._1,b._2-2,b._2-1,i+1);")

  val tinyloop = cfun3((l:(Int,Int),e:Int,r:(Int,Int)) => e + il_energy(l._1,l._2,r._1-1,r._2-1),
                                          "l,e,r","return e + il_energy(l._1,l._2,r._1-1,r._2-1);")

  val hairpin = cfun3((l:Int,e:(Int,Int),r:Int) => hl_energy(l,r),
                                   "l,e,r","return hl_energy(l,r);")
  // block
  val justStem = cfun3((l:Int,e:Int,r:Int) => e, "l,e,r","return e;")
  val regionStem = cfun2((l:Int,r:Int) => r, "l,r","return r;")
  // comps
  val bs = cfun2((e:Int,r:Int)=>e, "e,r","return e;")
  val bc = cfun2((l:Int,r:Int)=>l+r, "l,r","return l+r;")
  // struct
  val iD = cfun1((e:Int)=>e, "e","return e;")
  val cm = cfun2((x:Int,e:Int)=>x+e, "x,e","return x+e;")
  val rS = cfun2((x:Int,e:Int)=>e, "x,e","return e;")
  val nil = cfun1((d:Unit)=>0,"","return 0;")
}
/*
trait Zuker2Pretty extends Zuker2Sig {
  val iloopI = (r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1)
  val multiI = (l:Int,r:Int) => l+r // guessed
  // weak
  val multiO = (l:Int,e:Int,r:Int) => { println("stack"+(l,r)); sr_energy(l,r)+e }
  val iloopO = (l:Int,e:Int,r:Int) => { println("stack"+(l,r)); sr_energy(l,r)+e }
  val iloop1N = (r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1)
  val iloopN1 = (r1:(Int,Int),x:Int,r2:(Int,Int)) => x + il_energy(r1._1,r1._2,r2._1-1,r2._2-1)
  val bulgeL = (b:(Int,Int),x:Int,r:Int) => x + bl_energy(b._1,b._1+1,b._2-1,r,r-1) // guess ok
  val bulgeR = (i:Int,x:Int,b:(Int,Int)) => x + br_energy(i,b._1,b._2-2,b._2-1,i+1) // guess ok
  val tinyloop = (l:(Int,Int),e:Int,r:(Int,Int)) => e + il_energy(l._1,l._2,r._1-1,r._2-1) // guess ok
  val hairpin = (l:Int,e:(Int,Int),r:Int) => hl_energy(l,r) // guess ok
  // block
  val justStem = (l:Int,e:Int,r:Int) => e // guessed ok
  val regionStem = (l:Int,r:Int) => r // guessed ok
  // comps
  val bs = cfun2((e:Int,r:Int)=>e, "e,r","return e;")
  val bc = cfun2((l:Int,r:Int)=>l+r, "l,r","return l+r;")
  // struct
  val iD = cfun1((e:Int)=>e, "e","return e;")
  val cm = cfun2((x:Int,e:Int)=>x+e, "x,e","return x+e;")
  val rS = cfun2((x:Int,e:Int)=>e, "x,e","return e;")
  val nil = cfun1((d:Unit)=>0,"","return 0;")
}
*/

trait Zuker2Grammar extends Zuker2Sig with ADPParsers {
  lazy val iif = primary ~(3,30,1,maxN)~   weak  ~(1,maxN,3,30)~  primary ^^ iloopI aggregate h
  lazy val mif = block   ~(1,maxN,1,maxN)~ comps                          ^^ multiI aggregate h

  val weak:Tabulate = tabulate("we",(
    baseLr     ~(1,1,1,maxN)~   mif      ~(1,maxN,1,1)~   baselR     ^^ multiO
  | baseLr     ~(1,1,1,maxN)~   iif      ~(1,maxN,1,1)~   baselR     ^^ iloopO
  | primary    ~(3,3,1,maxN)~   weak     ~(1,maxN,5,31)~  primary    ^^ iloop1N
  | primary    ~(5,31,1,maxN)~  weak     ~(1,maxN,3,3)~   primary    ^^ iloopN1
  | baseLr     ~(1,1,1,maxN)~   weak     ~(1,maxN,3,31)~  primary    ^^ bulgeR
  | primary    ~(3,31,1,maxN)~  weak     ~(1,maxN,1,1)~   baselR     ^^ bulgeL
  | primaryPR  ~(1,4,1,maxN)~   weak     ~(1,maxN,1,4)~   primaryPL  ^^ tinyloop
  | baseLr     ~(1,1,1,maxN)~   primary  ~(1,maxN,1,1)~   baselR     ^^ hairpin
  ) aggregate h filter basepairing)

  val block:Tabulate = tabulate("bl",(
    baseLr ~(1,1,1,maxN)~ weak  ~(1,maxN,1,1)~ baselR  ^^ justStem // adjustStream n
  | base   ~(1,1,1,maxN)~ block                        ^^ regionStem
  ) aggregate h)

  val reglen = new Terminal[Int](0,maxN,(i:Var,j:Var) => (Nil,"(("+j+")-("+i+"))")) { def apply(sw:Subword) = List((sw._2-sw._1,bt0)) }
  val comps:Tabulate = tabulate("co",(
    block ~(1,maxN,1,maxN)~ reglen  ^^ bs
  | block ~(1,maxN,1,maxN)~ comps   ^^ bc
  | block                           ^^ iD
  ) aggregate h)

  val tail = cfun2((i:Int,j:Int) => j==size, "i,j","return j==M_W-1;")
  val struct:Tabulate = tabulate("st",(
    weak                           ^^ iD
  | base ~(1,1,0,maxN)~    struct  ^^ rS
  | weak ~(1,maxN,1,maxN)~ struct  ^^ cm
  | empty                          ^^ nil
  ) aggregate h filter tail, true)

  val axiom = struct

  // XXX: Faked parsers
  val base = eli
  val baseLr = eli
  val baselR = eli
  val primary = seq()
  val primaryPL = seq()
  val primaryPR = seq()
}

object Zuker2 extends App {
  object mfe extends Zuker2Grammar with Zuker2MFE with CodeGen {
    override val benchmark = true
    override val tps = (manifest[Alphabet],manifest[Answer])
    override val cudaSplit = 320
    override val cudaEmpty = "999999"
  }

  def testSeq(seq:String) {
    val (cpu,btC) = mfe.backtrack(mfe.convert(seq),mfe.psCPU).head
    val ref=mfe.time("ViennaRNA")(()=>Utils.refFold(seq,"resources/RNAfold_orig_mac --noPS --noLP -d2"))
    println("\nSeq: "+seq+"\nRef: "+ref+"\nCPU: "+cpu+"\n")
  }

/*
  for (k<-1 to 10) {
    testSeq(Utils.genRNA(100*k))
  }
*/
  testSeq(Utils.genRNA(1000))

  println("Not implemented")
}

  // base' :: Primary -> DIM2 -> (Scalar Nuc)
  // base' inp (Z:.i:.j) = Scalar $ index inp (Z:.i)

  //-- | Nucleotide, second one to the right. The assertion allows a size of one or
  // -- two to capture special cases of looking outside of the bounds (used by
  // -- @justStemF@ in RNAfold).
  // baseLr' :: Primary -> DIM2 -> (Scalar (Nuc :!: Nuc))
  // baseLr' inp (Z:.i:.j)
  //   = assert (let (_,Z:.n) = bounds inp in (i+1==j || i+2==j) && i>=0 && i+1<=n)
  //   . Scalar $ (index inp (Z:.i) :!: index inp (Z:.i+1))

  // baselR' :: Primary -> DIM2 -> (Scalar (Nuc :!: Nuc))
  // baselR' inp (Z:.i:.j)
  //   = assert (let (_,Z:.n) = bounds inp in i+1==j && i>0 && i<=n)
  //   . Scalar $ (index inp (Z:.i-1) :!: index inp (Z:.i))

  // -- | A 'Primary' together with the lowest included nucleotide and the highest included nucleotide.
  // primary' :: Primary -> DIM2 -> (Scalar (Primary :!: Int :!: Int))
  // primary' inp (Z:.i:.j)
  //   = assert (let (_,Z:.u) = bounds inp in i>=0 && j-1<=u && i<=j)
  //   $ Scalar (inp :!: i :!: j-1)

  // -- | A 'Primary' together with the lowest included nucleotide and the highest included nucleotide.
  // primaryPR' :: Primary -> DIM2 -> (Scalar (Primary :!: Int :!: Int))
  // primaryPR' inp (Z:.i:.j)
  //   = assert (let (_,Z:.u) = bounds inp in i>=0 && j-2<=u && i<=j)
  //   $ Scalar (inp :!: i :!: j)

  // -- | A 'Primary' together with the lowest included nucleotide and the highest included nucleotide.
  // primaryPL' :: Primary -> DIM2 -> (Scalar (Primary :!: Int :!: Int))
  // primaryPL' inp (Z:.i:.j)
  //   = assert (let (_,Z:.u) = bounds inp in i>0 && j-1<=u && i<=j)
  //   $ Scalar (inp :!: i-1 :!: j-1)

/*
BACKTACKING GRAMMAR
  weakG :: DIM2 -> [String]
  weakG = (
            multiBT ener    <<< baseLr -~+ block'  +~+ comps' +~- baselR             |||
            iloopBT ener    <<< baseLr -~+ primary #~~ weak'  ~~# primary +~- baselR |||
            iloop1NBT ener  <<< primary ---~+ weak'   +~@   primary |||
            iloopN1BT ener  <<< primary @~+   weak'   +~--- primary |||
            bulgeRBT ener   <<< baseLr  -~+   weak'   +~*   primary |||
            bulgeLBT ener   <<< primary *~+   weak'   +~-   baselR  |||
            tinyloopBT ener <<< primaryPR &~+   weak'   +~&   primaryPL |||
            hairpinBT  ener <<< baseLr  -~+   primary +~-   baselR  ..@ (hBT weak) `withBT` basepairing
          )

  blockG :: DIM2 -> [String]
  blockG = (
             adjustStreamBT n (justStemBT   ener <<< baseLr -~+ weak'  +~- baselR) |||
             regionStemBT ener <<< base   -~+  block'             ..@ (hBT block)
           )

  compsG :: DIM2 -> [String]
  compsG = (
             bsBT <<< block' +~+ reglen |||
             bcBT <<< block' +~+ comps' |||
             iDBT <<< block'            ..@ (hBT comps)
           )

  structG :: DIM2 -> [String]
  structG = (
              iDBT  <<< weak'             |||
              rSBT  <<< base  -~~ struct' |||
              cmBT  <<< weak' +~+ struct' |||
              nilBT <<< empty             ..@ (hBT struct `withBT` constrained (\(Z:.i:.j) -> j==n) )
            )

  multiBT ener l (b,bS) (c,cS) r =
    let e = multiOF ener l (multiIF ener b c) r
    in (e, ["("++x++y++")" | x<-bS, y<-cS])
  iloopBT ener lo ls@(_:!:li:!:lj) (w,wS) rs@(_:!:ri:!:rj) ro =
    let e = iloopOF ener lo (iloopIF ener ls w rs) ro
    in (e, L.map (\s -> "("++replicate (lj-li) '.'++"("++s++")"++replicate (rj-ri) '.'++")") wS)
  iloop1NBT ener ls (w,wS) rs@(_:!:ri:!:rj) =
    let e = iloop1NF ener ls w rs
    in (e, L.map (\s -> "(.("++s++")"++replicate (rj-ri-1) '.'++")") wS)
  iloopN1BT ener ls@(_:!:li:!:lj) (w,wS) rs =
    let e = iloopN1F ener ls w rs
    in (e, L.map (\s -> "("++replicate (lj-li-1) '.'++"("++s++").)") wS)
  bulgeRBT ener ls (w,wS) rs@(_:!:ri:!:rj) =
    let e = bulgeRF ener ls w rs
    in (e, L.map (\s -> "("++s++"."++replicate (rj-ri-1) '.'++")") wS)
  bulgeLBT ener ls@(_:!:li:!:lj) (w,wS) rs =
    let e = bulgeLF ener ls w rs
    in (e, L.map (\s -> "("++replicate (lj-li-1) '.'++"."++s++")") wS)
  hairpinBT ener llp reg@(xs:!:i:!:j) rpr =
    let e = hairpinF ener llp reg rpr
    in (e, ["(" ++ replicate (j-i+1) '.' ++ ")"])
  tinyloopBT ener ls@(_:!:li:!:lj) (w,sW) rs@(_:!:ri:!:rj) =
    let e = tinyloopF ener ls w rs
    in (e, L.map (\s -> "("++replicate (lj-li-1) '.'++s++replicate (rj-ri-1) '.'++")") sW)

  regionStemBT ener nc (w,sW) =
    let e = regionStemF ener nc w
    in (e, L.map (\s -> "." ++ s) sW)
  justStemBT ener llp (w,sW) rpr =
    let e = justStemF ener llp w rpr
    in (e, sW)

  bcBT (b,bW) (c,cW) =
    let e = b+c
    in (e,[ x++y | x<-bW, y<-cW ])
  bsBT (b,bW) reg = (b, L.map (++ replicate reg '.') bW)
  iDBT = id

  ssBT len = (ssF len, [replicate len '.'])
  rSBT n (w,wS) =
    let e = rSF n w
    in (e, map ("."++) wS)
  cmBT (w,wS) (s,sS) =
    let e = cmF w s
    in (e, [x++y | x<-wS, y<-sS])
  nilBT b = if b then (nilF b, [""]) else (nilF b, [])

  hBT tbl ij = L.concatMap snd . L.filter ((tbl!ij==).fst) . P.toList
*/
/*
(*~+) = makeLeft_MinRight (3,31) 1
(+~*) = makeMinLeft_Right 1 (3,31)
(&~+) = makeLeft_MinRight (1,4) 1
(+~&) = makeMinLeft_Right 1 (1,4)
(---~+) = makeLeft_MinRight (3,3) 1
(+~@) = makeMinLeft_Right 1 (5,31)
(+~---) = makeMinLeft_Right 1 (3,3)
(@~+) = makeLeft_MinRight (5,31) 1
(#~~) = makeLeft_MinRight (3,30) 1 // The structure on the left is a subword with size 2-28.
(~~#) xs ys = Box mk step xs ys where // The structure on the right is a subword with size 2-30 + inspect the stack an reduce the maximal size
*/
