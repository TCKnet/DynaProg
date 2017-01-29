package v4

trait ADPParsers extends BaseParsers { this:Signature =>
  // I/O interface
  private var input: Input = null
  def in(k:Int):Alphabet = input(k)
  def size:Int = input.size
  def parse(in:Input,ps:ParserStyle=psCUDA):List[Answer] = this match {
    case c:CodeGen if (ps==psCPU || ps==psCUDA) => List(c.parseC(in.asInstanceOf[c.Input],ps==psCUDA).asInstanceOf[Answer])
    case _ => run(if(ps==psBottomUp)"ScalaBotUp" else "ScalaTopDown",in,()=>{ if (ps==psBottomUp) parseBottomUp;
      (if (window>0) aggr(((0 to size-window).flatMap{x=>axiom(x,window+x)}).toList, h) else axiom(0,size)).map(_._1)
    })
  }
  def backtrack(in:Input,ps:ParserStyle=psCUDA):List[(Answer,Trace)] = this match {
    case c:CodeGen if (ps==psCPU || ps==psCUDA) => List(c.backtrackC(in.asInstanceOf[c.Input],ps==psCUDA).asInstanceOf[(Answer,Trace)])
    case _ => run(if(ps==psBottomUp)"ScalaBotUp+BT" else "ScalaTopDown+BT",in,()=>{ if (ps==psBottomUp) parseBottomUp;
      if (window>0) aggr(((0 to size-window).flatMap{x=>axiom.backtrack(x,window+x)}).toList, h) else axiom.backtrack(0,size)
    })
  }
  def build(in:Input,bt:Trace):Answer = run("Build",in,()=>axiom.build(bt))
  protected def run[T](name:String,in:Input, f:()=>T) = {
    input=in; analyze; tabInit(in.size+1,in.size+1);
    this match { case s:RNASignature => if (s.energies) librna.LibRNA.setSequence(in.mkString) case _ => }
    val res=time("Run"+name)(f);
    this match { case s:RNASignature => if (s.energies) librna.LibRNA.clear case _ => }
    tabReset; input=null; res
  }
  private def parseBottomUp {
    val rs=rulesOrder map {n=>rules(n)}; val du=(if (window>0) window else size)
    var d=0; while (d<=du) { for (r<-rs) { val iu=size-d; var i=0; while (i<=iu) { r((i,d+i)); i=i+1 } }; d=d+1; }
  }

  // Concatenation operations
  import scala.language.implicitConversions
  implicit def parserADP[T](p1:Parser[T]) = new ParserADP(p1)
  class ParserADP[T](p1:Parser[T],idx:(Int,Int,Int,Int)=null) {
    def ~ [U](p2:Parser[U]) = if (idx!=null) new Concat(p1,p2,0) { override val hint=idx } else new Concat(p1,p2,0)
    def ~ (minl:Int=0,maxl:Int=maxN,minr:Int=0,maxr:Int=maxN):ParserADP[T] = new ParserADP[T](p1,(minl,maxl,minr,maxr))
  }

  // Terminal parsers
  val empty = new Terminal[Unit](0,0,(i:Var,j:Var) => (Nil,"")) {
    def apply(sw:Subword) = { val (i,j)=sw; if (i==j) List(({},bt0)) else Nil }
  }
  val emptyi = new Terminal[Int](0,0,(i:Var,j:Var) => (Nil,"("+i+")")) {
    def apply(sw:Subword) = { val (i,j)=sw; if (i==j) List((i,bt0)) else Nil }
  }
  val el = new Terminal[Alphabet](1,1,(i:Var,j:Var) => (Nil,"_in1["+i+"]")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i+1==j) List((in(i),bt0)) else Nil }
  }
  val eli = new Terminal[Int](1,1,(i:Var,j:Var) => (Nil,"("+i+")")) {
    def apply(sw:Subword) = { val (i,j)=sw; if(i+1==j) List((i,bt0)) else Nil }
  }
  def seq(min:Int=1, max:Int=maxN) = new Terminal[Subword](min,max,(i:Var,j:Var) => (Nil,"(T2ii){"+i+","+j+"}")) {
    def apply(sw:Subword) = { val (i,j)=sw; if (i+min<=j && (max==maxN || i+max>=j)) List(((i,j),bt0)) else Nil }
  }
}

trait LexicalParsers extends ADPParsers { this:Signature =>
  override type Alphabet = Char
  // Predefined terminals
  val char = el
  val chari = eli
  val letter = char filter cf((c:Char)=>(c>='a' && c<='z') || (c>='A' && c<='Z'),"return (c>='a' && c<='z') || (c>='A' && c<='Z');")
  val digit = (char filter cf((c:Char)=>c>='0' && c<='9',"return c>='0' && c<='9';")) ^^ cfun1((c:Char)=>(c-'0').toInt,"c","return c-'0';")
  def charf(s:String) = char filter cf((c:Char)=>s.indexOf(c)!= -1,"const char* s=\""+esc(s)+"\"; for(;*s;++s) { if (*s==c) return true; } return false;")
  def charf(c0:Char) = char filter cf((c:Char)=>c==c0,"return c=='"+esc(""+c0)+"';")

  // Predefined filters
  def length(min:Int=0,max:Int= -1) = cfun2((i:Int,j:Int)=> (j-i)>=min && (max < 0 || j-i<=max), "i,j","return j-i>="+min+(if(max>=0)" && j-i<="+max else "")+";")

  import scala.language.implicitConversions
  implicit def toCharArray(s:String):Array[Char] = s.toArray
  private def esc(s:String) = (s /: List(('\\','\\'),('"','"'),('\'','\''),('\n','n'),('\r','r'),('\t','t'),('\u0000','0'),('\b','b'),('\f','f'))) {case (s,(a,b))=>s.replace(""+a,"\\"+b)}
  private def cf(f:Char=>Boolean,cc:String) = cfun1((i:Int,j:Int) => (i+1==j) && f(in(i)),"i,j","if (i+1!=j) return false; c=_in1[i]; "+cc)
  def in(k:Int):Char
}

trait RNASignature extends Signature { reqJNI
  final type Alphabet = Char
  val energies = true
  var paramsFile:String = null // parameters for CodeGen

  // Setting LibRNA parameters
  def setParams(file:String) = this match {
    case c:CodeGen => paramsFile = file
    case _ => if (energies) librna.LibRNA.setParams(file)
  }

  // Conversion to internal representation
  def convert(in:String):Array[Alphabet] = in.toArray.map { case 'A'|'a'=>'\u0001' case 'C'|'c'=>'\u0002' case 'G'|'g'=>'\u0003' case 'T'|'t'|'U'|'u'=>'\u0004' case _ => '\u0005' }

  // Predefined filters
  def length(min:Int=0,max:Int= -1) = cfun2((i:Int,j:Int) => (j-i)>=min && (max < 0 || j-i<=max), "i,j","return j-i>="+min+(if(max>=0)" && j-i<="+max else "")+";")
  val stackpairing = cfun2((i:Int,j:Int) => basepairing(i,j) && basepairing(i+1,j-1), "i,j","return i+4<=j && bp_index(_in1[i],_in1[j-1])!=NO_BP && bp_index(_in1[i+1],_in1[j-2])!=NO_BP;")
  val basepairing = cfun2((i:Int,j:Int) => if (i+2>j) false else (in(i),in(j-1)) match {
    case ('\u0001','\u0004') | ('\u0004','\u0001') | ('\u0004','\u0003') | ('\u0003','\u0004') | ('\u0003','\u0002') | ('\u0002','\u0003') => true // a-u, u-a, u-g, g-u, g-c, c-g
    case _ => false
  },"i,j","return i+2<=j && bp_index(_in1[i],_in1[j-1])!=NO_BP;")

  // Expose ADPParsers input
  def in(x:Int):Char
}
