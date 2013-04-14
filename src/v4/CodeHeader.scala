package v4

// Signature for C-generable functions
trait CFun {
  val body:String // C function body
  val args:List[(String,String)] // arguments as (name,Scala type)
  val tpe:String // result's Scala type (format as in 'type Foo = ...')
  override def hashCode = (body+args+tpe).hashCode
  override def equals(that:Any) = that match {
    case f:CFun => f.body==body && f.args==args && f.tpe==tpe
    case _ => false
  }
}

// Helper to decode types uniquely and store functions
class CodeHeader(within:Any) {
  // Types supported:
  // 1. Numerical types (int, float, boolean, ...)
  // 2. Case classes composed of 1 and 2
  // 3. Tuples containting any of 1, 2 and 3
  sealed abstract class Tp {
    def name:String = this match {
      case TPri(_,_,n) => n.substring(0,1).toLowerCase
      case TTuple(a) => "T"+a.size+a.map{_.name}.mkString
      case TClass(n,_) => "_"+n.replaceAll("[0-9a-zA-Z]+[.$]","").toLowerCase
    }
    override def toString = this match {
      case TPri(_,c,_) => c
      case TTuple(a) => addType(a.zipWithIndex.map{case(t,i)=>t.toString+" _"+(i+1) }.mkString("; "),this.name)
      case TClass(n,a) => addType(a.map{case (t,n)=>t+" "+n}.mkString("; "), n.replaceAll("[0-9a-zA-Z]+[.$]","").toLowerCase+"_t")
    }
  }
  case class TPri(s:String,c:String,n:String) extends Tp
  case class TTuple(a:List[Tp]) extends Tp
  case class TClass(n:String,a:List[(Tp,String)]) extends Tp

  // --------------------------------------------------------------------------
  // Parser for typeTag[?].toString
  private val pri = Map(("Boolean",TPri("Boolean","bool"         ,"Z")), // Scala, C, JNI
                        ("Byte"   ,TPri("Byte"   ,"unsigned char","B")),
                        ("Char"   ,TPri("Char"   ,"char"         ,"C")),
                        ("Short"  ,TPri("Short"  ,"short"        ,"S")),
                        ("Int"    ,TPri("Int"    ,"int"          ,"I")),
                        ("Long"   ,TPri("Long"   ,"long"         ,"J")),
                        ("Float"  ,TPri("Float"  ,"float"        ,"F")),
                        ("Double" ,TPri("Double" ,"double"       ,"D")),
                        ("String" ,TPri("String" ,"const char*"  ,"Ljava/lang/String;")))
  private val ctx = within.getClass.getCanonicalName
  private def up1(s:String) = s.substring(0,1).toUpperCase+s.substring(1)
  object parse extends scala.util.parsing.combinator.RegexParsers {
    def apply(str:String):Tp = parseAll(p,str) match { case Success(res,_) => res case e:NoSuccess => sys.error(e.msg) }
    def p:Parser[Tp]=(("boolean"|"byte"|"char"|"short"|"int"|"long"|"float"|"double") ^^ { x=>pri(up1(x)) }
      | opt(("scala")~".")~>("Boolean"|"Byte"|"String"|"Char"|"Short"|"Int"|"Long"|"Float"|"Double") ^^ { x=>pri(x) }
      | "(" ~> repsep(p,",") <~ ")" ^^ { a => TTuple(a) } // TypeTag notation
      | "scala.Tuple[0-9]+\\[".r ~> repsep(p,",") <~ "]" ^^ { a => TTuple(a) } // Manifest notation
      | opt("class")~>"[a-zA-Z0-9]+([.$][a-zA-Z0-9]+)*".r ^^ { x => val n0=x.replaceAll(".*[.$]",""); x match {
          case "java.lang.Integer" => pri("Int")
          case n if n.startsWith("java.lang.") => val c=n.substring(10); pri.get(c) match { case Some(p)=>p case None=>sys.error(n+" unsupported") }
          case n if n.startsWith("scala.Tuple") => sys.error("Tuples unsupported")
          case n => try { val cls:Class[_<:Any] = try { Class.forName(ctx+n0) } catch { case _ => Class.forName(n) }
            TClass(cls.getName,cls.getDeclaredFields.map{f=>if (f.getName=="$outer") sys.error("Nested class "+n+" not supported"); (apply(f.getType.toString),f.getName)}.toList)
          } catch { case e:Exception => throw new Exception(e.getMessage+" in "+n) }}}
      | failure("Illegal type expression"))
  }

  // --------------------------------------------------------------------------
  // Types declarations
  import scala.collection.mutable.HashMap
  private var tpc=0;
  
  val tps=new HashMap[String,String](); // struct body -> name
  def addType(tp:String,name:String):String = tps.getOrElseUpdate(tp,name)
  def getType(str:String):String = parse(str).toString

  // Functions declaration
  private var fnc=0;
  private val fns=new HashMap[CFun,String](); // function => name
  def add(f:CFun):String = {
    f.args.foreach { case (n,tp)=> getType(tp) }; getType(f.tpe)
    fns.getOrElseUpdate(f,{ val r=fnc; fnc=fnc+1; "fun"+r })
  }

  // Raw C code (put after all definitions)
  private var user="" // user declarations
  private var priv="" // private declarations
  def add(str:String) { user = user + str + "\n" }
  def addPriv(str:String) { priv = priv + str + "\n" }

  def flush = { // declared types must be written down in order to avoid 'incomplete type' issues
    val h1 = tps.toList.map{case (b,n) => "typedef struct __"+n+" "+n+";"}.mkString("\n") + "\n" +
             tps.toList.map{case (b,n) => "struct __"+n+" { "+b+"; };"}.mkString("\n") + "\n" + user
    val h2 = priv
    val h3 = fns.toList.sortBy(_._2).map{case (f,n) => "__attribute__((unused)) __device__ static inline "+getType(f.tpe)+" "+n+"("+
             f.args.map{case (n,tp)=>(getType(tp)+" "+n) }.mkString(", ")+") { "+f.body+" }" }.mkString("\n")+"\n"
    tps.clear(); fns.clear(); tpc=0; fnc=0; user=""; priv=""; (h1,h2,h3)
  }

  // --------------------------------------------------------------------------
  // JNI transfers

  def jniTp(tp:Tp):String = tp match { case TPri(s,_,_) => "j"+s.toLowerCase case _ => "jobject" }
  def jniRead(tp:Tp):String = {
    def norm(tp:Tp):Tp = tp match {
      case TTuple(a) => TClass("scala/Tuple"+a.size,(a map norm).zipWithIndex.map{case(t,n)=>(t,"_"+(n+1))})
      case TClass(n,a)=> TClass(n.replaceAll("\\.","/"),a map {case(t,n) => (norm(t),n)})
      case _ => tp
    }
    val jtp = jniTp(tp)
    "static unsigned jni_read(JNIEnv* env, "+jtp+"Array input, input_t** in) {\n"+
    "  if (*in) free(*in); *in=NULL; if (input==NULL) return 0;\n"+
    "  jsize i,size = env->GetArrayLength(input); if (size==0) return 0;\n"+
    "  *in=(input_t*)malloc(size*sizeof(input_t)); if (!*in) { fprintf(stderr,\"Not enough memory.\\n\"); exit(1); }\n"+
    (norm(tp) match {
      case TPri(s,c,j) =>
         "  jboolean isCopy=false; "+jtp+"* jarray = env->Get"+s+"ArrayElements(input,&isCopy);\n"+
         "  for (i=0;i<size;++i) (*in)[i] = ("+c+")jarray[i];\n"+
         "  env->Release"+s+"ArrayElements(input,jarray,0);\n"
      case tc:TClass =>
        def decl(tc:TClass,s:String):String = "  jclass cls"+s+" = env->GetObjectClass(el"+s+");\n"+tc.a.map{
          case (TPri(_,_,j),n) => "  jmethodID m"+s+n+" = env->GetMethodID(cls"+s+", \""+n+"\", \"()"+j+"\");\n"
          case (c@TClass(cl,a),n) =>
            "  jmethodID m"+s+n+" = env->GetMethodID(cls"+s+", \""+n+"\", \"()L"+cl+";\");\n"+
            "  jobject el"+s+n+" = env->CallObjectMethod(el"+s+", m"+s+n+");\n"+decl(c,s+n)
          case _ => ""
        }.mkString
        def data(tc:TClass,s:String,e:String):String = tc.a.map {
          case (TPri(t,c,_),n) => "    (*in)[i]"+e+"."+n+" = ("+c+")env->Call"+t+"Method(el"+s+", m"+s+n+");\n"
          case (c@TClass(_,a),n) => "    el"+s+n+" = env->CallObjectMethod(el"+s+", m"+s+n+");\n"+data(c,s+n,e+"."+n)
          case _ => ""
        }.mkString
        "  // Loading method handles\n"+
        "  jobject el = env->GetObjectArrayElement(input, 0);\n"+decl(tc,"")+
        "  // Loading data elements\n  for (i=0;i<size;++i) {\n"+
        "    el = env->GetObjectArrayElement(input, i);\n"+data(tc,"","")+
        "  }\n"
      case _ => sys.error("Bad normalization")
    })+"  return size;\n}\n"
  }

  def jniWrite(tp0:Tp,hasIdx:Boolean):String = {
    def wr(tp:Tp,s:String,e:String):String = tp match {
      case TPri(n,_,j) =>
        "  jclass cls"+s+" = env->FindClass(\"java/lang/"+(if (n=="Int")"Integer" else n)+"\");\n"+
        "  jmethodID ctr"+s+" = env->GetMethodID(cls"+s+",\"<init>\",\"("+j+")V\");\n"+
        "  jobject res"+s+" = env->NewObject(cls"+s+",ctr"+s+",score"+e+");\n"
      case TTuple(a) => a.zipWithIndex.map{case (t,n)=>wr(t,s+"_"+(n+1),e+"._"+(n+1)) }.mkString+
        "  jclass cls"+s+" = env->FindClass(\"scala/Tuple"+a.size+"\");\n"+
        "  jmethodID ctr"+s+" = env->GetMethodID(cls"+s+",\"<init>\",\"("+a.map{_=>"Ljava/lang/Object;"}.mkString+")V\");\n"+
        "  jobject res"+s+" = env->NewObject(cls"+s+",ctr"+s+","+(1 to a.size).map{n=>"res"+s+"_"+n}.mkString(",")+");\n"
      case TClass(n,a) => a.filter{case(TPri(_,_,_),n)=>false case _=>true}.map{case (t,n)=>wr(t,s+n,e+"."+n) }.mkString+
        "  jclass cls"+s+" = env->FindClass(\""+n.replaceAll("\\.","/")+"\");\n"+
        "  jmethodID ctr"+s+" = env->GetMethodID(cls"+s+",\"<init>\",\"("+a.map{
          case(TPri(_,_,j),_)=>j
          case(TTuple(a),_) => "Lscala/Tuple"+a.size+";"
          case(TClass(n,_),_) => "L"+n.replaceAll("\\.","/")+";"
        }.mkString+")V\");\n"+
        "  jobject res"+s+" = env->NewObject(cls"+s+",ctr"+s+","+a.map{case(TPri(_,_,_),n)=>"score"+e+"."+n case(t,n)=>"res"+s+n}.mkString(",")+");\n"
      case _ => sys.error("Bad normalization")
    }
    "static jobject jni_write(JNIEnv* env, "+tp0+" score, trace_t* trace, size_t size) {\n"+
    "  // Result object\n"+wr(tp0,"","")+"  // Backtrack trace\n"+
    "  if (trace==NULL) return res;\n"+
    "  jclass cl2 = env->FindClass(\"scala/Tuple2\");\n"+
    "  jmethodID ct2 = env->GetMethodID(cl2, \"<init>\", \"(Ljava/lang/Object;Ljava/lang/Object;)V\");\n"+
    "  #define J_PAIR(A,B) env->NewObject(cl2, ct2, A, B)\n"+
    "  jclass cli = env->FindClass(\"java/lang/Integer\");\n"+
    "  jmethodID cti = env->GetMethodID(cli, \"<init>\", \"(I)V\");\n"+
    "  #define J_INT(X) env->NewObject(cli, cti, X)\n"+
    // Nil and Cons
    "  jclass cln = env->FindClass(\"scala/collection/immutable/List\");\n"+
    "  jmethodID ctn = env->GetStaticMethodID(cln, \"empty\", \"()Lscala/collection/immutable/List;\");\n"+
    "  const jobject nil = env->CallStaticObjectMethod(cln, ctn);\n"+
    "  jclass clc = env->FindClass(\"scala/collection/immutable/$colon$colon\");\n"+
    "  jmethodID ctc = env->GetMethodID(clc, \"<init>\", \"(Ljava/lang/Object;Lscala/collection/immutable/List;)V\");\n"+
    "  #define J_CONS(H,T) env->NewObject(clc, ctc, H, T)\n"+
    // Build trace
    "  jobject jtrace = nil;\n"+
    "  for (int i=0; i<size; ++i) {\n"+
    "    jobject ixs = nil; trace_t* tr=&trace[i];\n"+
    (if(hasIdx) "    for (int l=trace_len[tr->rule]; l>0; --l) ixs = J_CONS(J_INT(tr->pos[l-1]), ixs);\n" else "")+
    "    jobject el = J_PAIR( J_PAIR(J_INT(tr->i),J_INT(tr->j)) , J_PAIR(J_INT(tr->rule),ixs) );\n"+
    "    jtrace = J_CONS(el, jtrace);\n"+
    "  }\n"+
    // Result is (Answer,Trace), Trace=List[(Subword,Backtrack)], Subword=(Int,Int), Backtrack=(Int,List[Int])
    "  return J_PAIR(res,jtrace);\n}\n"
  }
}
  /*
  // Converts a Scala value into its C representation
  def getVal(str:String):String = valParser.parse(str)
  private object valParser extends StandardTokenParsers {
    import lexical.{NumericLit,StringLit}
    lexical.reserved ++= c_types.keys.toList ++ List("e","E")
    lexical.delimiters ++= List("(",")",",","-",".")
    private def pr[T](o:Option[T]) = o match { case Some(v)=>v.toString case None => "" }
    private def num:Parser[String] = opt("-") ~
      ( opt(numericLit)~("."~>numericLit)^^{case o~d=>pr(o)+"."+d} | numericLit ) ~
      opt( ("e"|"E")~>(opt("-")~numericLit)^^{case o~d=> "e"+pr(o)+d} ) ^^ { case s~m~e => pr(s)+m+pr(e) }
    private def p:Parser[String] = (num ^^ { n=>n.toString }
      | "(" ~> repsep(p,",") <~ ")" ^^ { a=>"{"+a.mkString(",")+"}" }
      | repsep(ident,".") ^^ { x=> "("+x.mkString(".")+")" } | failure("Illegal type expression"))
    def parse(str:String):String = phrase(p)(new lexical.Scanner(str)) match { case Success(res, _)=>res case e=>sys.error(e.toString) }
  }
  // Scala Type of an composite Tuple/case classes/primary types
  def tpOf[T](a:T):String = {
    val s=a.getClass.toString;
    if (a.isInstanceOf[Product]) { val p=a.asInstanceOf[Product]; "("+(0 until p.productArity).map{x=>tpOf(p.productElement(x))}.mkString(",")+")" }
    else s match {
      case "boolean"|"byte"|"char"|"short"|"int"|"long"|"float"|"double" => s.substring(0,1).toUpperCase+s.substring(1,s.length)
      case _ if (s.startsWith("class java.lang.")) => s.substring(16) match { case "Character"=>"Char" case "Integer"=>"Int" case t=>t }
      case _ => s.substring(6) // seems that case classes extend Product, enforce that?
    }
  }
  */
