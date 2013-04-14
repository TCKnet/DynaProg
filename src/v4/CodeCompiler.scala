package v4

import java.io._
import scala.tools.nsc._
import scala.tools.nsc.reporters._

trait ScalaCompiler {
  val outPath:String
  var compiler: Global = _
  var reporter: ConsoleReporter = _
  def setupCompiler = {
    // Add outPath to the classpath
    try {
      val cl = this.getClass.getClassLoader
      val method=cl.getClass.getDeclaredMethod("addURL", classOf[java.net.URL]);
      method.setAccessible(true);
      try { method.invoke(cl, new File(outPath).toURI.toURL); }
      catch { case t:Throwable => sys.error("Unable to add outPath to classpath") }
    } catch { case t:Throwable => // Fallback, assuming we have appropriate permissions
      val currentThreadClassLoader = Thread.currentThread().getContextClassLoader();
      val urlClassLoader = new java.net.URLClassLoader(Array(new File(outPath).toURI.toURL), currentThreadClassLoader);
      Thread.currentThread().setContextClassLoader(urlClassLoader);
    }

    // Initialize compiler settings
    val settings = new Settings()
    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))
    compiler = new Global(settings, reporter)
  }

  def compile[T](className:String,source:String)(implicit mT: scala.reflect.ClassTag[T]): T = {
    if (this.compiler eq null) setupCompiler
    val compiler = this.compiler
    val run = new compiler.Run
    new File(outPath).mkdirs
    val fileSystem = new scala.tools.nsc.io.PlainFile(outPath) // this fails with VirtualDirectory("<vfs>", None). Bug?
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    run.compileSources(List(new scala.reflect.internal.util.BatchSourceFile("<stdin>", source)))
    reporter.printSummary(); reporter.reset
    val parent = this.getClass.getClassLoader
    val loader = new scala.tools.nsc.interpreter.AbstractFileClassLoader(fileSystem, parent)
    val cls: Class[_] = loader.loadClass(className)
    cls.newInstance.asInstanceOf[T]
  }
}

// Singleton counter
object CompileCounter {
  private var count = 1
  def get:Int = count
  def next = { count=count+1 }
  def getNext = { val v=count; count=count+1; v }
}

trait CCompiler {
  val outPath:String
  val cudaPath:String
  val cudaFlags:String
  val ccFlags:String
  val ldFlags:String

  var dataMap = new scala.collection.mutable.HashMap[String,String]()

  def replace(content:String, map:Map[String,String]) = {
    val builder = new StringBuilder(content);
    def rep(o:String, n:String) = {
      val ol=o.length; val nl=n.length
      var idx=builder.indexOf(o)
      while (idx != -1) { builder.replace(idx,idx+ol,n); idx=builder.indexOf(o,idx+nl) }
    }
    map.foreach { case (o,n) => rep("{"+o+"}",n) }
    builder.toString
  }

  // create/append to a file (c,cpp,cu,h)
  def add(ext:String, content:String):Unit = add(ext,content,Map())
  def add(ext:String, content:String, map:Map[String,String]) {
    val cnt = replace(content,map)
    dataMap.get(ext) match {
      case Some(o) => dataMap += ((ext,o+"\n/*---EOF---*/\n"+cnt))
      case None => dataMap += ((ext,cnt))
    }
  }

  // execute arbitrary command, return (out,err)
  private def run(cmd:String):(String,String) = {
    val p = Runtime.getRuntime.exec(cmd,null,new File(outPath));
    def gobble(in:InputStream) = new Runnable {
      var out = new StringBuilder
      var thr = new Thread(this); thr.start
      override def toString = { thr.join; out.toString.trim }
      override def run {
        val r = new BufferedReader(new InputStreamReader(in))
        var l = r.readLine; while(l != null) { out.append(l+"\n"); l = r.readLine }; r.close
      }
    }
    val out=gobble(p.getInputStream); val err=gobble(p.getErrorStream); p.waitFor
    val o=out.toString; val e=err.toString
    if (!o.equals("") || !e.equals("")) println("\nExec: "+cmd+"\n- Out: "+o+"\n- Err: "+e+"\n")
    (o,e)
  }

  // generate and load library
  def gen {
    val f = "comp"+CompileCounter.get
    new File(outPath).mkdirs
    dataMap.foreach { case (ext,data) => val out=new FileWriter(outPath+"/"+f+"."+ext); out.write(replace(data,Map(("file",f)))); out.close }
    dataMap.foreach {
      case("c",_) =>   run("g++ "+ccFlags+" "+f+".c -c -o "+f+"_c.o") // gcc fails to link properly with nvcc object files
      case("cpp",_) => run("g++ "+ccFlags+" "+f+".cpp -c -o "+f+"_cpp.o")
      case("cu",_) =>  run(cudaPath+"/bin/nvcc "+cudaFlags+" "+ccFlags+" "+f+".cu -c -o "+f+"_cu.o")
      case _ =>
    }
    val files = dataMap.map{case(x,_)=>x}.filter(! _.startsWith("h")).map(f+"_"+_+".o").mkString(" ")
    run("g++ "+files+" "+ldFlags+" -o libComp"+CompileCounter.get+".jnilib")
    dataMap.clear
    System.load(new File(outPath+"/libComp"+CompileCounter.get+".jnilib").getCanonicalPath)
    CompileCounter.next
  }
}
