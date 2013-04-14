package v4

// Problem-specific and benchmarking helpers

object Utils {
  import scala.util.Random
  def reset = Random.setSeed(123456748299L)
  reset

  // Benchmarking helper, displays median, min and max
  def bench[T](num:Int,gen:()=>T)(name:String,f:T=>Unit) {
    def pt(ms:Long) = "%3d.%03d".format(ms/1000,ms%1000)
    Utils.reset; f(gen()); // warm-up
    val ts = (0 until num).map{_=> val d=gen(); val s=System.currentTimeMillis; f(d); System.currentTimeMillis-s }.sorted
    val median = if (ts.length%2==1) ts(ts.length/2) else (ts(ts.length/2)+ts(ts.length/2-1))/2
    println("%-20s : ".format(name)+" "+pt(median)+"  ["+pt(ts.head)+", "+pt(ts.last)+"]")
  }

  // --------------------------------------------------------------------------
  // Matrix multiplication
  def genMats(n:Int=512) = {
    def rnd:Int = Math.abs(Random.nextInt)%253+1
    val s = Seq.fill(n)(rnd)
    (Seq(rnd) ++ s).zip(s).map{case(x,y)=>(x,y)/*unbox ints!!*/}.toArray
  }

  // --------------------------------------------------------------------------
  // Biosequences matching and folding
  def genDNA(n:Int=512) = Seq.fill(n)(Math.abs(Random.nextInt)%4).map {case 0=>'A' case 1=>'C' case 2=>'G' case 3=>'T'}.toArray
  def genRNA(n:Int=80) = Seq.fill(n)(Math.abs(Random.nextInt)%4).map {case 0=>'a' case 1=>'c' case 2=>'g' case 3=>'u'}.mkString

  // Debugging helper, program must be Vienna/RNAfold-compatible
  def refFold(seq:String,prog:String="./RNAfold --noPS --noLP -d2"):String = {
    import java.io._
    val p = Runtime.getRuntime.exec(prog);
    val in = new PrintStream(p.getOutputStream());
    def gobble(in:InputStream) = new Runnable {
      var out = new StringBuilder
      var thr = new Thread(this); thr.start
      override def toString = { thr.join; out.toString.trim }
      override def run { val r = new BufferedReader(new InputStreamReader(in))
        var l = r.readLine; while(l != null) { out.append(l+"\n"); l = r.readLine }; r.close
      }
    }
    val out=gobble(p.getInputStream);
    in.println(seq); in.close
    p.waitFor; out.toString.split("\n")(1)
  }

  // --------------------------------------------------------------------------
  // --------------------------------------------------------------------------

  // Pretty print multiple answer/backtrack traces
  def printBT[T](bs:List[(T,List[((Int, Int),(Int,List[Int]))])]) = {
    println("Backtrack = {")
    for(b<-bs) { print("  "+b._1+"   BT =")
      for (((i,j),(r,bt)) <- b._2) { print(" ["+i+","+j+"]="+r+","+bt+" ") }; println
    }
    println("}")
  }

  // --------------------------------------------------------------------------

  def runBenchmark(fc:Int=>Unit,fs:Int=>Unit,from:Int=1) {
    val sz=List(64,96,128,192,256,384,512,768,1024,1536,2048,3072,4096,6144,8192) //List(128,256,512,1024,2048,4096,8192)
    println("====( CUDA warm-up )==============================================")
    for (i<-0 until 2) fc(1024); reset
    println("====( CUDA )======================================================")
    for (s<-sz) if (from<=s) {
      println("---- Size = "+s+" ----------------")
      for (i<-0 until (if (s>2048) 6 else 10)) fc(s)
    }
    println("====( Scala warm-up )=============================================")
    fs(512); reset
    println("====( Scala )=====================================================")
    for (s<-sz) if (from<=s) {
      println("---- Size = "+s+" ----------------")
      for (i<-0 until (if (s>2048) 4 else 10)) fs(s)
    }
  }
}
