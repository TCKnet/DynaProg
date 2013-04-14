package librna

object LibRNA {
  private def load(pl:List[String]):Unit = pl match {
    case p::ps => val f = new java.io.File(p+"/libRNA.jnilib")
      if (f.exists) System.load(f.getCanonicalPath) else load(ps)
    case Nil => throw new Exception("JNI libRNA not found")
  }
  load((this.getClass.getClassLoader match {
    case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath)
    case _ => System.getProperty("java.class.path").split(":")
  }).reverse.toList)

  @native def setParams(file:String):Unit
  @native def setSequence(seq:String):Unit
  @native def clear:Unit
  @native def getConsts:String // defines for recompilation

  // Note that 'Byte' is mapping 'base_t'
  @native def termau_energy(i:Int, j:Int):Int
  @native def hl_energy(i:Int, j:Int):Int // <-- hairpin loop
  @native def hl_energy_stem(i:Int, j:Int):Int
  @native def il_energy(i:Int, k:Int, l:Int, j:Int):Int // <-- internal loop
  @native def bl_energy(i:Int, k:Int, l:Int, j:Int, Xright:Int):Int
  @native def br_energy(i:Int, k:Int, l:Int, j:Int, Xleft:Int):Int
  @native def sr_energy(i:Int, j:Int):Int // <-- 2 stacked base pairs
  @native def sr_pk_energy(a:Byte, b:Byte, c:Byte, d:Byte):Int
  @native def dl_energy(i:Int, j:Int):Int
  @native def dr_energy(i:Int, j:Int):Int
  @native def dli_energy(i:Int, j:Int):Int
  @native def dri_energy(i:Int, j:Int):Int
  @native def ext_mismatch_energy(i:Int, j:Int):Int
  @native def ml_mismatch_energy(i:Int, j:Int):Int
  @native def ml_energy:Int
  @native def ul_energy:Int
  @native def sbase_energy:Int
  @native def ss_energy(i:Int, j:Int):Int
  @native def dl_dangle_dg(dangle:Byte, i:Byte, j:Byte):Int
  @native def dr_dangle_dg(i:Byte, j:Byte, dangle:Byte):Int

  @native def mk_pf(x:Double):Double
  @native def scale(x:Int):Double;
  @native def iupac_match(base:Byte, iupac_base:Byte):Boolean;
}

/*
object Test extends App {
  import LibRNA._
  //setParams("vienna/rna_turner2004.par")
  setParams("src/librna/vienna/rna_turner2004.par")
  setSequence("acguacguacgu")
  val test = sr_energy(0,5)==130 && il_energy(0,2,4,6)==190
  println("LibRNA: "+(if (test) "ok." else "ERROR."));
  clear
}
*/
