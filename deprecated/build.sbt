name := "DynaProg"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scala-lang.virtualized" % "scala-library" % "2.10.0",
  "org.scala-lang.virtualized" % "scala-compiler" % "2.10.0",
  "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT"
)

scalacOptions ++= List("-Yvirtualize", "-target:jvm-1.7", "-optimise", "-deprecation", "-feature", "-unchecked")

scalaSource in Compile <<= baseDirectory(_ / "src")

compile in Compile <<= (compile in Compile) map { x => ("src/librna/make target/scala-2.10/classes").run.exitValue; x }

{
  def t(n:String) = { val t=TaskKey[Unit](n); t.dependsOn(compile in Compile); t }
  def s(t:TaskKey[Unit],cl:String) = Seq(fullRunTask(t in Test, Test, cl), fork in t:=true,javaOptions in t++=List("-Xss64m"))
  val (mm,mm2,mm3,align,zuker,z2,rnafold,nu,swat)=(t("mm"),t("mm2"),t("mm3"),t("align"),t("zuker"),t("z2"),t("rnafold"),t("nu"),t("swat")) // Examples
  val lt1=t("lt1") // LMS testing
  val ex="v4.examples."
  s(mm,ex+"MatrixMult") ++ s(mm2,ex+"MatrixMult2") ++ s(mm3,ex+"MatrixMult3") ++ s(align,ex+"SeqAlign") ++
  s(zuker,ex+"Zuker") ++ s(z2,ex+"Zuker2") ++ s(rnafold,ex+"RNAFold") ++ s(nu,ex+"Nussinov") ++ s(swat,ex+"SWatAffine") ++
  s(lt1,ex+"TestLMS")
}
