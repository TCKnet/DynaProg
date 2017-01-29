name := "DynaProg"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scala-lang" % "scala-compiler" % "2.12.1"
)

scalacOptions ++= List("-target:jvm-1.8", "-opt:l:classpath", "-deprecation", "-feature", "-unchecked")

scalaSource in Compile := baseDirectory(_ / "src").value

compile in Compile ~= { x => ("src/librna/make target/scala-2.12/classes").run.exitValue; x }

def tt(n:String, cls:String) = TaskKey[Unit](n) := {
  (compile in Compile).value
  val base = baseDirectory.value
  val cp = (managedClasspath in Compile map { cp => Path.makeString(cp.files)}).value
  ("java -Xss64m -cp "+base+"/target/scala-2.12/classes:"+base+"/bin:"+cp+" v4.examples."+cls) !
}
tt("mm", "MatrixMult"); tt("mm2", "MatrixMult2"); tt("mm3", "MatrixMult3")
tt("zuker", "Zuker"); tt("z2", "Zuker2"); tt("rnafold", "RNAFold")
tt("sa", "SeqAlign"); tt("nu", "Nussinov"); tt("swat", "SWatAffine")


