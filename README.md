# DynaProg
### A Scala DSL for Dynamic Programming

This is a minimal subset of DynaProg to enjoy our Scala+ADP DSL without hassle.

The full project repository can be found [here](https://github.com/manojo/lamp-dp-mt)

### Setup instructions

1. Install [SBT](http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html) and add it to your PATH

2. Patch (with provided patch) and publish locally [LMS](https://github.com/tiarkrompf/virtualization-lms-core)

        git clone https://github.com/TiarkRompf/virtualization-lms-core.git
        cd virtualization-lms-core
        git checkout develop
        patch -p0 < ..../lms-patch.diff
        sbt publish-local

<!--
3. Clone [DynaProg](https://github.com/TCKnet/DynaProg.git) repository

        git clone https://github.com/TCKnet/DynaProg.git
        cd DynaProg
        sbt
-->

4. From there you can run the following example programs
    * `mm`, `mm2`, `mm3` : Matrix chain multiplication
    * `swat`, `align` : Sequence alignement
    * `zuker`, `z2`, `rnafold` : Zuker RNA folding
    * `nu` : Nussinov78

You can also use regular SBT commands (`clean`,`compile`,`run`,`run-main <class>`, ...).

You need to use the specific commands above due to issues with multiple JNI library loading wihtin the same JVM.

Also you might need to update the configuration of the `CodeCompiler` at `src/v4/CodeGen.scala:520` to match you environment.

Enjoy.
