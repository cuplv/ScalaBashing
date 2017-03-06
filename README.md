# ScalaBashing
A higher level library for shell scripting in Scala, built on top of standard sys.process. Adds the following to the standard shell interface:

  * Logger integration with shell scripts
  * Interface for building high-level structure for shell tools (adb, emulator, etc..)
  * Monadic 'Try' sequencing for shell command (replaces '&&')

# Usage
The easiest way to use this library is to clone this git repo and publish it locally in your dev environment with sbt:

```
sbt publishLocal
```

Then, you can include this library in your own sbt projects by adding the following dependency:

```
libraryDependencies += "edu.colorado.plv.fixr" %% "scalabashing" % "1.0-SNAPSHOT"
```
