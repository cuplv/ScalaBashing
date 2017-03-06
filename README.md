# ScalaBashing
A higher level library for shell scripting in Scala, built on top of standard sys.process. Adds the following to the standard shell interface:

  * Logger integration with shell scripts
  * Interface for building high-level structure for shell tools (adb, emulator, etc..)
  * Monadic 'Try' sequencing for shell command (replaces '&&')
