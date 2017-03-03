package edu.colorado.plv.fixr.bash

/**
  * Created by edmund on 3/3/17.
  */

import scala.sys.process._

case class BashResult[O,E](exitCode:Int, stdout: O, stderr: E) extends Bashable {
  def getExitCode(): Int = exitCode
  def getStdout(): O = stdout
  def getStderr(): E = stderr
  override def toString: String = {
    s"EXITCODE :- $exitCode\nSTDOUT :- $stdout\nSTDERR :- $stderr\n"
  }
  override def run(): BashResult[String, String] = BashResult(exitCode, stdout.toString, stderr.toString)
  override def ~(next: Bashable): Bashable = next
}

class BashResultBuilder[O,E](outParser: String => O, errParser: String => E) {

  val out  = new StringBuilder
  val err  = new StringBuilder

  def getSep(): String = "##<<@@>>##"

  def addOutMsg(o: String) = out.append( o + getSep() )
  def addErrMsg(e: String) = err.append( e + getSep() )

  def formatStdout(): O = outParser( out.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n") )
  def formatStderr(): E = errParser( err.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n") )

  def mkResults(exitcode: Int): BashResult[O,E] = BashResult[O,E](exitcode, formatStdout(), formatStderr())

}

class StdResultBuilder extends BashResultBuilder[String,String](identity,identity)

object BashLogger {

  def mkBashLogger[O,E](outParser: String => O, errParser: String => E): (ProcessLogger,BashResultBuilder[O,E]) = {
    val resultBuilder = new BashResultBuilder[O,E](outParser,errParser)

    val logger = ProcessLogger(
      (o: String) => resultBuilder.addOutMsg( o ),
      (e: String) => resultBuilder.addErrMsg( e ) )
    return (logger,resultBuilder)
  }

  def mkBashLogger(): (ProcessLogger,BashResultBuilder[String,String]) = {
    mkBashLogger[String,String](identity,identity)
  }

}

object Bash {

  def runCmd(cmd: String): BashResult[String,String] = {
    val (logger,outcome) = BashLogger.mkBashLogger()
    val res = cmd ! logger
    outcome.mkResults(res)
  }

  def main(args: Array[String]): Unit = {
    val results = runCmd("ls /home/edmund -al")
    println(results)

    val results2 = runCmd("find /home/edmund/workshops/git_workshop")
    println(results2)
  }

}

abstract class Bashable {
  var index = 1
  def setIndex(newIndex: Int): Unit = { index = newIndex }
  def getIndex(): Int = index
  def ~ (next: Bashable): Bashable = {
    next.setIndex( getIndex() + 1 )
    val res = run()
    if (res.getExitCode() == 0) {
      next
    } else {
      res
    }
  }
  def run(): BashResult[String,String]
  def run(cmd: String):BashResult[String,String] = {
    val resultBuilder = new BashResultBuilder[String,String](identity,identity)
    val logger = ProcessLogger(
      (o: String) => resultBuilder.addOutMsg( o ),
      (e: String) => resultBuilder.addErrMsg( e ) )
    val res = resultBuilder.mkResults(cmd ! logger)
    if (res.getExitCode() == 0) {
      print(s"CMD $index Succeeded:\n$res")
    } else {
      print(s"CMD $index Failed:\n$res")
    }
    res
  }

  def ! (): BashResult[String,String] = {
    val res = run()
    print(s"Done!")
    res
  }
}

case class PCmd[O,E](cmd: String) extends Bashable {

  var outParser: String => O = null
  var errParser: String => E = null

  def setOut(newParser: String => O): Unit = { outParser = newParser }

  def setErr(newParser: String => E): Unit = { errParser = newParser }


  def out[ON](outParser: String => ON): PCmd[ON,E] = {
     val newCmd = new PCmd[ON,E](cmd)
     newCmd.setOut(outParser)
     return newCmd
  }

  def err[EN](errParser: String => EN): PCmd[O,EN] = {
     val newCmd = new PCmd[O,EN](cmd)
     newCmd.setErr(errParser)
     return newCmd
  }

  def run(outParser: String => O, errParser: String => E): BashResult[O,E] = {
    val resultBuilder = new BashResultBuilder[O,E](outParser,errParser)
    val logger = ProcessLogger(
      (o: String) => resultBuilder.addOutMsg( o ),
      (e: String) => resultBuilder.addErrMsg( e ) )
    resultBuilder.mkResults(cmd ! logger)
  }

  override def run(): BashResult[String,String] = run(cmd)

}

case class Cmd(cmd: String) extends Bashable {
  override def run(): BashResult[String, String] = run(cmd)
}

object CmdTest {

  def main(args: Array[String]): Unit = {

    Cmd("ls -al") ~ Cmd("tree /data/callback/repo") ~ Cmd("ls happy") !

  }

}


