package edu.colorado.plv.fixr.bash

/**
  * Created by edmund on 3/3/17.
  */

import scala.sys.process._

case class BashResult[O,E](exitCode:Int, stdout: O, stderr: E) {
  def getExitCode(): Int = exitCode
  def getStdout(): O = stdout
  def getStderr(): E = stderr
  override def toString: String = {
    s"EXITCODE :- $exitCode\nSTDOUT :- $stdout\nSTDERR :- $stderr"
  }
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



