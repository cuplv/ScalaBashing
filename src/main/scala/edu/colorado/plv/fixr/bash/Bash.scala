package edu.colorado.plv.fixr.bash

/**
  * Created by edmund on 3/3/17.
  */

import scala.sys.process._

case class BashStdout(output: String) {
  override def toString: String = s"Success: $output"
}

case class BashStderr(errMsg: String) {
  override def toString: String = s"Failure: $errMsg"
}

case class BashResults(exitCode: Int, stdout: String, stderr: String) {

  override def toString: String = {
     s"EXITCODE :- $exitCode\nSTDOUT :- $stdout\nSTDERR :- $stderr"
  }

}

class BashResultBuilder() {

  val out  = new StringBuilder
  val err  = new StringBuilder

  def getSep(): String = "##<<@@>>##"

  def addOutMsg(o: String) = out.append( o + getSep() )
  def addErrMsg(e: String) = err.append( e + getSep() )

  def formatStdout(): String = out.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n")
  def formatStderr(): String = err.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n")

  def mkResults(exitcode: Int): BashResults = BashResults(exitcode, formatStdout(), formatStderr())

}

object BashLogger {

  def mkBashLogger(): (ProcessLogger,BashResultBuilder) = {
    val outcomeBuilder = new BashResultBuilder

    val logger = ProcessLogger(
       (o: String) => outcomeBuilder.addOutMsg( o ),
       (e: String) => outcomeBuilder.addErrMsg( e ) )
    return (logger,outcomeBuilder)
  }

}

object Bash {

  def runCmd(cmd: String): BashResults = {
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