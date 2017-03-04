package edu.colorado.plv.fixr.bash

/**
  * Created by edmund on 3/3/17.
  */

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.sys.process._

case class BashResult[O,E](exitCode:Int, stdout: O, stderr: E) extends Bashable(null) {
  def getExitCode(): Int = exitCode
  def getStdout(): O = stdout
  def getStderr(): E = stderr
  override def toString: String = {
    s"EXITCODE :- $exitCode\n" + {if (stdout.toString.length > 0) s"STDOUT :- $stdout\n" else ""} + {if (stderr.toString.length > 0) s"STDERR :- $stderr\n" else ""}
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

abstract class Bashable(cmd: String) {
  var index = 1
  var resultLogger: Logger = null
  def setIndex(newIndex: Int): Bashable = { index = newIndex ; this }
  def getIndex(): Int = index
  def setLogger(newLogger: Logger): Bashable = { resultLogger = newLogger ; this }
  def logWith(newLogger: Logger): Bashable = setLogger(newLogger)

  def ~ (next: Bashable): Bashable = {
    next.setIndex( getIndex() + 1 )
    val res = run()
    if (res.getExitCode() == 0) {
      next.setLogger(resultLogger)
    } else {
      if (resultLogger != null) { resultLogger.error("Aborting command sequence...") }
      res
    }
  }
  def run(): BashResult[String,String] = {
     if (cmd != null) {
        run(cmd)
     } else {
        null
     }
  }
  def run(cmd: String):BashResult[String,String] = {
    val resultBuilder = new BashResultBuilder[String,String](identity,identity)
    val logger = ProcessLogger(
      (o: String) => resultBuilder.addOutMsg( o ),
      (e: String) => resultBuilder.addErrMsg( e ) )
    val res = resultBuilder.mkResults(cmd ! logger)
    if(resultLogger != null) {
      if (res.getExitCode() == 0) {
        resultLogger.info(s"CMD \'$cmd\' Succeeded:\n$res")
      } else {
        resultLogger.error(s"CMD \'$cmd\' Failed:\n$res")
      }
    }
    res
  }

  def ! (): BashResult[String,String] = {
    val res = run()
    if (resultLogger != null) { resultLogger.info(" Command sequence completed! ") }
    res
  }
}

case class PCmd[O,E](cmd: String) extends Bashable(cmd) {

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

  // override def run(): BashResult[String,String] = run(cmd)

}

case class Cmd(cmd: String) extends Bashable(cmd) {
  // override def run(): BashResult[String, String] = run(cmd)
}

object CmdTest {

  def main(args: Array[String]): Unit = {

    val logger = Logger(LoggerFactory.getLogger("name"))

    Cmd("ls -al").logWith(logger) ~ Cmd("tree /data/callback/repo") ~ Cmd("ls happy") !

    logger.debug("Test it")

  }

}


