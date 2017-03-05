package edu.colorado.plv.fixr.bash

import java.io.IOException

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.{FailTry, SuccTry, TryM}
import org.slf4j.LoggerFactory

import scala.sys.process._
import scala.sys.process.ProcessLogger

/**
  * Created by edmund on 3/5/17.
  */
abstract class BashResult(cmd:String, exitcode: Int, stdout:String, stderr:String) {
  override def toString: String = {
     s"Command: $cmd\n" +
     s"Exitcode: $exitcode\n" +
     {if (stdout.length > 0) s"Stdout: $stdout\n" else ""} +
     {if (stderr.length > 0) s"Stderr: $stderr\n" else ""}
  }
}

case class Succ(cmd:String, stdout:String, stderr:String) extends BashResult(cmd, 0, stdout, stderr)

case class Fail(cmd:String, exitcode: Int, stdout:String, stderr:String) extends BashResult(cmd, exitcode, stdout, stderr)

abstract class Bash {

  def ! (implicit bashLogger: Logger): TryM[BashResult,BashResult]

}

class BashResultsBuilder {

  val out  = new StringBuilder
  val err  = new StringBuilder

  def getSep(): String = "##<<@@>>##"

  def addOutMsg(o: String) = out.append( o + getSep() )
  def addErrMsg(e: String) = err.append( e + getSep() )

  def formatStdout(): String = out.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n")
  def formatStderr(): String = err.split(getSep().toCharArray).filter( _.length!=0 ).mkString("\n")

  def mkResults(cmd: String, exitcode: Int): BashResult = exitcode match {
    case 0 => Succ(cmd, formatStdout(), formatStderr())
    case default => Fail(cmd, exitcode, formatStdout(), formatStderr())
  }

}

case class Cmd(cmd: String) extends Bash {

  override def ! (implicit bashLogger: Logger): TryM[BashResult,BashResult] = {
    bashLogger.debug(s"Attempting to run command \'$cmd\'")
    val resultBuilder = new BashResultsBuilder
    val logger = ProcessLogger(
      (o: String) => resultBuilder.addOutMsg( o ),
      (e: String) => resultBuilder.addErrMsg( e ) )
    try {
      val res = resultBuilder.mkResults(cmd, cmd ! logger)
      bashLogger.debug(s"Ran command, outcome:\n$res")
      res match {
        case Succ(c, o, e) => SuccTry(res)
        case default => FailTry(res)
      }
    } catch {
      case e: IOException => {
        bashLogger.debug(s"IO Exception encountered: ${e.toString}")
        FailTry( Fail(cmd, 2, "", s"IO Exception encountered: ${e.toString}") )
      }
    }
  }

  def arg(raw: String): Cmd = Cmd(s"$cmd $raw")

  def param(key:String, value:String): Cmd = Cmd(s"$cmd -$key $value")

}

case class Check(tools: Seq[String]) extends Bash {

  override def ! (implicit bashLogger: Logger): TryM[BashResult,BashResult] = {
    bashLogger.debug(s"Checking tool requirements ${tools.mkString(",")}")
    val results = tools.map( s => Cmd(s"which $s") ! ).filter( _.isSucc() )

    if (results.length == tools.length) {
       bashLogger.debug(s"All tools available (${tools.mkString(",")})")
       SuccTry(Succ(tools.mkString(","),"",""))
    } else {
       bashLogger.debug(s"Not all tools are available.")
       FailTry(Fail(s"which <${tools.mkString(",")}>", 2, "", s"Not all tools are available."))
    }
  }

}

object Tester {

  def main(args: Array[String]): Unit = {

     implicit val logger = Logger(LoggerFactory.getLogger("name"))

     try {
       val g = for {
         u <- Check(Seq("ls", "adb", "ls")) !;
         i <- Cmd("lsd -al") !;
         j <- Cmd("which adb") !;
         k <- Cmd("lsd") !
       } yield k

       println(g)
     } catch {
       case e: Exception => println(s"Uncaught Exception $e")
     }

  }

}