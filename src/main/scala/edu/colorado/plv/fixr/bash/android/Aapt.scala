package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.{Fail, Lift, Pipeable}
import edu.colorado.plv.fixr.bash.utils.{FailTry, SuccTry, TryM}
import org.slf4j.LoggerFactory

/**
  * Created by edmund on 3/9/17.
  */
object Aapt extends Aapt("aapt") {

  def home(path:String): Aapt = Aapt(s"${path}/$cmd")

  def parse(stdout: String): TryM[ApkInfo,Fail] = {
     try {
       val ls = stdout.split("\n").filter(p => (p startsWith "package:") || (p startsWith "launchable-activity:")).map {
         _.split(" ").filter(_ contains "name=")(0).split("=")(1).stripPrefix("\'").stripSuffix("\'")
       }
       return SuccTry(ApkInfo(ls(0), if (ls.length>1) Some(ls(1)) else None))
     } catch {
       case e: Exception => return FailTry(Fail("<Parse ApkInfo>", 2, "", e.toString))
     }
  }

}

case class ApkInfo(packageName:String, activityName:Option[String]) {

}

case class Aapt(cmd: String) extends Pipeable {

  def extend(raw: String): Aapt = Aapt(s"$cmd $raw")

  override def command(): String = cmd

  def apkInfo(apkPath:String): Aapt = extend(s"dump badging $apkPath")

}

object TestApkInfo {

  def main(args: Array[String]): Unit = {

    implicit val logger = Logger(LoggerFactory.getLogger("aapt-tester"))

    for {
      stdout  <- Aapt.home("/usr/local/android-sdk/build-tools/24.0.3").apkInfo("/data/callback/repo/Kistenstapeln-Android-bug/app-debug.apk") !!! ;
      apkInfo <- Aapt.parse(stdout) ;
      unit    <- Lift ! println(s"$apkInfo")
    } yield apkInfo

  }

}