package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash._
import edu.colorado.plv.fixr.bash.utils.TryM
import org.slf4j.LoggerFactory

/**
  * Created by edmund on 3/7/17.
  */

object Adb extends Adb("adb")

case class Adb(cmd: String) extends Pipeable {

  override def ? (implicit bashLogger: Logger): TryM[Succ,Fail] = Check(Seq("adb")) !

  def extend(raw: String) = Adb(s"$cmd $raw")

  def target(deviceName: String) = extend(s"-s $deviceName")

  def waitForDevice(): Adb = extend("wait-for-device")

  def shell(shcmd: String): Adb = extend(s"shell $shcmd")

  def push(src:String, dest:String): Adb = extend(s"push $src $dest")

  def uninstall(packageName: String): Adb = extend(s"uninstall $packageName")

  def install(apkPath: String): Adb = extend(s"install $apkPath")

  override def command(): String = cmd

  def kill (implicit bashLogger: Logger): Adb = extend("emu kill")

  // Custom Shell commands

  def shellPsGrep(grepStr: String) (implicit bashLogger: Logger): TryM[Array[String],Fail] = {
    val shellPs = this.shell("ps")
    for {
      grepOut <- shellPs #| Cmd(s"grep $grepStr") !!!
    } yield grepOut.split("\n")
  }

  def getForeGroundAppName(implicit bashLogger: Logger): TryM[String,Fail] = {
    val shellCmd = this.shell("dumpsys window windows")
    for {
      grepOut <- shellCmd #| Cmd("grep mCurrentFocus") !!!
    } yield grepOut.split("/")(0).split(" ").last
  }

}

object TestCustomShellCmds {

  def main(args: Array[String]): Unit = {
    implicit val logger = Logger(LoggerFactory.getLogger("grep-out-tester"))

    val out1 = for {
      p1 <- Adb.target("emulator-5554").shellPsGrep("com.peilunzhang.contractiontimerdistilled")
      p2 <- Lift !!! p1.length
    } yield p2
    println(out1)

    val out2 = for {
      p1 <- Adb.target("emulator-5554").getForeGroundAppName
    } yield p1
    println(out2)

  }

}