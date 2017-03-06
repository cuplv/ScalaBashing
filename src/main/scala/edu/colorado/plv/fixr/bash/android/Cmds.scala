package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.{CreateDir, TryM}
import edu.colorado.plv.fixr.bash.{Cmd, Fail, Pipeable, Succ}
import org.slf4j.LoggerFactory

/**
  * Created by edmund on 3/5/17.
  */

object Emulator extends Emulator("emulator")

case class Emulator(cmd: String) extends Pipeable {

  def extend(raw: String) = Emulator(s"$cmd $raw")

  def sdCard(sdCardPath: String): Emulator = extend(s"-sdcard $sdCardPath")

  def name(devName:String, portOpt:Option[Int]): Emulator =
    portOpt match {
      case Some(port) => extend(s"-port $port @$devName")
      case None => extend(s"-avd $devName")
    }

  def noWindow(): Emulator = extend(s"-no-window")

  def noWindow(yes: Boolean): Emulator = if (yes) noWindow() else this

  // def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = Cmd(cmd) !

  override def command(): String = cmd

}

object Adb extends Adb("adb")

case class Adb(cmd: String) extends Pipeable {

  def extend(raw: String) = Adb(s"$cmd $raw")

  def waitForDevice(): Adb = extend("wait-for-device")

  def shell(shcmd: String): Adb = extend(s"shell $shcmd")

  // def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = Cmd(cmd) !

  override def command(): String = cmd

}


object TestEmu {

  def main(args: Array[String]): Unit = {


    implicit val logger = Logger(LoggerFactory.getLogger("name"))

     val sdCardPath = "/data/sd-store"
     val sdCardFile = sdCardPath + "/sdcard.img"

     for {
       p0 <- CreateDir(sdCardPath, true) ! ;
       p1 <- Cmd(s"mksdcard -l e 512M $sdCardFile") ! ;
       p2 <- Emulator.sdCard(sdCardFile).name("pokemon-x86", None).noWindow(false) ! ;
       p3 <- Adb.waitForDevice() ! ;
       p4 <- Adb.shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -1") !
     } yield p1

  }

}



