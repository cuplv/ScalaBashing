package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.{CreateDir, TryM}
import edu.colorado.plv.fixr.bash.{Cmd, Fail, Succ}
import org.slf4j.LoggerFactory

/**
  * Created by edmund on 3/5/17.
  */

object Emulator  {

  val emulator: String = "emulator"

  def init(): Emulator = Emulator(s"$emulator")

  def sdCard(sdCardPath: String): Emulator = init().sdCard(sdCardPath)
  def name(devName:String, portOpt:Option[Int]): Emulator = init().name(devName, portOpt)
  def noWindow(): Emulator = init().noWindow()
  def noWindow(yes: Boolean): Emulator = init().noWindow(yes)

}

case class Emulator(cmd: String) {

  def extend(raw: String) = Emulator(s"$cmd $raw")

  def sdCard(sdCardPath: String): Emulator = extend(s"-sdcard $sdCardPath")

  def name(devName:String, portOpt:Option[Int]): Emulator =
    portOpt match {
      case Some(port) => extend(s"-port $port @$devName")
      case None => extend(s"-avd $devName")
    }

  def noWindow(): Emulator = extend(s"-no-window")

  def noWindow(yes: Boolean): Emulator = if (yes) noWindow() else this

  def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = Cmd(cmd) !

}

object TestEmu {

  def main(args: Array[String]): Unit = {


    implicit val logger = Logger(LoggerFactory.getLogger("name"))

     val sdCardPath = "/data/sd-store"
     val sdCardFile = sdCardPath + "/sdcard.img"

     for {
       p0 <- CreateDir(sdCardPath, true) ! ;
       p1 <- Cmd(s"mksdcard -l e 512M $sdCardFile") ! ;
       p2 <- Emulator.sdCard(sdCardFile).name("pokemon-x86", None).noWindow(false) !
     } yield p1

  }

}



