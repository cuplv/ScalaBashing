package edu.colorado.plv.fixr.bash.android

import java.io.File

import scala.io.Source
import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils._
import edu.colorado.plv.fixr.bash._
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

/**
  * Created by edmund on 3/5/17.
  */

case class StartEmulator(deviceName:String, emulatorSDPath:String, devicePort:Option[Int], noWindow:Boolean) (implicit ec: ExecutionContext) extends Bash {

  override def ? (implicit bashLogger: Logger): TryM[Succ,Fail] = Check(Seq("adb","emulator","mksdcard")) !

  override def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = {
    val emulatorSDFile = s"$emulatorSDPath/sdcard.img"
    val file = GenFile.genNewFile() // new File("tmp")
    for {
      // Create SD Card for emulator
      p0 <- CreateDir(emulatorSDPath, true) ! ;
      p1 <- Cmd(s"mksdcard -l e 512M $emulatorSDFile") ! ;

      // Start the emulator, pipe stdout to tmp file 'file' and fork a seperate process
      p2 <- Emulator.sdCard(emulatorSDFile).name(deviceName,devicePort).noWindow(noWindow) #>> file & ;

      // Repeatedly poll 'file' until serial number is visible, after which delete the tmp file and retrieve the emulator serial number
      p3 <- repeat (Cmd(s"cat ${file.getPath}")) until {
        case SuccTry(Succ(c, o, e)) => o contains "emulator: Serial number of this emulator (for ADB):"
        case default => false
      } ;
      p4 <- Lift ! file.delete() ;
      emuID <- Lift !!! p3.stdout.split("\n").filter(_ contains "emulator: Serial number of this emulator (for ADB):")(0).split(":").last.trim() ;

      // Wait for device until it has started
      p6 <- Adb.target(emuID).waitForDevice() ! ;

      // Repeatedly poll the device's 'ps' list and wait until bootanimation has terminated
      p7 <- repeat (Adb.target(emuID).shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -l")) until {
        case SuccTry(Succ(c, o, e)) => o.trim() == "0"
        case default => false
      }
    } yield Succ("<StartEmulator>", emuID, "")
  }

}

object TestEmu {

  def main(args: Array[String]): Unit = {

     implicit val logger = Logger(LoggerFactory.getLogger("emu-tester"))

     implicit val ec = ExecutionContext.global

     val sdCardPath = "/data/sd-store"

     val p0 = for {
        p0 <- StartEmulator("pokemon-x86", "/data/sd-store", None, false) !!! ;
        p1 <- Lift ! println(s"Lifted: $p0")
     } yield p0

     println(s"Here! $p0")

  }

}

object TestWC {

  def main(args: Array[String]): Unit = {
     implicit val logger = Logger(LoggerFactory.getLogger("emu-tester"))
     for {
        p0 <- Cmd("grep crappy README.md") #| Cmd("wc -l") !
     } yield p0
  }

}

