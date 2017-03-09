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

  def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = {
    val emulatorSDFile = s"$emulatorSDPath/sdcard.img"
    val file = GenFile.genNewFile() // new File("tmp")
    val out = for {
      p0 <- CreateDir(emulatorSDPath, true) ! ;
      p1 <- Cmd(s"mksdcard -l e 512M $emulatorSDFile") ! ;
      p2 <- Emulator.sdCard(emulatorSDFile).name(deviceName,devicePort).noWindow(noWindow) #>> file & ;
      p3 <- repeat (Cmd(s"cat ${file.getPath}")) until {
        case SuccTry(Succ(c, o, e)) => o contains "emulator: Serial number of this emulator (for ADB):"
        case default => false
      } ;
      p4 <- Lift ! file.delete() ;
      p5 <- Lift ! p3.stdout.split("\n").filter(_ contains "emulator: Serial number of this emulator (for ADB):")(0).split(":").last.trim() ;
      p6 <- Adb.target(p5.stdout).waitForDevice() ! ;
      p7 <- repeat (Adb.shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -l")) until {
        case SuccTry(Succ(c, o, e)) => o.trim() == "0"
        case default => false
      } ;
      p8 <- Lift ! p5.stdout
    } yield p8

    out match {
      case SuccTry(Succ(c, o, e))     => SuccTry(Succ("<StartEmulator>", o, e))
      case FailTry(Fail(c, ec, o, e)) => FailTry(Fail("<StartEmulator>", ec, o, "Start Emulator Failed"))
    }
  }
   /*
   def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = {
      val emulatorSDFile = s"$emulatorSDPath/sdcard.img"
      val file = GenFile.genNewFile() // new File("tmp")
      val out = for {
        p0 <- CreateDir(emulatorSDPath, true) ! ;
        p1 <- Cmd(s"mksdcard -l e 512M $emulatorSDFile") ! ;
        p2 <- Emulator.sdCard(emulatorSDFile).name(deviceName,devicePort).noWindow(noWindow) #>> file &
      } yield p2

      out match {
        case SuccTry(Succ(c, o, e)) => {
           var id = ""
           var cont = true
           while(cont) {
             if( file.exists() ) {
               val idOutput = Source.fromFile(file).getLines().filter(
                 _ startsWith "emulator: Serial number of this emulator (for ADB):"
               )
               if (idOutput.hasNext) {
                 id = idOutput.next.split(":").last.trim
                 cont = false
               } else {
                 bashLogger.debug("waiting for device to report in id...")
                 Thread.sleep(1000)
               }
             } else {
               bashLogger.debug("waiting for device to report in id...")
               Thread.sleep(1000)
             }
           }
           file.delete()
           for {
             p0 <- Adb.target(id).waitForDevice() ! ;
             p1 <- repeat (Adb.shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -l")) until {
               _ match {
                 case SuccTry(Succ(c, o, e)) => o.trim() == "0"
                 case default => false
               }
             }
           } yield p1
           SuccTry(Succ(c, id, e))
        }
        case FailTry(Fail(c, ec, o, e)) => {
           FailTry(Fail(c, ec, o, "Start emulator failed.. aborting"))
        }
      }

   } */

  /*
   def !!! (implicit bashLogger: Logger): TryM[String,Fail] = {
      this ! match {
        case SuccTry(Succ(c, o, e))     => SuccTry(o)
        case FailTry(Fail(c, ec, o, e)) => FailTry(Fail(c, ec, o, e))
      }
   } */

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

