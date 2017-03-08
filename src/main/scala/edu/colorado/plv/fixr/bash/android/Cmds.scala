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
      val file = new File("tmp")
      val out = for {
        p0 <- CreateDir(emulatorSDPath, true) ! ;
        p1 <- Cmd(s"mksdcard -l e 512M $emulatorSDFile") ! ;
        p2 <- Emulator.sdCard(emulatorSDFile).name("pokemon-x86",None).noWindow(false) #>> file &
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
           for {
             p0 <- Adb.target(id).waitForDevice() ! ;
             p1 <- RepeatUntil(Adb.shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -l")).satisfied(Conditions.succMatches("0")) !
           } yield p1
           SuccTry(Succ(c, id, o))
        }
        case FailTry(Fail(c, ec, o, e)) => {
           FailTry(Fail(c, ec, o, "Start emulator failed.. aborting"))
        }
      }

   }

}

object TestEmu {

  def main(args: Array[String]): Unit = {

     implicit val logger = Logger(LoggerFactory.getLogger("emu-tester"))

     implicit val ec = ExecutionContext.global

     val sdCardPath = "/data/sd-store"

     val p0 = for {
        p0 <- StartEmulator("pokemon-x86", "/data/sd-store", None, false) !
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

