package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils._
import edu.colorado.plv.fixr.bash._
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by edmund on 3/7/17.
  **/

object Emulator extends Emulator("emulator") {

  def quickStart(avdName:String, portOpt:Option[Int], sdPath:String, display:Boolean)
                (implicit bashLogger: Logger, ec: ExecutionContext): TryM[String,Fail] = {
     Emulator.name(avdName, portOpt).sdCard(sdPath).noWindow(!display).start !!!
  }

  def quickCreateAndStart(avdName:String, portOpt:Option[Int], sdPath:String, display:Boolean)
                         (implicit bashLogger: Logger, ec: ExecutionContext): TryM[String,Fail] = {
    for {
      pz <- doTry (Android.deleteAVD(avdName)) !;
      p0 <- Cmd("echo no") #| Android.createAVD(avdName, true).x86.api23 !;
      emuID <- quickStart(avdName, portOpt, sdPath, display);
      p1 <- Lift ! bashLogger.info(s"Started emulator: $emuID")
    } yield emuID
  }

  def stop(emuID:String) (implicit bashLogger: Logger): TryM[Succ,Fail] = {
    Adb.target(emuID).kill !
  }

}

case class Emulator(cmd: String) extends Pipeable {

  var sdPathOpt: Option[String] = None
  var portOpt: Option[Int] = None

  override def ? (implicit bashLogger: Logger): TryM[Succ,Fail] = Check(Seq("emulator")) !

  def extend(raw: String): Emulator = {
    val emu = Emulator(s"$cmd $raw")
    emu.sdPathOpt = sdPathOpt
    emu.portOpt = portOpt
    return emu
  }

  def sdCard(sdCardPath: String): Emulator = { sdPathOpt = Some(sdCardPath) ; extend(s"-sdcard $sdCardPath/sdcard.img") }

  def name(devName:String, pOpt:Option[Int]): Emulator = {
    portOpt = pOpt
    pOpt match {
      case Some(port) => extend(s"-port $port @$devName")
      case None => extend(s"-avd $devName")
    }
  }

  def noWindow(): Emulator = extend(s"-no-window")

  def noWindow(yes: Boolean): Emulator = if (yes) noWindow() else this

  // def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = Cmd(cmd) !

  override def command(): String = cmd

  def start(implicit bashLogger: Logger, ex:ExecutionContext): Bash = {
     Thunk.make {
       this.run match {
         case SuccTry(emuID) => SuccTry(Succ("<Thunk>",emuID,""))
         case FailTry(Fail(c,ec,o,e)) => FailTry(Fail(c,ec,o,e))
       }
     }
  }

  def run (implicit bashLogger: Logger, ec: ExecutionContext): TryM[String,Fail] = {

    // Handle SD card selection
    sdPathOpt match {
      case Some(emulatorSDPath) => {
        // Create SD Card for emulator
        val emulatorSDFile = s"$emulatorSDPath/sdcard.img"
        val out = for {
          p0 <- CreateDir(emulatorSDPath, true) ! ;
          p1 <- Cmd(s"mksdcard -l e 512M $emulatorSDFile") !
        } yield p1
        out match {
          case FailTry(Fail(c, ec, o, e)) => return FailTry(Fail(c, ec, o, e))
          case SuccTry(_) =>
        }
      }
      case None => bashLogger.debug("omitting SD card creation")
    }

    // Handle emulator serial number
    val emuID = portOpt match {
      case Some(port) => {
        for {
          p2 <- Cmd(cmd) &
        } yield p2
        s"emulator-$port"
      }
      case None => {
        val file = GenFile.genNewFile()
        val emuComp = for {
        // Start the emulator, pipe stdout to tmp file 'file' and fork a seperate process
          p2 <- Cmd(cmd) #>> file &;

          // Repeatedly poll 'file' until serial number is visible, after which delete the tmp file and retrieve the emulator serial number
          p3 <- repeat(Cmd(s"cat ${file.getPath}")) until {
            case SuccTry(Succ(c, o, e)) => o contains "emulator: Serial number of this emulator (for ADB):"
            case default => false
          };
          p4 <- Lift ! file.delete();
          emuID <- Lift !!! p3.stdout.split("\n").filter(_ contains "emulator: Serial number of this emulator (for ADB):")(0).split(":").last.trim()
        } yield emuID
        emuComp match {
          case SuccTry(emuID) => emuID
          case FailTry(Fail(c, ec, o, e)) => return FailTry(Fail(c, ec, o, e))
        }
      }
    }

    for {
      // Wait for device until it has started
      p6 <- Adb.target(emuID).waitForDevice() ! ;

      // Repeatedly poll the device's 'ps' list and wait until bootanimation has terminated
      p7 <- repeat (Adb.target(emuID).shell("ps") #| Cmd("grep bootanimation") #| Cmd("wc -l")) until {
        case SuccTry(Succ(c, o, e)) => o.trim() == "0"
        case default => false
      }
    } yield emuID
  }

}

object TestEmulator {

  def main(args: Array[String]): Unit = {

    implicit val logger = Logger(LoggerFactory.getLogger("emu-tester"))

    implicit val ec = ExecutionContext.global

    val sdCardPath = "/data/sd-store"

    val avdName = "scala-test-again-x86"
    val emuID = for {
      pz <- doTry (Android.deleteAVD(avdName)) !;
      p0 <- Cmd("echo no") #| Android.createAVD(avdName, true).x86.api23 !;
      emuID <- Emulator.name(avdName, Some(5560)).sdCard("/data/sdStore").noWindow(false).start !!!;
      p1 <- Lift ! println(s"Lifted: $emuID");
      p2 <- Lift ! Thread.sleep(10000);
      p3 <- Adb.target(emuID).kill !
    } yield emuID

    println(s"Here! $emuID")


  }

}

object TestManyEmulator {

  def main(args: Array[String]): Unit = {

    implicit val logger = Logger(LoggerFactory.getLogger("emu-tester"))

    implicit val ec = ExecutionContext.global

    val sdCardPath = "/data/sd-store"
    val avdName = "scala-test-again-x86"

    val emus = Seq(("scala-1","/data/sd-store/scala-1",5560),("scala-2","/data/sd-store/scala-2",5570))

    emus.map {
      (avd:(String,String,Int)) => {
        val avdName = avd._1
        val sdPath  = avd._2
        val port    = avd._3
        for {
          pz <- doTry (Android.deleteAVD(avdName)) !;
          p0 <- Cmd("echo no") #| Android.createAVD(avdName, true).x86.api23 !;
          emuID <- Emulator.name(avdName, Some(port)).sdCard(sdPath).noWindow(false).start !!!
        } yield emuID
      }
    }.map {
       _ match {
         case SuccTry(emuID) => {
           logger.info(s"Successfully started $emuID")
           Thread.sleep(20000)
           Adb.target(emuID).kill !
         }
         case FailTry(Fail(c,ec,o,e)) => logger.error(s"Failed: $e")
       }
    }

  }

}