package edu.colorado.plv.fixr.bash.android

import edu.colorado.plv.fixr.bash.Pipeable

/**
  * Created by edmund on 3/7/17.
  **/

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
