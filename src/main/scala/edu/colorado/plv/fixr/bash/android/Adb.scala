package edu.colorado.plv.fixr.bash.android

import edu.colorado.plv.fixr.bash.Pipeable

/**
  * Created by edmund on 3/7/17.
  */

object Adb extends Adb("adb")

case class Adb(cmd: String) extends Pipeable {

  def extend(raw: String) = Adb(s"$cmd $raw")

  def target(deviceName: String) = extend(s"-s $deviceName")

  def waitForDevice(): Adb = extend("wait-for-device")

  def shell(shcmd: String): Adb = extend(s"shell $shcmd")

  // def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = Cmd(cmd) !

  override def command(): String = cmd

}