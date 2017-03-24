package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.{Check, Fail, Pipeable, Succ}
import edu.colorado.plv.fixr.bash.utils.TryM

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

}

