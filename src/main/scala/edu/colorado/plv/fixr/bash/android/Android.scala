package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.TryM
import edu.colorado.plv.fixr.bash._
import org.slf4j.LoggerFactory

/**
  * Created by edmund on 3/9/17.
  */

object Android extends Android("android")

case class Android(cmd: String) extends Pipeable {

  override def ? (implicit bashLogger: Logger): TryM[Succ,Fail] = Check(Seq("android")) !

  override def command(): String = cmd

  def extend(raw: String): Android = Android(s"$cmd $raw")

  def deleteAVD(avdName: String): Android = extend(s"delete avd -n $avdName")

  def createAVD(avdName: String, forced: Boolean): Android = {
     if (forced) extend(s"create avd --force -n $avdName") else extend(s"create avd -n $avdName")
  }

  def abi(abiType: String): Android = extend(s"--abi $abiType")

  def api(apiLevel: String): Android = extend(s"-t $apiLevel")

  // Predefined ABI types and API levels

  def x86(): Android = abi("google_apis/x86")

  def arm7(): Android = abi("google_apis/armeabi-v7a")

  def api23(): Android = api("android-23")

  def api22(): Android = api("android-22")

  // Listing available AVDs

  def listAVD(): Android = extend("list avd")

}

case class AVDInfo(name:String, device:String, path:String, target:String, abi:String, skin:String, sdcard:String)

object TestAndroid {

  def main(args: Array[String]): Unit = {

    implicit val logger = Logger(LoggerFactory.getLogger("android-tester"))

    for {
      p0 <- Cmd("echo no") #| Android.createAVD("scala-test-x86", true).x86.api23 !
    } yield p0

  }

}
