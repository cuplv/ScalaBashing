package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.TryM
import edu.colorado.plv.fixr.bash.{Check, Fail, Pipeable, Succ}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

/**
  * Created by edmund on 3/9/17.
  */


/*
am [start|instrument]

am start [-a <action>] [-d <data_uri>]
[-t <mime_type>] [-c <category> [-c <category>] ...]
[-e <extra_key> <extra_value>
[-e <extra_key> <extra_value> ...]
[-n <component>] [-D] [<uri>]

am instrument [-e <arg_name> <arg_value>] [-p <prof_file>] [-w] <component>

adb shell am instrument -w -r -e eventTrace <ProtoBuf Trace>
-e debug false
-e class com.peilunzhang.contractiontimerdistilled.TraceNoCrash
com.peilunzhang.contractiontimerdistilled.test/edu.colorado.plv.chimp.driver.ChimpJUnitRunner
*/

object AmInstrument extends AmInstrument("adb shell am instrument") {

  def target(deviceName: String): AmInstrument = AmInstrument(s"adb -s $deviceName shell am instrument")

}

case class AmInstrument(cmd:String) extends Pipeable {

  override def command(): String = cmd

  override def ? (implicit bashLogger: Logger): TryM[Succ,Fail] = Check(Seq("adb")) !

  def extend(raw:String): AmInstrument = AmInstrument(s"$cmd $raw")

  def sync(): AmInstrument = extend("-w")

  def raw(): AmInstrument = extend("-r")

  def extra(key:String, value:String): AmInstrument = extend(s"-e $key $value")

  def debug(de:Boolean): AmInstrument = extra("debug", s"$de")

  def components(appPackage:String, testerClass:String, testerPackage:String, instrumenterName:String): AmInstrument = {
     extra("class",s"$appPackage.$testerClass").extend(s"$testerPackage/$instrumenterName")
  }

}

object TestAm {

  def main(args:Array[String]): Unit = {

    implicit val logger = Logger(LoggerFactory.getLogger("am-tester"))
    implicit val ec = ExecutionContext.global

    val proto = "ChMIARIPCAESCwoJCAIaBWxvZ2luCgYIAhoCCAMKBggCGgIIBQoGCAESAggIChsIARIXCAY6EwoLCAIaB3VzZXJib3gSBHRlc3QKBggCGgIIBgoGCAIaAggHCgYIARICCAgKGggBEhYIBjoSCgoIAhoGcHdkYm94EgQxMjM0CgYIAhoCCAQKBggCGgIIBQoGCAESAggIChAIARIMCAESCAoGCAIaAkdv"
    val instrOut = for{
      appAPKout  <- Aapt.home("/usr/local/android-sdk/build-tools/24.0.3").apkInfo("/data/chimpCheck/app-debug.apk") !!! ;
      testAPKout <- Aapt.home("/usr/local/android-sdk/build-tools/24.0.3").apkInfo("/data/chimpCheck/app-debug-androidTest.apk") !!! ;
      appInfo  <- Aapt.parse(appAPKout) ;
      testInfo <- Aapt.parse(testAPKout) ;
      instrOut <- AmInstrument.target("emulator-5554").raw().sync().debug(false).extra("eventTrace",proto)
                              .components( appInfo.packageName, "TestExpresso", testInfo.packageName,"edu.colorado.plv.chimp.driver.ChimpJUnitRunner") !
    } yield instrOut

    println(s"Done! $instrOut")

  }

}