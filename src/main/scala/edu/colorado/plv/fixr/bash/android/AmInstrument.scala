package edu.colorado.plv.fixr.bash.android

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.utils.{DeleteFile, GenFile, TryM}
import edu.colorado.plv.fixr.bash._
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

  val THRESHOLD_LENGTH = 3500

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

  override def ! (implicit bashLogger: Logger): TryM[Succ,Fail] = {
    val cmd = command();
    if (cmd.length < THRESHOLD_LENGTH) {
      this ! cmd
    } else {
      // Length of command is too long, revert to 'adb push' and calling shell script.
       val tmpFile = GenFile.genNewFile("sh")
       val tmpPath = tmpFile.getPath
       for {
          p1 <- Cmd(s"echo ${cmd.split("shell")(1).trim()}") #>> tmpFile ! ;
          p2 <- Cmd(s"chmod +x $tmpPath") ! ;
          p3 <- Adb.push(tmpPath ,"/data/local/tmp") ! ;
          p4 <- Adb.shell(s"sh /data/local/tmp/$tmpPath") ! ;
          p5 <- DeleteFile(tmpPath, false) !
       } yield p4
    }
  }

}

object TestLongCmd {

  def main(args:Array[String]): Unit = {
    implicit val logger = Logger(LoggerFactory.getLogger("long-cmd-tester"))

    val prototrace = "CgYIAhoCCAYKCwgBEgcIB0IDCNAPCgYIAhoCCAYKCwgBEgcIB0IDCNAPChYIAyISCAMiDgoMCAESCAgCGgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDCu8BCAMi6gEIAyLlAQriAQgBEt0BCAY62AEKAggDEtEB5Yud7I+f75ix67u66raE7rKD5Ya34L6z4rue7oiJ64ii7J6N77Sn76SU7ZW44a675La36Ii34r+46Yu86YqZ6Yi967ui7qOJ05rlibXls7bohJDqlJ3mubLsjrzpub/itIrpnZDihLvnlqvquLXvnprsoavWsum2kOG2qeemtearh+iSiOeMmeOhiuK5ru+/heWyu3Dng5norq3mvIrjgKrvuoniubfhsr7mg5HlvYrhk6/jh5Xpg6vnnpjmp5DqmJbhuJznrbTnr4btlaDon7kKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKYQgDIl0IAyJZClcIARJTCAY6TwoCCAMSSeyZmuSvmu+5veWyv+OuqOCvt9mE6omk4Zms4bOQ666T5buY4qKF3aDnmobksInlk5bhi4ftiJzlnJvmoZXmpZDqg6LqrpHshIcKFggDIhIIAyIOCgwIARIICAESBAoCCAMKRwgDIkMIAyI/Cj0IARI5CAY6NQoCCAMSL+mcn+KknOacrOKcj+uBmdG57rW66ZqR4pum7q2+5rOu7Kqw76OY5qe07Ju86bCmChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDCo0CCAMiiAIIAyKDAgqAAggBEvsBCAY69gEKAggDEu8B74WV57G45aS76Zms74Kz7Lm246ST6rGA6biR54Om5ZiN27rtk67pmpvmr7voiKflnKLoqankt7ThtYbDjeq0jOSrpOCphuOPnOKhk++Hv+qSvO60seSohOqZseCpv+O3kuyFtuWRpemRkOOwkeaIvumiueaou+aNkOqSt+2Dm+y9mum1muGJge+epeypleiyn+iXsOyIp+iVreqqieqnleabmuOEuOOSou6lt+yEpueQjeK6t+yXguCthOunoeGEru6aquKTvOmhs+ivg+iWp+2Co+eqoeKNqkngrK7gur/mpJ/njbnup4vrk4zsnIYKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKvwIIAyK6AggDIrUCCrICCAESrQIIBjqoAgoCCAMSoQLuuYzjnpXsjrDhprrsn6rqrq3mi6LiqYfiubHlprDrvKjqqJvlmrbrharhvL3mvbvkq4Dsp7blh5HkvYHlkJTliJfns5/iuZ/hlobmor3si4TinrbipKbiu7zooILgorjqu7DjpqPusoDunIvkso7ojJvisrLihIrotZPruK/mu6vrubvno4zsu6HhrozvoKziv7zukYfnjZDivrjsqKTok4DmpIjnjZbmrYfnho7jtonhkr7rpo/jn7vki6rvmoDnoZPulYzkpo3nur/irKrlr4bWnuyJtOmAkeKWh9yk55Sg5JWA6qq/5K+r6qG75q244oOI7pGO7Ken46Sp6Lu66q6n65eY7p6d5JGE5aax6pyV7r6L4aWM5Yqd7oG25LuBChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgCGgQKAggDChYIAyISCAMiDgoMCAESCAgCGgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgCGgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgCGgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDChYIAyISCAMiDgoMCAESCAgBEgQKAggDCkIIAyI+CAMiOgo4CAESNAgGOjAKAggDEirskI3qk7Hmsqbpq5vqnZDvurHija7ptaTugKrhrLDiraXolY7jppzmspcKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAIaBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKpQIIAyKgAggDIpsCCpgCCAESkwIIBjqOAgoCCAMShwLvnrbpoJHimbLiobrjq7rotLfvtZPknYzvi5rnlpXrkLHmn4/oqrzipY3nlK3srqDiq7Lsp7vnlITohZfllqfro57qlITkgrnmtLbrtJLkubTmnbLkla3uhavivZPkpLngqpDgtq/jgbvrk7PptqLnt7/jrZ7srKXgqYjjooDrl63ulITGv+iYq+WQoeeMseWXl+eahsm464yc7LO767en7qee6aCb7Z6V5pO55ZWW6Y6Z6ICm7KiK46K67oSb5YWg5a+P4bS+5o2l46+t5ZqC7Zq74Z6X46Kp4oSn4bSP5Z+656OL6pS74bi7xLvns67ns4PooIfikpDuor/qk6Tjm5frnLbbkwoVCAMiEQgDIg0KCwgBEgcIB0IDCIQUCuoBCAMi5QEIAyLgAQrdAQgBEtgBCAY60wEKAggDEswB54GT4raJ5reB5bGx4LOK7r2a5L+66pqz7Kiv57C40JLko4fnk7LkvKPvpZ7isaHliKjrrKXspZLpiqruqbvslrXmoLvhrbPsspbmkbXtjavviK3vvJfLlem5q+yvlOCrv+uCsuu4m+OLh+SiiO+TlOGTiteC5aKO4rO37IWF4KOc5pq377GT5ISR6LqW77a37rKX54G957Oo4auD7o6J54Gg57Cd7ZWa7pCK6oqH6Jyo5Iuz4rqT6biE446s4LeX55SI4ais656k75O0ChYIAyISCAMiDgoMCAESCAgBEgQKAggDCqUCCAMioAIIAyKbAgqYAggBEpMCCAY6jgIKAggDEocC44+r7Jij5bmo4ZGH7oWT45Sj7Iek4Y2a5bmu5r6C6rqz7ryi77O55Jep5o6U6Ya267ue7rui6Ki+7oCn5qCX4rK74La/74OF7o6d7LiD7oKS4rK/5pq455um4qmL5IGB546E4rmY5YiA7qyp7IyP5pGC4bin67iX4aWJ6LqM4LWU76OK7oOc6JmJ75ag4bmb65Wo6oaZ6r++55SF7YGk5rWW6ZSU1K3uu4zurobmhb3lhbzqrZDqrYTmm77utLfii4vrm5Dgo4Lon4Huk5voqbzsprvpuLzuk7Tlqp/vq5HvvbzplJnjsYHpu5/jlJ3ikr7kgaLupZrsk5DnmYzmmY/go4TlgIkKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAIaBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKFggDIhIIAyIOCgwIARIICAESBAoCCAMKkgEIAyKNAQgDIogBCoUBCAESgAEIBjp8CgIIAxJ2456M44+N7pGC5bO56YSu77KZ54uU752q7KeI46ee64yY5Y6/47yZ7IW165OU4LuG6JyO6qm56oey5KCv7ZSN5LuW67eY6YKm4r6LUu6PiOK8sOK5r+mqqOWVkeqahuGCkuehqu2OjeKno+Cxr+G+uOC/guqytwoGCAESAggI"

    val p = for {
      p <- AmInstrument.target("emulator-5554").raw().sync().debug(false).extra("eventTrace", prototrace)
              .components("com.peilunzhang.contractiontimerdistilled", "TestExpresso",
                          "com.peilunzhang.contractiontimerdistilled.test", "edu.colorado.plv.chimp.driver.ChimpJUnitRunner") !!!
    } yield p

    println(p)

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