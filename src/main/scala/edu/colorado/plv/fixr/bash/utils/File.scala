package edu.colorado.plv.fixr.bash.utils

import java.io.File

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.{Bash, BashResult, Fail, Succ}
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory

import scala.util.Random


/**
  * Created by edmund on 3/4/17.
  */

case class DeleteFile(pathName: String, ignoreMissing: Boolean) extends Bash {

  override def !(implicit bashLogger: Logger): TryM[Succ,Fail] = {
    bashLogger.debug(s"Attempting to delete ${pathName}")
    val path = new File(pathName)
    val cmd = s"<DeleteFile $pathName>"
    if (!path.isFile) {
      bashLogger.debug(s"Path is not a file ($pathName). Delete aborted.")
      return FailTry(Fail(cmd, 2, "", s"Path is not a file ($pathName). Delete aborted."))
    }
    if (!path.exists()) {
      bashLogger.debug(s"Path does not exist ($pathName). Delete aborted.")
      if (ignoreMissing) return SuccTry(Succ(cmd,"","")) else return FailTry(Fail(cmd, 2, "", s"Path does not exist ($pathName). Delete aborted."))
    }
    if (path.delete()) return SuccTry(Succ(cmd,"Successfully deleted",""))
    else {
      bashLogger.debug(s"Failed to delete $pathName")
      return FailTry(Fail(cmd, 2, "", s"Failed to delete $pathName"))
    }
  }

}

case class DeleteDir(pathName: String, ignoreMissing: Boolean) extends Bash {

  override def !(implicit bashLogger: Logger): TryM[Succ,Fail] = {
    bashLogger.debug(s"Attempting to delete ${pathName}")
    val path = new File(pathName)
    val cmd = s"<DeleteFile $pathName>"
    if (!path.isDirectory) {
      bashLogger.debug(s"Path is not a directory ($pathName). Delete aborted.")
      return FailTry(Fail(cmd, 2, "", s"Path is not a directory ($pathName). Delete aborted."))
    }
    if (!path.exists()) {
      bashLogger.debug(s"Path does not exist ($pathName). Delete aborted.")
      if (ignoreMissing) return SuccTry(Succ(cmd,"","")) else return FailTry(Fail(cmd, 2, "", s"Path does not exist ($pathName). Delete aborted."))
    }
    FileUtils.deleteDirectory(path)
    return SuccTry(Succ(cmd,"Successfully deleted",""))
  }

}

case class Delete(pathName: String, ignoreMissing: Boolean) extends Bash {
  override def !(implicit bashLogger: Logger): TryM[Succ,Fail] = {
    val path = new File(pathName)
    if (path.isFile) return (DeleteFile(pathName, ignoreMissing) !)
    if (path.isDirectory) { return (DeleteDir(pathName, ignoreMissing) !) }

    bashLogger.debug("Unknown path type")
    return FailTry(Fail(s"<Delete> $pathName", 2, "", "Unknown path type"))
  }
}

case class CreateDir(pathName: String, recreate: Boolean) extends Bash {

  override def !(implicit bashLogger: Logger): TryM[Succ,Fail] = {
    bashLogger.debug(s"Attempting to create ${pathName}")
    val path = new File(pathName)
    val cmd = s"<CreateDir $pathName>"
    if (path.exists()) {
      bashLogger.debug(s"Path already exists ${pathName}")
      if (recreate) {
        Delete(pathName, true) !

        bashLogger.debug(s"Path ${pathName} deleted.")
      } else {
        bashLogger.debug(s"Create path ${pathName} aborted.")
        return SuccTry(Succ(cmd, s"Create path ${pathName} aborted.", ""))
      }
    }
    val succ = path.mkdirs()
    bashLogger.debug( s"Path ${pathName} created: $succ" )
    if (succ) SuccTry(Succ(cmd,s"Path ${pathName} created: $succ","")) else FailTry(Fail(cmd, 2, "", "Failed to create directories"))
  }

}

object GenFile {

  def genNewFile(): File = genNewFile("")

  def genNewFile(ext:String): File = {
     val rand = Random
     while(true) {
        val fext = if (ext.length > 0) s".$ext" else ""
        val file = new File(s"tmp_${rand.nextInt()}_${System.currentTimeMillis/1000}$fext")
        if (!file.exists()) return file
     }
     return null
  }

}

object BashFile {

   def main(args: Array[String]): Unit = {

     implicit val bashLogger:Logger = Logger(LoggerFactory.getLogger("Happy-Pokemon"))

     for {
        i <- DeleteDir("/home/edmund/workshops/test/gogo", true) ! ;
        j <- DeleteDir("/home/edmund/workshops/test/cas", true) ! ;
        k <- CreateDir("/home/edmund/workshops/test/gogo/coco/pop", false) !
     } yield k




   }

}
