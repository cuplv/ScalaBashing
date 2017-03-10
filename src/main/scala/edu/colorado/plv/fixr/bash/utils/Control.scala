package edu.colorado.plv.fixr.bash.utils

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.{Bash, Cmd, Fail, Succ}

/**
  * Created by edmund on 3/7/17.
  */

object Conditions {

  def succMatches(exp: String): TryM[Succ,Fail] => Boolean = {
    (res:TryM[Succ,Fail]) => res match {
      case SuccTry(Succ(c, o, e)) => o.trim() == exp
      case default => false
    }
  }

}

case class repeat(op: Bash) extends Bash {

  var condition = (x:TryM[Succ,Fail]) => false
  var waitTime = 1000
  var tries = -1
  var ignore = true

  def until(c: TryM[Succ,Fail] => Boolean) (implicit bashLogger: Logger): TryM[Succ,Fail] = { condition = c ; this ! }

  def wait(time: Int): repeat = { waitTime = time ; this }

  def times(number: Int): repeat = { tries = number ; this }

  def ignoreFail(ig: Boolean): repeat = { ignore = ig ; this }

  override def !(implicit bashLogger: Logger): TryM[Succ, Fail] = {
    var count = 0
    while (true) {
       val res = for{ p <- op ! } yield p
       if (!ignore) {
         res match {
           case FailTry(_) => return res
           case default    =>
         }
       }
       if (condition(res)) {
         res match {
           case FailTry(Fail(c, ec, o, e)) => return SuccTry(Succ("<RepeatUntil>", o, e))
           case default => return res
         }
       }
       count += 1
       if (tries > 0 && count > tries) return SuccTry(Succ("<RepeatUntil>", "Exceeded number of tries", ""))
       Thread.sleep(waitTime)
    }
    FailTry(Fail("<RepeatUntil>", 2, "", "This is unreachable."))
  }

}

case class doTry(op: Bash) extends Bash {

  var contingencies: Fail => Bash = (f:Fail) => Cmd(s"echo Try failed: ${f.stderr}")

  def catchWith(c: Fail => Bash) (implicit bashLogger: Logger): TryM[Succ,Fail] = { contingencies = c ; this ! }

  override def ! (implicit bashLogger: Logger): TryM[Succ, Fail] = {
     op ! match {
       case SuccTry(Succ(c,o,e)) => SuccTry(Succ(c,o,e))
       case FailTry(fail) => contingencies(fail) !
     }
  }

}