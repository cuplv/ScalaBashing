package edu.colorado.plv.fixr.bash.utils

import com.typesafe.scalalogging.Logger
import edu.colorado.plv.fixr.bash.{Bash, Fail, Succ}

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
  var tries = 10

  def until(c: TryM[Succ,Fail] => Boolean) (implicit bashLogger: Logger): TryM[Succ,Fail] = { condition = c ; this ! }

  def wait(time: Int): repeat = { waitTime = time ; this }

  def times(number: Int): repeat = { tries = number ; this }

  override def !(implicit bashLogger: Logger): TryM[Succ, Fail] = {
    var count = 0
    while (true) {
       val res = for{ p <- op ! } yield p
       if (condition(res)) return res
       count += 1
       if (tries > 0 && count > tries) return SuccTry(Succ("<RepeatUntil>", "Exceeded number of tries", ""))
       Thread.sleep(waitTime)
    }
    FailTry(Fail("<RepeatUntil>", 2, "", "This is unreachable."))
  }

}
