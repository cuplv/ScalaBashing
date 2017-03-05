package edu.colorado.plv.fixr.bash.utils

import com.typesafe.scalalogging.Logger

/**
  * Created by edmund on 3/5/17.
  */
sealed trait TryM[+R,E] {
  def flatMap[NR](f: R => TryM[NR,E]) (implicit logger:Logger): TryM[NR,E] = this match {
    case SuccTry(r) => {
      logger.info(r.toString)
      f(r)
    }
    case FailTry(e) => {
      logger.error(e.toString)
      FailTry(e)
    }
  }

  def map[NR](f: R => NR) (implicit logger: Logger): TryM[NR,E] = flatMap { r => SuccTry(f(r)) }

  def isSucc(): Boolean
}

case class SuccTry[R,E](result: R) extends TryM[R,E] {
  override def isSucc(): Boolean = true
}

case class FailTry[R,E](error: E) extends TryM[R,E] {
  override def isSucc(): Boolean = false
}