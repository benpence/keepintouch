package com.dbulysse.keepintouch.util

import java.util.Date

object Days {
  val Today = new Date
  val MillisecondsPerDay: Long = 1000 * 60 * 60 * 24

  def between(begin: Date, end: Date): Int =
    ((end.getTime - begin.getTime) / MillisecondsPerDay).toInt

  def after(date: Date, days: Int): Date =
    new Date(date.getTime + days * MillisecondsPerDay)
}
