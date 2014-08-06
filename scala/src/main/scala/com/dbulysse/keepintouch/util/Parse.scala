package com.dbulysse.keepintouch.util

object Parse {
  def isWeight(s: String) =
    try {
      val d = s.toDouble
      0 <= d && d <= 1
    } catch {
      case _: Throwable => false
    }
}
