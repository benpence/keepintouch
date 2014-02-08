package com.dbulysse.keepintouch

import java.util.Date

import com.dbulysse.keepintouch.util.Days

case class Entry(val interval: Int, val lastContacted: Date, val names: Seq[String]) {
  def this(interval: Int, names: String*) = this(interval, Days.Today, names.toSeq)
}
