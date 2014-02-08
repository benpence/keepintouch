package com.dbulysse.keepintouch.schedule

import scala.util.Random
import scala.math.Ordering
import com.dbulysse.keepintouch.Entry

/*
 * Random ordering of entries
 */
object RandomScheduler extends Scheduler {
  def apply(entries: Seq[Entry]): Seq[Entry] = Random.shuffle(entries)
}
