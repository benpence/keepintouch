package com.dbulysse.keepintouch.schedule

import scala.util.Random
import com.dbulysse.keepintouch.Entry
import com.dbulysse.keepintouch.util.Days

object OverdueScheduler {
  val DefaultWeight = 0.25
  val instance = new OverdueScheduler(DefaultWeight)

  def apply(entries: Seq[Entry]): Seq[Entry] = instance(entries)
}

/*
 * Sort with most overdue entries first. Adds or subtracts a weighted random value a
 */
class OverdueScheduler(val randomWeight: Double) extends Scheduler {
  if (randomWeight < 0 || 1 < randomWeight) 
    throw new Exception("randomWeight must be 0 <= randomWeight <= 1")

  def apply(entries: Seq[Entry]): Seq[Entry] = {
    // Calculate the value to sort by and put in (entry, value) tuple
    entries.map { e => {
      // randomProportion * interval < randomizedInterval < randomProportion * interval
      val minBound = (1 - randomWeight) * e.interval
      val randomPortion = 2 * ( (e.interval * randomWeight) * Random.nextFloat )
      
      val randomizedInterval = (minBound + randomPortion).toInt

      // How many days ago was this unit supposed to be contacted?
      //   (negative value means date is in the future)
      val daysOverdue = Days.between(
        Days.after(e.lastContacted, randomizedInterval),
        Days.Today)

      (e, daysOverdue)
    }}

      // Sort with most overdue entries first
      .sortBy(_._2)(Ordering.Int.reverse)

      // Extract entry
      .map(_._1)
  }
}
