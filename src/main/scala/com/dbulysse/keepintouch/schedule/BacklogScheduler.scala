package com.dbulysse.keepintouch.schedule

import com.dbulysse.keepintouch.Entry
import com.dbulysse.keepintouch.util.Days

/*
 * Sort with most overdue entries first. Removes entries that are due in the future
 */
object BacklogScheduler extends Scheduler {
  def apply(entries: Seq[Entry]): Seq[Entry] = {
    // Calculate the value to sort by and put in (entry, value) tuple
    entries.map { e => {
      // How many days ago was this unit supposed to be contacted?
      //   (negative value means date is in the future)
      val daysOverdue = Days.between(
        Days.after(e.lastContacted, e.interval),
        Days.Today)

      (e, daysOverdue)
    }}

      // Only entries that are due today or before
      .filter(_._2 >= 0)

      // Sort with most overdue entries first
      .sortBy(_._2)(Ordering.Int.reverse)

      // Extract entry
      .map(_._1)
  }
}
