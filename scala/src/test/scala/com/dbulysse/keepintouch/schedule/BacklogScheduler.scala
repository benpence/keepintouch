package com.dbulysse.keepintouch.schedule

import org.specs2.mutable._

import com.dbulysse.keepintouch.Entry
import com.dbulysse.keepintouch.util.Days

object BacklogSchedulerSpec extends Specification {
  "BacklogScheduler" should {
    val entrySets: Seq[(Seq[Entry], Seq[Entry])] = Seq(
      ( Seq(
          Entry(1, Days.after(Days.Today, -1), Seq("Bravo")),
          Entry(1, Days.after(Days.Today, -2), Seq("Alpha")),
          Entry(1, Days.Today,                 Seq("Charlie", "Delta"))),
        Seq(
          Entry(1, Days.after(Days.Today, -2), Seq("Alpha")),
          Entry(1, Days.after(Days.Today, -1), Seq("Bravo")))),

      ( Seq(
          Entry(1, Days.after(Days.Today, 1), Seq("Alpha")),
          Entry(1, Days.after(Days.Today, 2), Seq("Bravo")),
          Entry(1, Days.after(Days.Today, 3), Seq("Charlie", "Delta"))),
        Nil))

    "schedule people by how overdue they are" in {
      entrySets.foreach { case (before, after) =>
        BacklogScheduler(before) mustEqual after
      }
    }
  }
}
