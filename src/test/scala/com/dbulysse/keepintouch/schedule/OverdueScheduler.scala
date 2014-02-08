package com.dbulysse.keepintouch.schedule

import org.specs2.mutable._

import java.util.Date

import com.dbulysse.keepintouch.Entry

object OverdueSchedulerSpec extends Specification {
  "OverdueScheduler" should {
    val entrySets: Seq[(Double, Seq[Entry], Seq[Entry])] = Seq(
      (
        0,
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry( 1, new Date(2014, 1, 9), Seq("Bravo")),
          Entry(10, new Date(2014, 1, 3), Seq("Charlie", "Delta"))),
        Seq(
          Entry( 1, new Date(2014, 1, 9), Seq("Bravo")),
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(10, new Date(2014, 1, 3), Seq("Charlie", "Delta")))),
      (
        0,
        Seq(
          Entry(30, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(28, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(26, new Date(2014, 1, 3), Seq("Charlie", "Delta"))),
        Seq(
          Entry(26, new Date(2014, 1, 3), Seq("Charlie", "Delta")),
          Entry(28, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 1), Seq("Alpha")))),

      // TODO: Find a better way of testing random
      (
        0.10,
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(10, new Date(2014, 1, 4), Seq("Bravo")),
          Entry(10, new Date(2014, 1, 6), Seq("Charlie", "Delta"))),
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(10, new Date(2014, 1, 4), Seq("Bravo")),
          Entry(10, new Date(2014, 1, 6), Seq("Charlie", "Delta")))))

    "schedule people by how overdue they are" in {
      entrySets.foreach { case (randomWeight, before, after) =>
        (new OverdueScheduler(randomWeight))(before) mustEqual after
      }
    }
  }
}
