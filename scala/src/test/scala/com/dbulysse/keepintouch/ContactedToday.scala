package com.dbulysse.keepintouch

import org.specs2.mutable._

import java.util.Date

import com.dbulysse.keepintouch.util.Days

object ContactedTodaySpec extends Specification {
  val Today = Days.Today

  "ContactedToday" should {
    val entrySets: Seq[(String, Seq[Entry], Option[Seq[Entry]])] = Seq(
      (
        "Delta",
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 3), Seq("Charlie", "Delta"))),
        Some(Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, Today,                Seq("Charlie", "Delta"))))),
      (
        "Alpha",
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 3), Seq("Charlie", "Delta"))),
        Some(Seq(
          Entry(10, Today,                Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 3), Seq("Charlie", "Delta"))))))

    val nones: Seq[(String, Seq[Entry])] = Seq(
      (
        "Echo",
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 3), Seq("Charlie", "Delta")))),
      (
        "Foxtrot",
        Seq(
          Entry(10, new Date(2014, 1, 1), Seq("Alpha")),
          Entry(20, new Date(2014, 1, 2), Seq("Bravo")),
          Entry(30, new Date(2014, 1, 3), Seq("Charlie", "Delta")))))
    

    "update date for the person contacted" in {
      entrySets.foreach { case (name, before, after) =>
        ContactedToday(before, name) mustEqual after 
      }
    }

    "recognize when you contact someone new" in {
      nones.foreach { case (name, entries) =>
        ContactedToday(entries, name) mustEqual None
      }
    }
  }
}
