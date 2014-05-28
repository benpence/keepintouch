package com.dbulysse.keepintouch.io

import java.text.SimpleDateFormat
import com.dbulysse.keepintouch.Entry

object PlaintextPutter extends Inputter[String] with Outputter[String] {
  val DateFormat = new SimpleDateFormat("yyyy/MM/dd")

  val EntryRegex = """(\d+)\n(\d\d\d\d/\d\d/\d\d)\n((?:[^\n]+(?:\n|\Z))+)""".r
  val EntryFormat = "%d\n%s\n%s"

  // Inputter
  def apply(input: String): Seq[Entry] = {
    EntryRegex.findAllIn(input).map {
      case EntryRegex(intervalS, lastContactedS, namesS) => {
        // TODO: Catch exception java.text.ParseException or narrow regex
        Entry(
          intervalS.toInt,
          DateFormat.parse(lastContactedS),
          namesS.split("\n"))
      }
    }

      // Convert from scala.util.matching.Regex.MatchInterator
      .toSeq
  }

  // Outputter
  def apply(entries: Seq[Entry]): String =
    // Format each entry
    entries.map { e =>
      EntryFormat.format(
        e.interval,
        DateFormat.format(e.lastContacted),
        e.names.mkString("\n"))
    }
     
      // Separate entry strings by newline
      .mkString("\n\n")
}
