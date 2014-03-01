package com.dbulysse.keepintouch.app

import com.dbulysse.keepintouch.Entry
import com.dbulysse.keepintouch.ContactedToday
import com.dbulysse.keepintouch.schedule.OverdueScheduler
import com.dbulysse.keepintouch.io.PlaintextPutter
import com.dbulysse.keepintouch.util.Terminal
import com.dbulysse.keepintouch.util.FileUtils

object Main {
  lazy val IntervalRegex = """^\d+$""".r

  def main(args: Array[String]): Unit = {
    // TODO: Try to work this check into pattern matching without redundancy (implicitly?)
    if (args.length < 2) Terminal.error("Supply more parameters")
    else {
      val dataFile = args(0)
      val entries = FileUtils.read(dataFile) match {
        case None         => { Terminal.error("Cannot access '%s'".format(dataFile)); Seq[Entry]() }
        case Some(input)  => PlaintextPutter(input)
      }

      // TODO: Research pattern matching on mutable Array
      args.drop(1).toSeq match {
        case "schedule"  +: _       => schedule(entries)
        case "contacted" +: pieces  => contacted(entries, pieces.mkString(" "), dataFile)
        case _                      => Terminal.error("Unrecognized parameters")
      }
    }
  }

  def schedule(entries: Seq[Entry]): Unit = {
    Terminal.writeLine(
      // TODO: Add option for choosing scheduler
      OverdueScheduler(entries)
        .map(_.names.mkString(", "))    // Names are comma separated
        .mkString("\n"))                // Lists of names are newline separated
  }

  def contacted(entries: Seq[Entry], name: String, dataFile: String): Unit = {
    val newEntries = ContactedToday(entries, name) match {
      // Update contact
      case Some(modifiedEntries)  => modifiedEntries
 
      // Create new contact
      case _                      => {
        Terminal.writeLine("Creating new entry for new contact '%s'.".format(name))
 
        var intervalString: String = ""
 
        while (IntervalRegex.findFirstMatchIn(intervalString).isEmpty) {
          Terminal.writeLine("How often (in days) do you want to contact '%s'?".format(name))
          intervalString = Terminal.readLine match {
            case Some(s) => s
            case None    => { System.exit(1); ""}
          }
        }
 
        new Entry(intervalString.toInt, name) +: entries
      }
    }

    if (!FileUtils.replace(dataFile, PlaintextPutter(newEntries)))
      Terminal.error("Unable to edit '%s'.".format(dataFile))
  }
}
