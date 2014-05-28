package com.dbulysse.keepintouch.util

import scala.io.Source

import java.io.File
import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

object FileUtils {
  val RenamingException: Exception = new Exception

  def read(filename: String): Option[String] = {
    try {
      Some(Source.fromFile(filename).mkString)

    } catch {
      case e: IOException => None
    }
  }

  def replace(filename: String, contents: String): Boolean = {
    try {
      val tempFile = File.createTempFile("keepintouch", "")

      val fileWriter = new FileWriter(tempFile)

      try {
        val buffer = new BufferedWriter(fileWriter)
        buffer.write(contents)
        buffer.flush

      } finally {
        fileWriter.close
      }

      Files.move(
        tempFile.toPath,
        (new File(filename)).toPath,
        REPLACE_EXISTING)

      true

    } catch {
      case e: IOException => false
    }
  }
}

object Terminal {
  lazy val lines = Source.stdin.getLines

  def readLine: Option[String] =
    if (lines.hasNext) Some(lines.next)
    else None

  def writeLine(line: String): Unit = println(line)

  def error(line: String): Unit = {
    writeLine(line)
    System.exit(1)
  }
}
