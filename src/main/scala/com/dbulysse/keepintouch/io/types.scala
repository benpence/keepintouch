package com.dbulysse.keepintouch.io

import com.dbulysse.keepintouch.Entry

trait Inputter[A] {
  def apply(input: A): Seq[Entry]
}

trait Outputter[A] {
  def apply(entries: Seq[Entry]): A
}
