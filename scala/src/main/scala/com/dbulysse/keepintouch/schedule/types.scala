package com.dbulysse.keepintouch.schedule

import com.dbulysse.keepintouch.Entry

trait Scheduler extends (Seq[Entry] => Seq[Entry])
