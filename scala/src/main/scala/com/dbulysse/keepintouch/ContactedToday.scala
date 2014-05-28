package com.dbulysse.keepintouch

/*
 * @return @param entries, with the lastContacted field updated to today of the entry that contains @param name
 */
object ContactedToday {
  /*
   * Mark a person as contacted today
   *
   * @return   Some(s) with the relevant entry's lastContacted field updated to today
   *           None if the name was not in the sequence
   */
  def apply(entries: Seq[Entry], name: String): Option[Seq[Entry]] = {
    // foldRight ensures consistent ordering
    entries.foldRight((false, Seq[Entry]())) {
      case (e @ Entry(i, _, names), (wasFound, accList)) =>
        if (names.exists(namesEqual(_, name)))
          (true, new Entry(i, names: _*) +: accList)
        else
          (wasFound, e +: accList)
    }

      match {
        case (false, _) => None
        case (_,     s) => Some(s)
      }
  }

  def namesEqual(first: String, second: String): Boolean = {
    first.toLowerCase.equals(second.toLowerCase)
  }
}
