import datetime
import random
import math

from keepintouch import util

class Scheduler(object):
    def schedule(self, entries):
        pass
        
class BacklogScheduler(Scheduler):
    @classmethod
    def _appraise(cls, entry):
        return entry.last_contacted + datetime.timedelta(days=entry.interval)

    @classmethod
    def schedule(cls, entries):
        today = util.todays_date()

        appraised_entries = []
        for entry in entries:
            due = cls._appraise(entry)
            if due < today:
                appraised_entries.append((entry, due))

        appraised_entries.sort(key=lambda p: p[1])

        return [p[0] for p in appraised_entries]

class WeightScheduler(Scheduler):
    DEFAULT_WEIGHT = 0.75

    def __init__(self, weight=DEFAULT_WEIGHT):
        self.weight = weight

    def _appraise(self, entry):
        return math.floor(
            self.weight *                     # A portion of...
            entry.interval +                  #   ...the interval and
            (1 - self.weight) * 2 *           # a portion of...
            random.random() * entry.interval  #   ...random above or below original
            )

    def schedule(self, entries):
        return sorted(entries, key=self._appraise)

class RandomScheduler(Scheduler):
    @classmethod
    def schedule(cls, entries):
        shuffled_entries = [e for e in entries]

        random.shuffle(shuffled_entries)

        return shuffled_entries
