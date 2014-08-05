class Entry(object):
    def __init__(self, interval, last_contacted, names):
        self.interval = interval
        self.last_contacted = last_contacted
        self.names = names

    def __repr__(self):
        return "Entry(%d, %s, %s)" % (
            self.interval,
            self.last_contacted.strftime("%Y/%m/%d"),
            ",".join(self.names))
