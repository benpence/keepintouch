import datetime
import re

import keepintouch

DATE_FORMAT = "%Y/%m/%d"
DATA_REGEX = re.compile(r"""
    (\d+) \n
    (\d\d\d\d/\d\d/\d\d) \n
    ((?:(?: [^\n]+) (?:\n|\Z))+)
    
    """, re.VERBOSE)

def parse_data(input_pipe):
    data = []

    for match in DATA_REGEX.finditer(input_pipe.read()):
        interval = int(match.group(1))

        date = datetime.datetime.strptime(
            match.group(2),
            DATE_FORMAT).date()

        names = [
            n
            for n in filter(
                lambda s: s != "",
                match.group(3).split("\n"))]

        entry = keepintouch.Entry(interval, date, names)
        data.append(entry)

    return data

def output_data(output_pipe, entries):
    output_pipe.write("\n\n".join((
        "%d\n%s\n%s" % (
            entry.interval,
            entry.last_contacted.strftime(DATE_FORMAT),
            "\n".join(entry.names))
        for entry in entries)))
