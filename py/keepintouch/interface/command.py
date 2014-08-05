import os
import tempfile
import sys
import re

import keepintouch
from keepintouch import interface
from keepintouch import util
from keepintouch import io
from keepintouch import schedule

# TODO: Give Interface print/readline object functions
class CommandInterface(interface.Interface):
    DAYS_REGEX = re.compile(r'\d+')

    def __init__(self, filepath):
        self.filepath = filepath

    def entries(self):
        with open(self.filepath, 'r') as data_file:
            # TODO: Catch IOexception or float up?
            return io.parse_data(data_file)
            
    def new_person(self, name):
        days = ""
        while not self.DAYS_REGEX.match(days):
            self._new_person_message(name)
            days = sys.stdin.readline().strip()
            
        return keepintouch.Entry(int(days), util.todays_date(), [name])
            
    def _new_person_message(self, name):
        print("How often do you want to contact '%s' (days)?:" % name)
        
    def replace_data(self, entries):
        temp_file = tempfile.NamedTemporaryFile(
            dir=os.path.dirname(self.filepath),
            mode='w',
            delete=False)
        io.output_data(temp_file, entries)

        os.replace(temp_file.name, self.filepath)
        
    def scheduled(self, entries):
        for entry in entries:
            print(", ".join(entry.names))

def usage():
    program = sys.argv[0]
    print("Usage: %s FILE contact NAME" % program)
    print("Usage: %s FILE [schedule [backlog]]" % program)
    print("Usage: %s FILE schedule weight [WEIGHT]" % program)
    print("Usage: %s FILE schedule random" % program)
    exit(1)

def to_weight(s):
    try:
        return float(s)
    except ValueError as e:
        return False
    
def main():
    args = sys.argv[1:]

    if len(args) == 0:
        usage()
        exit()

    data_file = sys.argv[1]
    interface = CommandInterface(data_file)
    entries = interface.entries()
    
    # Contact
    if len(args) > 2 and args[1] == "contact":
        person = " ".join(args[2:])

        if not util.contacted_today(person, entries):
            new_entry = interface.new_person(person)
            entries.insert(0, new_entry)

        interface.replace_data(entries)

    # Backlog scheduler
    elif len(args) == 1 or (
         len(args) == 2 and args[1] == "schedule") or (
         len(args) == 3 and args[1] == "schedule" and args[2] == "backlog"):
        interface.scheduled(schedule.BacklogScheduler.schedule(entries))
        
    # Weight scheduler
    elif len(args) == 3 and args[1] == "schedule" and args[2] == "weight":
        interface.scheduled(schedule.WeightScheduler().schedule(entries))

    elif len(args) == 4 and args[1] == "schedule" and args[2] == "weight" and to_weight(args[3]):
        scheduler = schedule.WeightScheduler(to_weight(args[3]))
        interface.scheduled(scheduler.schedule(entries))
    
    # Random scheduler
    elif len(args) == 3 and args[1] == "schedule" and args[2] == "random":
        interface.scheduled(schedule.RandomScheduler.schedule(entries))

    else:
        usage()
