import datetime

def todays_date():
    return datetime.datetime.today().date()

def contacted_today(person, entries):
    for entry in entries:
        for name in entry.names:
            if person.lower() == name.lower():
                entry.last_contacted = todays_date()
                return True
                
    return False
