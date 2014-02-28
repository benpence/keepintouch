# Summary

_keepintouch_ is a simple program that reminds you to contact people, primarily for the following reasons:

* Pleasure/virtue: friends, family, mentors, etc.
* Utility: Social networking

Developed by dbulysse (Benjamin Pence)

## Usage

    $ keepintouch keepintouch.data | head -3
    Paul Allen
    Patrick Bateman
    Huey, Louie, Dewie

    $ keepintouch keepintouch.data contacted Paul Allen
    $

    $ keepintouch keepintouch.data | head -3
    Patrick Bateman
    Huey, Louie, Dewie
    Paul Allen

# Data Format

Currently, _keepintouch_ expects the user to supply all the people you wish to contact in the following file format:

    interval
    last contacted
    name1
  
    interval
    last contacted
    name1
    name2
  
    ...

where each entry has

* interval: generally how often (in days) you want to communicate with this person/these people
* last contacted: the day this person/these people were last contacted in the year-month-day format YYYY/MM/DD
* name(s): the person/people you want to contact, one per line. Multiple names are allowed because often times one regularly contacts a group/family that only requires a single communique or each person in a group at the same time

A sample data file:

    30
    2014/11/01
    Paul Allen
 
    365
    2014/12/22
    Patrick Bateman
 
    365
    2014/12/22
    Huey
    Louie
    Dewie

    25
    2015/01/22
    Scrooge

# Scheduling

## Overdue Scheduler

The Overdue Scheduler is the default scheduler. For each entry in the data file, _keepintouch_ determines a score to sort the entries by:

    today - (last contacted + (interval +/- 25%)) 

where the `+/- 25%` added or subtracted is randomized each time you run _keepintouch_. Entries with the greatest score will be listed to you first and will tend to be those people that you have not contacted in a while and you wish to contact more often. You don't have to use the program on a regular basis.

You can use the Overdue Scheduler with either of these commands:

    $ keepintouch keepintouch.data
    $ keepintouch keepintouch.data schedule Overdue

## Backlog Scheduler

The Backlog Scheduler will sort the entries by:

    today - (last contacted + interval)

Every entry that is due in the future will not be shown. You can use the Backlog Scheduler with this command:

    $ keepintouch keepintouch.data schedule backlog

## Random Scheduler

The Random scheduler sorts the entries randomly and is not terribly useful. You can use the Random Scheduler with this command:

    $ keepintouch keepintouch.data schedule random

# TODO

## Feature

* _keepintouch_ should allow dates to be entered in alternate or partial formats

## Fix

* _keepintouch_ should validate dates in the datafile
* _keepintouch_ should have more unit tests
