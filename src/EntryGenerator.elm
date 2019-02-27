module Main exposing (EntryGenerator)

import Date exposing (Date)
import Time exposing (Weekday)


type EntryGenerator
    = EntryGenerator String Schedule


type Schedule
    = Single String Date
    | Weekly Weekday
