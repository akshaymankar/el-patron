module DateUtils.Duration exposing (ago, agoFromNow)

import Date exposing (..)
import Date.Extra.Duration exposing (..)
import Task exposing (..)


pluralize : Int -> String -> String
pluralize h singular =
    case h of
        1 ->
            "1 " ++ singular

        n ->
            toString n ++ " " ++ singular ++ "s"


ago : DeltaRecord -> String
ago { year, month, day, hour, minute, second } =
    if year > 0 then
        pluralize year "year" ++ " ago"
    else if month > 0 then
        pluralize month "month" ++ " ago"
    else if day > 0 then
        pluralize day "day" ++ " ago"
    else if hour > 0 then
        pluralize hour "hour" ++ " ago"
    else if minute > 0 then
        toString minute ++ " min ago"
    else
        "few seconds ago"


agoFromNow : Date -> Cmd String
agoFromNow d =
    perform identity (Task.map ago (Task.map (diff d) now))
