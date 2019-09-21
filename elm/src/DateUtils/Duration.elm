module DateUtils.Duration exposing (ago, agoFromNow, diff)

import Task
import Time exposing (posixToMillis)


pluralize : Int -> String -> String
pluralize h singular =
    case h of
        1 ->
            "1 " ++ singular

        n ->
            String.fromInt n ++ " " ++ singular ++ "s"


ago : Int -> String
ago millis =
    let
        seconds =
            millis // 1000
    in
    if seconds < 60 then
        "few seconds ago"

    else if seconds < 60 * 60 then
        String.fromInt (seconds // 60) ++ " min ago"

    else if seconds < 60 * 60 * 24 then
        pluralize (seconds // (60 * 60)) "hour" ++ " ago"

    else
        pluralize (seconds // (60 * 60 * 24)) "day" ++ " ago"


diff : Time.Posix -> Time.Posix -> Int
diff t1 t2 =
    posixToMillis t1 - posixToMillis t2


agoFromNow : Time.Posix -> Cmd String
agoFromNow d =
    Task.perform identity (Task.map ago (Task.map (diff d) Time.now))
