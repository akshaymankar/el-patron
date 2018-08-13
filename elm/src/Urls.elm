module Urls exposing (..)

import Models exposing (..)


buildActionUrl : String -> Flags -> Pool -> Lock -> String
buildActionUrl action f pool lock =
    f.backendUrl ++ "/pools/" ++ pool ++ "/locks/" ++ lock.name ++ "/" ++ action


actionUrl : Flags -> LockAction -> String
actionUrl f action =
    case action of
        Claim pool lock ->
            buildActionUrl "claim" f pool lock

        Unclaim pool lock ->
            buildActionUrl "unclaim" f pool lock

        Recycle pool lock ->
            buildActionUrl "recycle" f pool lock

        NoAction ->
            "#"


locksUrl : Flags -> String
locksUrl f =
    f.backendUrl ++ "/locks"


authUrl : Flags -> String
authUrl f =
    f.backendUrl ++ "/auth/page/github/forward"
