module Models exposing (..)

import Dict exposing (..)


type LockState
    = Unclaimed
    | Claimed
    | Recycling
    | WaitingToRecycle


type LockAction
    = Claim Pool Lock
    | Recycle Pool Lock
    | Unclaim Pool Lock
    | NoAction


type Route
    = RootRoute
    | GithubCallbackRoute String
    | AuthenticatedRoute String
    | NotFoundRoute


type alias Lock =
    { name : String, state : LockState }


type alias Pool =
    String


type alias Pools =
    Dict Pool (List Lock)


type alias Flags =
    { backendUrl : String }


type alias Model =
    { flags : Flags, pools : Pools, loading : Bool, githubToken : Maybe String }


initialModel : Model
initialModel =
    { flags = { backendUrl = "http://localhost:1000" }
    , pools = Dict.singleton "pool1" [ { name = "Lock1", state = Claimed } ]
    , loading = True
    , githubToken = Nothing
    }
