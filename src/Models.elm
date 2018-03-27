module Models exposing (..)

import Date exposing (..)
import Dict exposing (..)
import Http exposing (..)


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
    { name : String, state : LockState, lockedSince : Date, lockedSinceStr : String }


type alias Pool =
    String


type alias Pools =
    Dict Pool (List Lock)


type alias Flags =
    { backendUrl : String }


type alias Model =
    { flags : Flags, pools : Pools, loading : Bool, githubToken : Maybe String }


type Msg
    = NoOp
    | NewLocks (Result Http.Error Pools)
    | PerformLockAction LockAction
    | LockActionDone (Result Http.Error (List String))


type ErrorMessage
    = ErrorMessage String


initialModel : Model
initialModel =
    { flags = { backendUrl = "http://localhost:1000" }
    , pools = Dict.empty
    , loading = True
    , githubToken = Nothing
    }
