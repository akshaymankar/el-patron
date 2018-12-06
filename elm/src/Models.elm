module Models exposing (..)

import Date exposing (..)
import Http exposing (..)


type alias TimesAndOwner =
    { since : Date, sinceStr : String, owner : LockOwner }


type alias Times =
    { since : Date, sinceStr : String }


type LockState
    = Unclaimed
    | Claimed TimesAndOwner
    | Recycling Times
    | WaitingToRecycle Times


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


type LockOwner
    = Pipeline PipelineDetails
    | Committer String
    | GafferUser String


type alias PipelineDetails =
    { pipeline : String, job : String, buildNumber : Int }


type alias Lock =
    { name : String, state : LockState }


type alias Pool =
    { name : String, hasLifecycle : Bool }


type alias Pools =
    List ( Pool, List Lock )


type alias Flags =
    {}


type LoadingState
    = Loading
    | Loaded
    | LoadingFailed String


type alias Model =
    { flags : Flags, pools : Pools, loadingState : LoadingState }


type Msg
    = NoOp
    | NewLocks (Result Http.Error Pools)
    | PerformLockAction LockAction
    | LockActionDone (Result Http.Error (List String))


type ErrorMessage
    = ErrorMessage String


initialModel : Model
initialModel =
    { flags = {}
    , pools = []
    , loadingState = Loading
    }
