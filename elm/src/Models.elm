module Models exposing (..)

import Http exposing (..)
import Time exposing (..)


type alias TimesAndOwner =
    { since : Time.Posix, sinceStr : String, owner : LockOwner }


type alias Times =
    { since : Time.Posix, sinceStr : String }


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


type alias Config =
    { disableActionButtons : Bool }


type Error
    = FailedToLoadLocks String
    | FailedToDoLockAction


type alias Model =
    { flags : Flags, pools : Pools, loading : Bool, config : Config, error : Maybe Error }


type BetterHttpError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String


type Msg
    = NoOp
    | NewLocks (Result BetterHttpError ( Pools, Config ))
    | PerformLockAction LockAction
    | LockActionDone (Result BetterHttpError (List String))


type ErrorMessage
    = ErrorMessage String


initialModel : Model
initialModel =
    { flags = {}
    , pools = []
    , loading = True
    , config = { disableActionButtons = False }
    , error = Nothing
    }
