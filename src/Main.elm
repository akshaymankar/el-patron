import Debug exposing (crash)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict)
--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type LockState = Unclaimed | Claimed | Recycling | WaitingToRecycle
type alias Lock = {name: String, state: LockState}
type alias Pool = String

type alias Model = Dict Pool (List Lock)

initialModel : Model
initialModel =
  Dict.singleton "pool1" [{name ="Lock1", state = Claimed}]

init : ( Model, Cmd Msg )
init =
    (initialModel, updateLocks)

-- UPDATE

type Msg
   = NoOp | NewLocks (Result Http.Error Model)

decodeLockState : Decoder LockState
decodeLockState = string
  |> andThen (\str ->
       case str of
         "Claimed"          -> succeed Claimed
         "Unclaimed"        -> succeed Unclaimed
         "Recycling"        -> succeed Recycling
         "WaitingToRecycle" -> succeed WaitingToRecycle
         somethingElse      -> fail <| "Unknown lock state: " ++ somethingElse
       )

decodeLock : Decoder Lock
decodeLock =
  decode Lock
  |> required "name" string
  |> required "state" decodeLockState

decodeModel : Decoder Model
decodeModel = dict <| list decodeLock

updateLocks : Cmd Msg
updateLocks = Http.send NewLocks <| Http.get "http://localhost:3000/locks" decodeModel

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        NewLocks (Ok newModel) ->
            newModel ! []
        NewLocks (Err _) ->
            crash "Failed to get locks!"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

lockView : Lock -> Html Msg
lockView lock = p [] [text (lock.name ++ " - " ++ (toString lock.state)) ]

locksView : List Lock -> Html Msg
locksView locks = div [] (List.map lockView locks)

poolView : Pool -> List Lock -> Html Msg
poolView pool locks = div [class "item"] [text pool, locksView locks]

poolsView : Model -> List (Html Msg)
poolsView model = Dict.values (Dict.map poolView model)

view : Model -> Html Msg
view model =
    div [class "masonry"] (poolsView model)
