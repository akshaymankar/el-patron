import Html exposing (..)
import Debug exposing (crash)
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
type alias Pool = {name: String, locks: List Lock}

type alias Model =
    {pools: List Pool}

initialModel : Model
initialModel =
    {pools = [{name = "pool1", locks = [{name ="Lock1", state = Claimed}]}]}

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- UPDATE

type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

lockView : Lock -> Html Msg
lockView lock = li [] [text lock.name]

locksView : List Lock -> Html Msg
locksView locks = ul [] (List.map lockView locks)

poolView : Pool -> Html Msg
poolView x = li [] [text x.name, locksView x.locks]

poolsView : Model -> Html Msg
poolsView model = ul [] (List.map poolView model.pools)

view : Model -> Html Msg
view model =
    div []
        [poolsView model]
