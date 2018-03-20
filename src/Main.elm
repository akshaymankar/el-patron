module Main exposing (..)

import Debug exposing (crash)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type LockState
    = Unclaimed
    | Claimed
    | Recycling
    | WaitingToRecycle


type LockAction
    = Claim Pool Lock
    | Recycle Pool Lock
    | Unclaim Pool Lock
    | Nothing


type alias Lock =
    { name : String, state : LockState }


type alias Pool =
    String


type alias Pools =
    Dict Pool (List Lock)


type alias Flags =
    { backendUrl : String }


type alias Model =
    { flags : Flags, pools : Pools, loading : Bool }


initialModel : Model
initialModel =
    { flags = { backendUrl = "http://localhost:1000" }
    , pools = Dict.singleton "pool1" [ { name = "Lock1", state = Claimed } ]
    , loading = True
    }


init : Flags -> ( Model, Cmd Msg )
init f =
    let
        model =
            { initialModel | flags = f }
    in
    ( model, updateLocks model )



-- UPDATE


locksUrl : Flags -> String
locksUrl f =
    f.backendUrl ++ "/locks"


type Msg
    = NoOp
    | NewLocks (Result Http.Error Pools)
    | PerformLockAction LockAction
    | LockActionDone (Result Http.Error (List String))


decodeLockState : Decoder LockState
decodeLockState =
    string
        |> andThen
            (\str ->
                case str of
                    "Claimed" ->
                        succeed Claimed

                    "Unclaimed" ->
                        succeed Unclaimed

                    "Recycling" ->
                        succeed Recycling

                    "WaitingToRecycle" ->
                        succeed WaitingToRecycle

                    somethingElse ->
                        fail <| "Unknown lock state: " ++ somethingElse
            )


decodeLock : Decoder Lock
decodeLock =
    decode Lock
        |> required "name" string
        |> required "state" decodeLockState


decodeModel : Decoder Pools
decodeModel =
    dict <| list decodeLock


updateLocks : Model -> Cmd Msg
updateLocks oldModel =
    Http.send NewLocks <| Http.get (locksUrl oldModel.flags) decodeModel


performLockAction : Flags -> LockAction -> Cmd Msg
performLockAction f a =
    Http.send LockActionDone <| Http.post (actionUrl f a) Http.emptyBody (list string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        NewLocks (Ok newLocks) ->
            { model | pools = newLocks, loading = False } ! []

        NewLocks (Err _) ->
            crash "Failed to get locks!"

        PerformLockAction a ->
            ( { model | loading = True }, performLockAction model.flags a )

        LockActionDone (Ok _) ->
            ( model, updateLocks model )

        LockActionDone (Err _) ->
            crash "Failed to update locks!"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


lockClasses : Lock -> Attribute msg
lockClasses lock =
    classList [ ( "lock", True ), ( toString lock.state, True ) ]


lockText : Lock -> Html msg
lockText lock =
    text (lock.name ++ " - " ++ toString lock.state)


lockAction : Pool -> Lock -> LockAction
lockAction pool lock =
    case lock.state of
        Claimed ->
            Recycle pool lock

        Unclaimed ->
            Claim pool lock

        WaitingToRecycle ->
            Nothing

        Recycling ->
            Unclaim (pool ++ "-lifecycle") lock


toSymbol : LockAction -> String
toSymbol a =
    case a of
        Claim pool lock ->
            "Claim"

        Unclaim pool lock ->
            "Unclaim"

        Recycle pool lock ->
            "Recycle"

        Nothing ->
            ""


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

        Nothing ->
            "#"


lockActionButton : Flags -> Pool -> Lock -> Html Msg
lockActionButton f pool lock =
    let
        action =
            lockAction pool lock
    in
    a [ href "#", onClick (PerformLockAction action) ] [ text (toSymbol action) ]


lockView : Flags -> Pool -> Lock -> Html Msg
lockView f pool lock =
    p [ lockClasses lock ]
        [ span [] [ lockText lock ]
        , span [] [ text " - " ]
        , span [] [ lockActionButton f pool lock ]
        ]


locksView : Flags -> Pool -> List Lock -> Html Msg
locksView f pool locks =
    div [] (List.map (lockView f pool) locks)


poolView : Flags -> Pool -> List Lock -> Html Msg
poolView f pool locks =
    div [ class "item" ] [ p [ class "pool" ] [ text pool ], locksView f pool locks ]


poolsView : Model -> List (Html Msg)
poolsView model =
    Dict.values (Dict.map (poolView model.flags) model.pools)


view : Model -> Html Msg
view model =
    div []
        [ div [ classList [ ( "loader-wrapper", True ), ( "hidden", not model.loading ) ] ]
            [ div [ class "lds-ring" ] [ div [] [] ] ]
        , div [ class "masonry" ] (poolsView model)
        ]
