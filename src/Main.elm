module Main exposing (..)

import Debug exposing (crash)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models exposing (..)
import Navigation exposing (..)
import Urls exposing (..)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init f =
    let
        model =
            { initialModel | flags = f }
    in
    ( model, updateLocks model )



-- UPDATE


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


getWithCreds : String -> Decoder a -> Http.Request a
getWithCreds url decoder =
    Http.request
        { method = "Get"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }


type ErrorMessage
    = ErrorMessage String


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    decode ErrorMessage |> required "message" string


updateLocks : Model -> Cmd Msg
updateLocks oldModel =
    Http.send NewLocks <| getWithCreds (locksUrl oldModel.flags) decodeModel


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

        NewLocks (Err (Http.BadStatus r)) ->
            if r.status.code == 403 then
                case decodeString decodeErrorMessage r.body of
                    Ok (ErrorMessage "Permission Denied. User not logged in.") ->
                        ( model, load (authUrl model.flags) )

                    Ok (ErrorMessage e) ->
                        crash e

                    Err _ ->
                        crash "failed to error!"
            else
                crash ("Failed to get locks with code: " ++ r.status.message)

        NewLocks (Err x) ->
            crash ("Failed to get locks!" ++ toString x)

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
            NoAction

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

        NoAction ->
            ""


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
