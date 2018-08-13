module Main exposing (..)

import Date
import Debug exposing (crash)
import Decoders exposing (..)
import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Models exposing (..)
import Navigation exposing (..)
import Task exposing (..)
import Urls exposing (..)
import View exposing (..)


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


requestWithCreds : String -> String -> Decoder a -> Http.Request a
requestWithCreds method url decoder =
    Http.request
        { method = method
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }


getWithCreds : String -> Decoder a -> Http.Request a
getWithCreds =
    requestWithCreds "Get"


postWithCreds : String -> Decoder a -> Http.Request a
postWithCreds =
    requestWithCreds "Post"


updateLocks : Model -> Cmd Msg
updateLocks oldModel =
    let
        httpRequest d =
            Http.toTask <| getWithCreds (locksUrl oldModel.flags) (decodeModel d)
    in
    attempt NewLocks (Task.andThen httpRequest Date.now)


performLockAction : Flags -> LockAction -> Cmd Msg
performLockAction f a =
    Http.send LockActionDone <| postWithCreds (actionUrl f a) (list string)


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
