module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav
import Decoders exposing (..)
import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Models exposing (..)
import Task exposing (..)
import Time
import Urls exposing (..)
import View exposing (..)


main : Program Flags Model Msg
main =
    Browser.element
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


requestWithCreds : String -> String -> Decoder a -> Task BetterHttpError a
requestWithCreds method url decoder =
    Http.riskyTask
        { method = method
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , resolver = decoderToResolver decoder
        , timeout = Nothing
        }


getWithCreds : String -> Decoder a -> Task BetterHttpError a
getWithCreds =
    requestWithCreds "Get"


postWithCreds : String -> Decoder a -> Task BetterHttpError a
postWithCreds =
    requestWithCreds "Post"


decoderToResolver : Decoder a -> Http.Resolver BetterHttpError a
decoderToResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata body)

                Http.GoodStatus_ metadata body ->
                    case decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (BadBody (errorToString err))
        )


getTask : String -> Decoder a -> Task BetterHttpError a
getTask url decoder =
    Http.task
        { method = "Get"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = decoderToResolver decoder
        , timeout = Nothing
        }


updateLocks : Model -> Cmd Msg
updateLocks oldModel =
    let
        getLocksRequest d =
            getWithCreds (locksUrl oldModel.flags) (decodeModel d)
    in
    attempt NewLocks <|
        Task.map2 (\pools config -> ( pools, config ))
            (Task.andThen getLocksRequest Time.now)
            (getTask configUrl decodeConfig)


performLockAction : Flags -> LockAction -> Cmd Msg
performLockAction f a =
    Task.attempt LockActionDone <|
        postWithCreds (actionUrl f a) (list string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        NewLocks (Ok ( newLocks, newConfig )) ->
            ( { model | pools = newLocks, config = newConfig, loading = False }
            , Cmd.none
            )

        NewLocks (Err (BadStatus metadata body)) ->
            if metadata.statusCode == 403 then
                case decodeString decodeErrorMessage body of
                    Ok (ErrorMessage "Permission Denied. User not logged in.") ->
                        ( model, Nav.load (authUrl model.flags) )

                    otherwise ->
                        ( { model
                            | error = Just <| FailedToLoadLocks <| "403 Unauthorized: " ++ body
                            , loading = False
                          }
                        , Cmd.none
                        )

            else
                ( { model
                    | error = Just <| FailedToLoadLocks <| "Bad Status: " ++ String.fromInt metadata.statusCode
                    , loading = False
                  }
                , Cmd.none
                )

        NewLocks (Err (BadBody body)) ->
            ( { model
                | error = Just <| FailedToLoadLocks <| "Bad Body: " ++ body
                , loading = False
              }
            , Cmd.none
            )

        NewLocks (Err NetworkError) ->
            ( { model
                | error = Just <| FailedToLoadLocks "Network Error"
                , loading = False
              }
            , Cmd.none
            )

        NewLocks (Err Timeout) ->
            ( { model
                | error = Just <| FailedToLoadLocks "Timed out"
                , loading = False
              }
            , Cmd.none
            )

        NewLocks (Err (BadUrl url)) ->
            ( { model
                | error = Just <| FailedToLoadLocks <| "Bad url: " ++ url ++ ", please report this as a bug"
                , loading = False
              }
            , Cmd.none
            )

        PerformLockAction a ->
            ( { model | loading = True }, performLockAction model.flags a )

        LockActionDone (Ok _) ->
            ( model, updateLocks model )

        LockActionDone (Err _) ->
            ( { model
                | error = Just FailedToDoLockAction
                , loading = False
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
