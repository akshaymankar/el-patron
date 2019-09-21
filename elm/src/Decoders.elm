module Decoders exposing (..)

import DateUtils.Duration exposing (..)
import Dict exposing (fromList)
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models exposing (..)
import Parser
import Time


timesAndOwnerDecoder : Time.Posix -> Decoder TimesAndOwner
timesAndOwnerDecoder d =
    succeed TimesAndOwner
        |> required "since" dateDecoder
        |> required "since" (agoDecoder d)
        |> required "owner" ownerDecoder


timesDecoder : Time.Posix -> Decoder Times
timesDecoder d =
    succeed Times
        |> required "since" dateDecoder
        |> required "since" (agoDecoder d)


decodeLockState : Time.Posix -> Decoder LockState
decodeLockState d =
    field "name" string
        |> andThen
            (\str ->
                case str of
                    "Claimed" ->
                        map Claimed (timesAndOwnerDecoder d)

                    "Unclaimed" ->
                        succeed Unclaimed

                    "Recycling" ->
                        map Recycling (timesDecoder d)

                    "WaitingToRecycle" ->
                        map WaitingToRecycle (timesDecoder d)

                    somethingElse ->
                        fail <| "Unknown lock state: " ++ somethingElse
            )


dateDecoder : Decoder Time.Posix
dateDecoder =
    string
        |> Json.Decode.andThen
            (\dateString ->
                case Iso8601.toTime dateString of
                    Ok date ->
                        Json.Decode.succeed date

                    Err deadEnds ->
                        Json.Decode.fail (Parser.deadEndsToString deadEnds)
            )


agoDecoder : Time.Posix -> Decoder String
agoDecoder d =
    Json.Decode.map (ago << diff d) dateDecoder


ownerDecoder : Decoder LockOwner
ownerDecoder =
    field "type" string
        |> andThen
            (\t ->
                case t of
                    "Pipeline" ->
                        map Pipeline
                            (succeed PipelineDetails
                                |> required "pipeline" string
                                |> required "job" string
                                |> required "buildNumber" int
                            )

                    "Committer" ->
                        succeed Committer |> required "committer" string

                    "GafferUser" ->
                        succeed GafferUser |> required "username" string

                    somethingElse ->
                        fail <| "Unknown lock owner type: " ++ somethingElse
            )


decodeLock : Time.Posix -> Decoder Lock
decodeLock d =
    succeed Lock
        |> required "name" string
        |> required "state" (decodeLockState d)


decodeModel : Time.Posix -> Decoder Pools
decodeModel d =
    list (decodeTuple d)


decodeTuple : Time.Posix -> Decoder ( Pool, List Lock )
decodeTuple d =
    map2 makeTuple (index 0 decodePool) (index 1 (list (decodeLock d)))


makeTuple : a -> b -> ( a, b )
makeTuple a b =
    ( a, b )


decodePool : Decoder Pool
decodePool =
    succeed Pool
        |> required "poolName" string
        |> required "poolHasLifecycle" bool


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    succeed ErrorMessage |> required "message" string


decodeConfig : Decoder Config
decodeConfig =
    succeed Config
        |> required "disableActionButtons" bool
