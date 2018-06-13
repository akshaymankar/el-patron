module Decoders exposing (..)

import Date exposing (..)
import Date.Extra.Duration exposing (..)
import DateUtils.Duration exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models exposing (..)


timesAndOwnerDecoder : Date -> Decoder TimesAndOwner
timesAndOwnerDecoder d =
    decode TimesAndOwner
        |> required "since" dateDecoder
        |> required "since" (agoDecoder d)
        |> required "owner" ownerDecoder


timesDecoder : Date -> Decoder Times
timesDecoder d =
    decode Times
        |> required "since" dateDecoder
        |> required "since" (agoDecoder d)


decodeLockState : Date -> Decoder LockState
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


dateDecoder : Decoder Date
dateDecoder =
    string
        |> Json.Decode.andThen
            (\dateString ->
                case fromString dateString of
                    Ok date ->
                        Json.Decode.succeed date

                    Err errorString ->
                        Json.Decode.fail errorString
            )


agoDecoder : Date -> Decoder String
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
                            (decode PipelineDetails
                                |> required "pipeline" string
                                |> required "job" string
                                |> required "buildNumber" int
                            )

                    "Committer" ->
                        decode Committer |> required "committer" string

                    "GafferUser" ->
                        decode GafferUser |> required "username" string

                    somethingElse ->
                        fail <| "Unknown lock owner type: " ++ somethingElse
            )


decodeLock : Date -> Decoder Lock
decodeLock d =
    decode Lock
        |> required "name" string
        |> required "state" (decodeLockState d)


decodeModel : Date -> Decoder Pools
decodeModel d =
    dict <| list (decodeLock d)


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    decode ErrorMessage |> required "message" string
