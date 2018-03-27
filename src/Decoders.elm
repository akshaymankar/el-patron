module Decoders exposing (..)

import Date exposing (..)
import Date.Extra.Duration exposing (..)
import DateUtils.Duration exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Models exposing (..)


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


decodeLock : Date -> Decoder Lock
decodeLock d =
    decode Lock
        |> required "name" string
        |> required "state" decodeLockState
        |> required "lockedSince" dateDecoder
        |> required "lockedSince" (agoDecoder d)


decodeModel : Date -> Decoder Pools
decodeModel d =
    dict <| list (decodeLock d)


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    decode ErrorMessage |> required "message" string
