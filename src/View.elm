module View exposing (..)

import DateUtils.Duration exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Models exposing (..)


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
    div [ lockClasses lock ]
        [ p
            [ class "lock-name" ]
            [ span [] [ lockText lock ]
            , span [] [ text " - " ]
            , span [] [ lockActionButton f pool lock ]
            ]
        , p [ class "locked-since" ] [ text lock.lockedSinceStr ]
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
