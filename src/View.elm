module View exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Models exposing (..)


lockClasses : Lock -> Attribute msg
lockClasses lock =
    classList [ ( "lock", True ), ( stateName lock.state, True ) ]


stateName : LockState -> String
stateName state =
    case state of
        Claimed _ ->
            "Claimed"

        Unclaimed ->
            "Unclaimed"

        Recycling _ ->
            "Recycling"

        WaitingToRecycle _ ->
            "WaitingToRecycle"


lockText : Lock -> Html msg
lockText lock =
    text (lock.name ++ " - " ++ stateName lock.state)


lockAction : Pool -> Lock -> LockAction
lockAction pool lock =
    case lock.state of
        Claimed _ ->
            Recycle pool lock

        Unclaimed ->
            Claim pool lock

        WaitingToRecycle _ ->
            NoAction

        Recycling _ ->
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


lockedBy : LockOwner -> String
lockedBy o =
    case o of
        Pipeline p ->
            p.pipeline ++ "/" ++ p.job ++ "#" ++ toString p.buildNumber

        Committer c ->
            c

        GafferUser u ->
            u


lockedByText : Lock -> Html Msg
lockedByText lock =
    case lock.state of
        Unclaimed ->
            p [ class "locked-by", class "hidden" ] []

        Claimed c ->
            p [ class "locked-by" ] [ text <| lockedBy c.owner ]

        Recycling _ ->
            p [ class "locked-by", class "hidden" ] []

        WaitingToRecycle _ ->
            p [ class "locked-by", class "hidden" ] []


lockedSinceElement : String -> Html Msg
lockedSinceElement lockedSince =
    p [ class "locked-since" ] [ text lockedSince ]


lockedSinceText : Lock -> Html Msg
lockedSinceText lock =
    case lock.state of
        Unclaimed ->
            p [ class "locked-since", class "hidden" ] []

        Claimed c ->
            lockedSinceElement c.sinceStr

        Recycling r ->
            lockedSinceElement r.sinceStr

        WaitingToRecycle w ->
            lockedSinceElement w.sinceStr


lockView : Flags -> Pool -> Lock -> Html Msg
lockView f pool lock =
    div [ lockClasses lock ]
        [ p
            [ class "lock-name" ]
            [ span [] [ lockText lock ]
            , span [] [ text " - " ]
            , span [] [ lockActionButton f pool lock ]
            ]
        , lockedByText lock
        , lockedSinceText lock
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
