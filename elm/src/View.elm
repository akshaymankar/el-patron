module View exposing (..)

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
            if pool.hasLifecycle then
                Recycle pool lock

            else
                Unclaim pool lock

        Unclaimed ->
            Claim pool lock

        WaitingToRecycle _ ->
            NoAction

        Recycling _ ->
            Unclaim { pool | name = pool.name ++ "-lifecycle" } lock


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


lockActionButton : Pool -> Lock -> Html Msg
lockActionButton pool lock =
    let
        action =
            lockAction pool lock
    in
    a [ href "#", onClick (PerformLockAction action) ] [ text (toSymbol action) ]


lockedBy : LockOwner -> String
lockedBy o =
    case o of
        Pipeline p ->
            p.pipeline ++ "/" ++ p.job ++ "#" ++ String.fromInt p.buildNumber

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


lockTextAndMaybeButton : Config -> Pool -> Lock -> List (Html Msg)
lockTextAndMaybeButton c pool lock =
    let
        lt =
            span [] [ lockText lock ]
    in
    if c.disableActionButtons then
        [ lt
        , span [] [ text " - " ]
        , span [] [ lockActionButton pool lock ]
        ]

    else
        [ lt ]


lockView : Config -> Pool -> Lock -> Html Msg
lockView c pool lock =
    div [ lockClasses lock ]
        [ p
            [ class "lock-name" ]
            (lockTextAndMaybeButton c pool lock)
        , lockedByText lock
        , lockedSinceText lock
        ]


locksView : Config -> Pool -> List Lock -> Html Msg
locksView c pool locks =
    div [] (List.map (lockView c pool) locks)


poolView : Config -> ( Pool, List Lock ) -> Html Msg
poolView c ( pool, locks ) =
    div [ class "item" ] [ p [ class "pool" ] [ text pool.name ], locksView c pool locks ]


poolsView : Model -> List (Html Msg)
poolsView model =
    List.map (poolView model.config) model.pools


errorView : Model -> Html Msg
errorView model =
    case model.error of
        Nothing ->
            div [] []

        Just (FailedToLoadLocks err) ->
            div [] [ text <| "Failed to load locks, due to error: " ++ err ]

        Just FailedToDoLockAction ->
            div [] [ text <| "Failed to fulfill the request, please try again" ]


view : Model -> Html Msg
view model =
    div []
        [ div [ classList [ ( "loader-wrapper", True ), ( "hidden", not model.loading ) ] ]
            [ div [ class "lds-ring" ] [ div [] [] ] ]
        , div [ class "" ] [ errorView model ]
        , div [ class "masonry" ] (poolsView model)
        ]
