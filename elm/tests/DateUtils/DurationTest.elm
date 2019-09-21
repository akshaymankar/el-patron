module DateUtils.DurationTest exposing (..)

import DateUtils.Duration exposing (..)
import Expect
import Test exposing (..)


second =
    1000


minute =
    60 * second


hour =
    60 * minute


day =
    24 * hour


all : Test
all =
    describe "ago"
        [ describe "few seconds ago"
            [ test "0:00" <|
                \_ ->
                    Expect.equal "few seconds ago" (ago 0)
            , test "0:59" <|
                \_ ->
                    Expect.equal "few seconds ago" (ago (59 * second))
            ]
        , describe "min ago"
            [ test "1:00" <|
                \_ ->
                    Expect.equal "1 min ago" (ago minute)
            , test "3:20" <|
                \_ ->
                    Expect.equal "3 min ago" (ago (3 * minute + 20 * second))
            ]
        , describe "hour(s) ago"
            [ test "3:33:33" <|
                \_ ->
                    Expect.equal "3 hours ago" (ago (3 * hour + 33 * minute + 33 * second))
            , test "1:20:22" <|
                \_ ->
                    Expect.equal "1 hour ago" (ago (1 * hour + 20 * minute + 22 * second))
            ]
        , describe "day(s) ago"
            [ test "1 day" <|
                \_ ->
                    Expect.equal "1 day ago" (ago (1 * day))
            , test "4 days" <|
                \_ ->
                    Expect.equal "4 days ago" (ago (4 * day))
            ]
        -- , describe "month(s) ago"
        --     [ test "1 month" <|
        --         \_ ->
        --             Expect.equal "1 month ago" (ago { zeroDelta | month = 1 })
        --     , test "4 months" <|
        --         \_ ->
        --             Expect.equal "4 months ago" (ago { zeroDelta | month = 4 })
        --     ]
        -- , describe "year(s) ago"
        --     [ test "1 year" <|
        --         \_ ->
        --             Expect.equal "1 year ago" (ago { zeroDelta | year = 1 })
        --     , test "4 year" <|
        --         \_ ->
        --             Expect.equal "4 years ago" (ago { zeroDelta | year = 4 })
        --     ]
        ]
