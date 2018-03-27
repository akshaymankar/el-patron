module DateUtils.DurationTest exposing (..)

import Date.Extra.Duration exposing (..)
import DateUtils.Duration exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "ago"
        [ describe "few seconds ago"
            [ test "0:00" <|
                \_ ->
                    Expect.equal "few seconds ago" (ago { zeroDelta | second = 0 })
            , test "0:59" <|
                \_ ->
                    Expect.equal "few seconds ago" (ago { zeroDelta | second = 59 })
            ]
        , describe "min ago"
            [ test "1:00" <|
                \_ ->
                    Expect.equal "1 min ago" (ago { zeroDelta | minute = 1, second = 0 })
            , test "3:20" <|
                \_ ->
                    Expect.equal "3 min ago" (ago { zeroDelta | minute = 3, second = 20 })
            ]
        , describe "hour(s) ago"
            [ test "3:33:33" <|
                \_ ->
                    Expect.equal "3 hours ago" (ago { zeroDelta | hour = 3, minute = 33, second = 33 })
            , test "1:20:22" <|
                \_ ->
                    Expect.equal "1 hour ago" (ago { zeroDelta | hour = 1, second = 20, second = 22 })
            ]
        , describe "day(s) ago"
            [ test "1 day" <|
                \_ ->
                    Expect.equal "1 day ago" (ago { zeroDelta | day = 1 })
            , test "4 days" <|
                \_ ->
                    Expect.equal "4 days ago" (ago { zeroDelta | day = 4 })
            ]
        , describe "month(s) ago"
            [ test "1 month" <|
                \_ ->
                    Expect.equal "1 month ago" (ago { zeroDelta | month = 1 })
            , test "4 months" <|
                \_ ->
                    Expect.equal "4 months ago" (ago { zeroDelta | month = 4 })
            ]
        , describe "year(s) ago"
            [ test "1 year" <|
                \_ ->
                    Expect.equal "1 year ago" (ago { zeroDelta | year = 1 })
            , test "4 year" <|
                \_ ->
                    Expect.equal "4 years ago" (ago { zeroDelta | year = 4 })
            ]
        ]
