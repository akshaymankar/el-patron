module DecodersTest exposing (..)

import Date exposing (..)
import Decoders exposing (..)
import Expect
import Json.Decode exposing (..)
import Test exposing (..)


all : Test
all =
    describe "decoders"
        [ describe "decodePool"
            [ test "decode simple pool" <|
                \() ->
                    let
                        input =
                            """
                    {"poolName": "pool1", "poolHasLifecycle": true}
                    """

                        decodedOutput =
                            decodeString decodePool input
                    in
                    Expect.equal decodedOutput (Ok { name = "pool1", hasLifecycle = True })
            ]
        , describe "decodeModel"
            [ test "decode a sample model" <|
                \() ->
                    let
                        input =
                            """
[
  [
    {
      "poolName":"pool1",
      "poolHasLifecycle": true
    },
    [
      {
        "state":{
          "owner":{
            "username":"akshaymankar",
            "type":"GafferUser"
          },
          "name":"Claimed",
          "since":"2018-09-03T16:27:30Z"
        },
        "name":"lock1"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock2"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock3"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-20T02:18:41Z"
        },
        "name":"lock4"
      }
    ]
  ],
  [
    {
      "poolName":"pool2",
      "poolHasLifecycle": true
    },
    [
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock1"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock2"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock3"
      },
      {
        "state":{
          "name":"Unclaimed"
        },
        "name":"lock4"
      }
    ]
  ],
  [
    {
      "poolName":"pool3",
      "poolHasLifecycle": true
    },
    [
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock1"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock2"
      },
      {
        "state":{
          "owner":{
            "committer":"Akshay Mankar",
            "type":"Committer"
          },
          "name":"Claimed",
          "since":"2018-03-16T19:15:50Z"
        },
        "name":"lock3"
      },
      {
        "state":{
          "name":"Unclaimed"
        },
        "name":"lock4"
      }
    ]
  ]
]
   """

                        decodedOutput =
                            decodeString (decodeModel (fromTime 0)) input
                    in
                    case decodedOutput of
                        Ok _ ->
                            Expect.pass

                        Err err ->
                            Expect.fail err
            ]
        ]
