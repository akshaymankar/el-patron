module DecodersTest exposing (..)

import Date exposing (..)
import Decoders exposing (..)
import Expect
import Json.Decode exposing (..)
import Test exposing (..)


-- [[{"poolName":"kubo-aws"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-20T02:09:03Z"},"name":"aws-artichoke"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-20T02:15:44Z"},"name":"aws-broccoli"},{"state":{"name":"WaitingToRecycle","since":"2018-03-20T02:21:20Z"},"name":"aws-cauliflower"}]],[{"poolName":"kubo-aws-lb"},[{"state":{"name":"Unclaimed"},"name":"aws-badger"},{"state":{"name":"Unclaimed"},"name":"aws-alpaca"},{"state":{"name":"Unclaimed"},"name":"aws-chipmunk"}]],[{"poolName":"kubo-gcp"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-drum"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-baldwin"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-cyclone"},{"state":{"name":"WaitingToRecycle","since":"2018-03-20T19:54:37Z"},"name":"gcp-arsenal"},{"state":{"name":"WaitingToRecycle","since":"2018-04-10T21:00:42Z"},"name":"gcp-flute"},{"state":{"name":"WaitingToRecycle","since":"2018-03-16T19:15:50Z"},"name":"foo1"},{"state":{"name":"WaitingToRecycle","since":"2018-03-20T02:28:55Z"},"name":"foo"},{"state":{"name":"WaitingToRecycle","since":"2018-03-20T22:58:14Z"},"name":"gcp-east"}]],[{"poolName":"kubo-gcp-lb"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-battery"},{"state":{"name":"Unclaimed"},"name":"gcp-flounder"},{"state":{"name":"Unclaimed"},"name":"gcp-eel"},{"state":{"name":"Unclaimed"},"name":"gcp-devilray"},{"state":{"name":"WaitingToRecycle","since":"2018-03-30T01:41:19Z"},"name":"gcp-tuna"},{"state":{"name":"WaitingToRecycle","since":"2018-08-14T22:49:01Z"},"name":"gcp-camera"},{"state":{"name":"WaitingToRecycle","since":"2018-03-31T20:23:20Z"},"name":"gcp-abacus"}]],[{"poolName":"kubo-openstack"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"openstack-flank"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-20T22:19:33Z"},"name":"openstack-duck"},{"state":{"owner":{"username":"akshaymankar","type":"GafferUser"},"name":"Claimed","since":"2018-04-10T21:04:59Z"},"name":"openstack-elk"}]],[{"poolName":"kubo-openstack-lb"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"openstack-chicken"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"openstack-beef"},{"state":{"name":"Unclaimed"},"name":"openstack-angus"}]],[{"poolName":"kubo-vsphere"},[{"state":{"name":"Unclaimed"},"name":"vsphere-baffin"},{"state":{"name":"Unclaimed"},"name":"vsphere-abbey"},{"state":{"name":"Unclaimed"},"name":"vsphere-caribou"}]],[{"poolName":"kubo-vsphere-lb"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"vsphere-airbus"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"vsphere-boeing"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"vsphere-cessna"}]],[{"poolName":"pks-api"},[{"state":{"name":"Unclaimed"},"name":"pipeline.yml"}]],[{"poolName":"pks-gcp-om"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-mumbai"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"gcp-dospalos"},{"state":{"name":"Unclaimed"},"name":"gcp-healdsburg"},{"state":{"name":"Unclaimed"},"name":"gcp-menlopark"}]],[{"poolName":"pks-vsphere-om"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-20T02:18:58Z"},"name":"vsphere-diglett"},{"state":{"name":"Unclaimed"},"name":"vsphere-lobster"},{"state":{"name":"Unclaimed"},"name":"vsphere-tentacool"},{"state":{"name":"Unclaimed"},"name":"vsphere-gopher"}]],[{"poolName":"pool1"},[{"state":{"owner":{"username":"akshaymankar","type":"GafferUser"},"name":"Claimed","since":"2018-09-03T16:27:30Z"},"name":"lock1"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock2"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock3"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-20T02:18:41Z"},"name":"lock4"}]],[{"poolName":"pool2"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock1"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock2"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock3"},{"state":{"name":"Unclaimed"},"name":"lock4"}]],[{"poolName":"pool3"},[{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock1"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock2"},{"state":{"owner":{"committer":"Akshay Mankar\n","type":"Committer"},"name":"Claimed","since":"2018-03-16T19:15:50Z"},"name":"lock3"},{"state":{"name":"Unclaimed"},"name":"lock4"}]]]


all : Test
all =
    describe "decoders"
        [ describe "decodePool"
            [ test "decode simple pool" <|
                \() ->
                    let
                        input =
                            """
                    {"poolName": "pool1"}
                    """

                        decodedOutput =
                            decodeString decodePool input
                    in
                    Expect.equal decodedOutput (Ok { name = "pool1" })
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
      "poolName":"pool1"
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
      "poolName":"pool2"
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
      "poolName":"pool3"
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
