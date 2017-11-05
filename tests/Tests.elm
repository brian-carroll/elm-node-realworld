module Tests exposing (..)

import Test exposing (..)
import Tests.Models


all : Test
all =
    describe "Elm tests"
        [ Tests.Models.all
        ]
