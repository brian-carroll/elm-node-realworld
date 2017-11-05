module Tests.Models exposing (all)

import Test exposing (describe)
import Tests.Models.Article


all =
    describe "Models"
        [ Tests.Models.Article.all
        ]
