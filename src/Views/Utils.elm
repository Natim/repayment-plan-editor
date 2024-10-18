module Views.Utils exposing (euros, percent)

import Decimal exposing (Decimal)
import Round


euros : Decimal -> String
euros cents =
    let
        amount =
            cents
                |> Decimal.toFloat
                |> Round.round 2
    in
    amount


percent : Decimal -> String
percent cents =
    let
        amount =
            cents
                |> Decimal.toFloat
                |> Round.round 4
    in
    amount
