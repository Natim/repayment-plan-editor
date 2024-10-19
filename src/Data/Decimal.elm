module Data.Decimal exposing (decode, euros, percent)

import Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)
import Round


decode : Decoder Decimal
decode =
    Decode.string
        |> Decode.andThen
            (\value ->
                Decimal.fromString value
                    |> Maybe.withDefault Decimal.zero
                    |> Decode.succeed
            )


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
