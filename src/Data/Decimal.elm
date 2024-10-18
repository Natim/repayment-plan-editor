module Data.Decimal exposing (decode)

import Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)


decode : Decoder Decimal
decode =
    Decode.string
        |> Decode.andThen
            (\value ->
                Decimal.fromString value
                    |> Maybe.withDefault Decimal.zero
                    |> Decode.succeed
            )
