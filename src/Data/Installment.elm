module Data.Installment exposing
    ( Fields(..)
    , FlagsInstallment
    , Installment
    , decode
    , empty
    , encode
    , fromFlagsInstallment
    )

import Data.Decimal as Decimal exposing (euros)
import Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import String exposing (left)


type Fields
    = Date
    | Amount


type alias FlagsInstallment =
    { date : String
    , cash_flow : String
    , capital_amount : String
    , interest_amount : String
    , commission_amount : String
    , total_left_to_pay : String
    , remaining_capital_to_reimburse : String
    , remaining_capital_to_reimburse_at_start : String
    }


type alias Installment =
    { date : String
    , amount : String
    , cash_flow : Decimal
    , capital_amount : Decimal
    , interest_amount : Decimal
    , commission_amount : Decimal
    , total_left_to_pay : Decimal
    , remaining_capital_to_reimburse : Decimal
    , remaining_capital_to_reimburse_at_start : Decimal
    }


empty : Installment
empty =
    Installment
        ""
        "0.00"
        Decimal.zero
        Decimal.zero
        Decimal.zero
        Decimal.zero
        Decimal.zero
        Decimal.zero
        Decimal.zero


fromFlagsInstallment : FlagsInstallment -> Installment
fromFlagsInstallment flags_installment =
    Installment
        flags_installment.date
        flags_installment.cash_flow
        (Decimal.fromString flags_installment.cash_flow |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.capital_amount |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.interest_amount |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.commission_amount |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.total_left_to_pay |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.remaining_capital_to_reimburse |> Maybe.withDefault Decimal.zero)
        (Decimal.fromString flags_installment.remaining_capital_to_reimburse_at_start |> Maybe.withDefault Decimal.zero)


decode : Decoder Installment
decode =
    Decode.succeed Installment
        |> required "date" Decode.string
        -- We decode it twice, once as a string, once as a Decimal
        |> required "cash_flow" Decode.string
        |> required "cash_flow" Decimal.decode
        -- -------------------------------------------------------
        |> required "capital_amount" Decimal.decode
        |> required "interest_amount" Decimal.decode
        |> required "commission_amount" Decimal.decode
        |> required "total_left_to_pay" Decimal.decode
        |> required "remaining_capital_to_reimburse" Decimal.decode
        |> required "remaining_capital_to_reimburse_at_start" Decimal.decode


encode : Installment -> Encode.Value
encode installment =
    Encode.object
        [ ( "date", Encode.string installment.date )
        , ( "cash_flow", Encode.string installment.amount )
        , ( "capital_amount"
          , installment.capital_amount
                |> euros
                |> Encode.string
          )
        , ( "interest_amount"
          , installment.interest_amount
                |> euros
                |> Encode.string
          )
        , ( "commission_amount"
          , installment.commission_amount
                |> euros
                |> Encode.string
          )
        , ( "total_left_to_pay"
          , installment.total_left_to_pay
                |> euros
                |> Encode.string
          )
        , ( "remaining_capital_to_reimburse"
          , installment.remaining_capital_to_reimburse
                |> euros
                |> Encode.string
          )
        , ( "remaining_capital_to_reimburse_at_start"
          , installment.remaining_capital_to_reimburse_at_start
                |> euros
                |> Encode.string
          )
        ]
