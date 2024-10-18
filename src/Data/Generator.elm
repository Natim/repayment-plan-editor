module Data.Generator exposing (GeneratorFields(..), generate)

import Data.Installment exposing (empty)
import Data.Model exposing (Model)
import Data.RepaymentPlan exposing (RepaymentPlan)
import Decimal
import Iso8601
import Round
import Time exposing (Month(..), Posix, utc)
import Time.Extra as TE exposing (Interval(..))
import Views.Utils exposing (euros, percent)


type GeneratorFields
    = Date
    | Installment
    | Capital
    | Commission


toPosix : String -> Posix
toPosix =
    Iso8601.toTime
        >> Result.withDefault (Time.millisToPosix 0)


toString : Posix -> String
toString =
    Iso8601.fromTime >> String.left 10


schedulePaymentDates : Int -> Posix -> List Posix
schedulePaymentDates installments_count starting_date =
    List.range 0 (installments_count - 1)
        |> List.map (\i -> TE.add Month i utc starting_date)


generate : Model -> RepaymentPlan
generate { capital_amount, commission_percentage, date, installment_amount } =
    let
        starting_date =
            toPosix date

        installments_count =
            Decimal.fastdiv capital_amount installment_amount
                |> Maybe.withDefault Decimal.one
                |> Decimal.toFloat
                |> Round.ceilingNum 0
                |> Round.truncate

        schedule =
            schedulePaymentDates installments_count starting_date
    in
    schedule
        |> List.indexedMap
            (\index installment_date ->
                let
                    markup =
                        Decimal.add Decimal.one commission_percentage

                    total_amount =
                        Decimal.mul markup capital_amount

                    paid =
                        Decimal.mul (Decimal.fromInt index) installment_amount

                    remaining =
                        Decimal.sub total_amount paid

                    cash_flow =
                        if Decimal.toFloat remaining >= Decimal.toFloat installment_amount && index /= installments_count - 1 then
                            installment_amount

                        else
                            remaining

                    capital_share =
                        if index /= installments_count - 1 then
                            Decimal.fastdiv cash_flow markup
                                |> Maybe.withDefault Decimal.zero
                                |> euros
                                |> Decimal.fromString
                                |> Maybe.withDefault Decimal.zero

                        else
                            Decimal.fastdiv cash_flow markup
                                |> Maybe.withDefault Decimal.zero
                                |> Decimal.toFloat
                                |> Round.ceiling 2
                                |> Decimal.fromString
                                |> Maybe.withDefault Decimal.zero

                    commission_share =
                        Decimal.sub cash_flow capital_share

                    remaining_capital_to_reimburse =
                        Decimal.sub remaining_capital_to_reimburse_at_start capital_share
                            |> euros
                            |> Decimal.fromString
                            |> Maybe.withDefault Decimal.zero

                    remaining_capital_to_reimburse_at_start =
                        Decimal.fromInt index
                            |> Decimal.mul capital_share
                            |> Decimal.sub capital_amount
                            |> euros
                            |> Decimal.fromString
                            |> Maybe.withDefault Decimal.zero
                in
                { empty
                    | date = toString installment_date
                    , amount = euros cash_flow
                    , cash_flow = cash_flow
                    , capital_amount = capital_share
                    , commission_amount = commission_share
                    , total_left_to_pay = remaining
                    , remaining_capital_to_reimburse = remaining_capital_to_reimburse
                    , remaining_capital_to_reimburse_at_start = remaining_capital_to_reimburse_at_start
                }
            )
        |> RepaymentPlan
