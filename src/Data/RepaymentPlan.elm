module Data.RepaymentPlan exposing
    ( FlagsRepaymentPlan
    , RepaymentPlan
    , capitalAmount
    , commissionAmount
    , commissionPercentage
    , decode
    , empty
    , encode
    , fromFlagsRepaymentPlan
    )

import Data.Decimal as Decimal
import Data.Installment as Installment exposing (FlagsInstallment, Installment)
import Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Round
import String exposing (left)
import Utils.InterestRate exposing (optimal_interest_rate)
import Views.Utils exposing (euros, percent)


type alias FlagsRepaymentPlan =
    { rows : List FlagsInstallment }


type alias RepaymentPlan =
    { rows : List Installment }


empty : RepaymentPlan
empty =
    RepaymentPlan []


fromFlagsRepaymentPlan : FlagsRepaymentPlan -> RepaymentPlan
fromFlagsRepaymentPlan flags_fee_plan =
    RepaymentPlan
        (List.map Installment.fromFlagsInstallment flags_fee_plan.rows)


decode : Decoder RepaymentPlan
decode =
    Decode.succeed RepaymentPlan
        |> required "rows" (Decode.list Installment.decode)


totalAmount : RepaymentPlan -> Decimal
totalAmount repayment_plan =
    repayment_plan.rows |> List.map .cash_flow |> List.foldr Decimal.add Decimal.zero


capitalAmount : RepaymentPlan -> Decimal
capitalAmount repayment_plan =
    repayment_plan.rows |> List.map .capital_amount |> List.foldr Decimal.add Decimal.zero


commissionAmount : RepaymentPlan -> Decimal
commissionAmount repayment_plan =
    repayment_plan.rows |> List.map .commission_amount |> List.foldr Decimal.add Decimal.zero


commissionPercentage : RepaymentPlan -> Decimal
commissionPercentage repayment_plan =
    let
        capital_amount =
            capitalAmount repayment_plan

        commission_amount =
            commissionAmount repayment_plan
    in
    Decimal.fastdiv commission_amount capital_amount
        |> Maybe.withDefault Decimal.zero


encode : RepaymentPlan -> Encode.Value
encode repayment_plan =
    let
        loan_amount =
            capitalAmount repayment_plan
                |> Decimal.toFloat
                |> Debug.log "loan_amount"

        installments =
            List.map (.cash_flow >> Decimal.toFloat) repayment_plan.rows
                |> Debug.log "installments"
    in
    Encode.object
        [ ( "rows", Encode.list Installment.encode repayment_plan.rows )
        , ( "initial_cash_flow"
          , capitalAmount repayment_plan
                |> Decimal.mul Decimal.minusOne
                |> euros
                |> Encode.string
          )
        , ( "total_cost_of_credit"
          , commissionAmount repayment_plan
                |> euros
                |> Encode.string
          )
        , ( "total_interest_amount", Encode.string "0.00" )
        , ( "overall_effective_rate"
          , optimal_interest_rate loan_amount installments
                |> Maybe.withDefault 0
                |> (*) 1200
                |> Round.round 2
                |> Encode.string
          )
        , ( "total_commission_amount"
          , commissionAmount repayment_plan
                |> euros
                |> Encode.string
          )
        , ( "fixed_commission_on_initial_amount", Encode.string "0.00" )
        , ( "total_cost_of_credit_as_percentage"
          , Decimal.mul (Decimal.fromInt 100) (commissionPercentage repayment_plan)
                |> percent
                |> Encode.string
          )
        , ( "fixed_commission_on_capital_percent", Encode.string "0.00" )
        , ( "fixed_commission_on_initial_amount_percent", Encode.string "0.00" )
        ]
