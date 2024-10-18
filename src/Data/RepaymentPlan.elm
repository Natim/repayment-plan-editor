module Data.RepaymentPlan exposing
    ( FlagsRepaymentPlan
    , RepaymentPlan
    , capitalAmount
    , commissionAmount
    , commissionPercentage
    , decode
    , empty
    , fromFlagsRepaymentPlan
    )

import Data.Decimal as Decimal
import Data.Installment as Installment exposing (FlagsInstallment, Installment)
import Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import String exposing (left)


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
