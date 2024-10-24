module Data.Model exposing (Model)

import Data.RepaymentPlan exposing (RepaymentPlan)
import Decimal exposing (Decimal)


type alias Model =
    { repayment_plan : RepaymentPlan
    , commission_percentage : Decimal
    , left_to_pay_amount : Decimal
    , installment_amount : Decimal
    , markup : String
    , left_to_pay : String
    , date : String
    , installment : String
    }
