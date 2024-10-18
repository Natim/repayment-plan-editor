module Data.Model exposing (Model)

import Data.RepaymentPlan exposing (RepaymentPlan)
import Decimal exposing (Decimal)


type alias Model =
    { repayment_plan : RepaymentPlan
    , commission_percentage : Decimal
    , capital_amount : Decimal
    , installment_amount : Decimal
    , markup : String
    , capital : String
    , date : String
    , installment : String
    }
