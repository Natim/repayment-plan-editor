module Data.Flags exposing (Flags)

import Data.RepaymentPlan exposing (FlagsRepaymentPlan)


type alias Flags =
    { repayment_plan : FlagsRepaymentPlan
    }
