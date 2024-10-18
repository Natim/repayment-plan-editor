module Utils.InterestRate exposing (optimal_interest_rate)

import Utils.Newton as Newton


optimal_interest_rate : Float -> List Float -> Maybe Float
optimal_interest_rate loan_amount installments =
    let
        f_sum x =
            List.indexedMap (\index total_amount -> total_amount * (1 / (1 + x)) ^ (toFloat index + 1)) installments
                |> List.sum

        f x =
            loan_amount - f_sum x

        maybe_teg =
            Newton.optimize f
    in
    maybe_teg
