module Views.RepaymentPlan exposing (form, generator, summary)

import Data.Generator as Generator exposing (GeneratorFields(..))
import Data.Installment as Installment exposing (Fields(..), Installment)
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.RepaymentPlan as RepaymentPlan exposing (RepaymentPlan)
import Decimal
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Views.Utils exposing (euros, percent)


generator : Model -> Html Msg
generator { markup, capital, date, installment } =
    div []
        [ p []
            [ label [ style "width" "200px" ]
                [ text "Capital: "
                ]
            , input [ type_ "text", style "text-align" "center", UpdateGenerator Generator.Capital |> onInput, value capital ] []
            ]
        , p []
            [ label [ style "width" "200px" ]
                [ text "Commission percentage: "
                ]
            , input [ type_ "text", style "text-align" "center", UpdateGenerator Generator.Commission |> onInput, value markup ] []
            ]
        , p []
            [ label [ style "width" "200px" ]
                [ text "First installment date: "
                ]
            , input [ type_ "text", style "text-align" "center", UpdateGenerator Generator.Date |> onInput, value date ] []
            ]
        , p []
            [ label [ style "width" "200px" ]
                [ text "Installment amount: "
                ]
            , input [ type_ "text", style "text-align" "center", UpdateGenerator Generator.Installment |> onInput, value installment ] []
            ]
        , p [] [ button [ onClick Generate, type_ "button" ] [ text "Generate" ] ]
        ]


form : RepaymentPlan -> Html Msg
form repayment_plan =
    List.concat
        [ repayment_plan.rows |> List.indexedMap installmentForm
        , [ button [ onClick AddInstallment ] [ text "+ Add line" ] ]
        ]
        |> div []


installmentForm : Int -> Installment -> Html Msg
installmentForm index installment =
    Html.form []
        [ label [] [ input [ type_ "text", style "text-align" "center", UpdateInstallment index Installment.Date |> onInput, value installment.date ] [] ]
        , label [] [ input [ type_ "text", style "text-align" "center", UpdateInstallment index Installment.Amount |> onInput, installment.amount |> value ] [] ]
        , label [] [ input [ type_ "text", style "text-align" "center", disabled True, installment.capital_amount |> euros |> value ] [] ]
        , label [] [ input [ type_ "text", style "text-align" "center", disabled True, installment.commission_amount |> euros |> value ] [] ]
        , button [ type_ "button", DelInstallment index |> onClick ] [ text "-" ]
        ]


summary : RepaymentPlan -> Html Msg
summary repayment_plan =
    let
        capital_amount =
            RepaymentPlan.capitalAmount repayment_plan

        commission_amount =
            RepaymentPlan.commissionAmount repayment_plan

        commission_percentage =
            RepaymentPlan.commissionPercentage repayment_plan
    in
    p [ style "margin" "10px" ]
        [ ul []
            [ li []
                [ text "Capital Amount : "
                , capital_amount |> euros |> text
                , text " €"
                ]
            , li []
                [ text "Commission Amount : "
                , commission_amount |> euros |> text
                , text " €"
                ]
            , li []
                [ text "Commission Percentage : "
                , Decimal.fromInt 100
                    |> Decimal.mul commission_percentage
                    |> percent
                    |> text
                , text " %"
                ]
            ]
        ]
