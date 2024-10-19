module RepaymentPlanEditor exposing (main)

import Browser
import Data.Decimal exposing (euros, percent)
import Data.Flags exposing (Flags)
import Data.Generator as Generator exposing (GeneratorFields(..))
import Data.Installment as Installment exposing (Fields(..))
import Data.Model exposing (Model)
import Data.Msg exposing (Msg(..))
import Data.RepaymentPlan as RepaymentPlan
import Decimal
import Html exposing (Html, div)
import Json.Decode exposing (decodeValue)
import Views.RepaymentPlan as RepaymentPlan


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        repayment_plan =
            RepaymentPlan.fromFlagsRepaymentPlan flags.repayment_plan

        first_installment =
            case repayment_plan.rows of
                f :: l ->
                    f

                [] ->
                    Installment.empty
    in
    ( { repayment_plan = repayment_plan
      , commission_percentage = RepaymentPlan.commissionPercentage repayment_plan
      , capital_amount = RepaymentPlan.capitalAmount repayment_plan
      , installment_amount = first_installment.cash_flow
      , markup = RepaymentPlan.commissionPercentage repayment_plan |> Decimal.mul (Decimal.fromInt 100) |> percent
      , capital = RepaymentPlan.capitalAmount repayment_plan |> euros
      , date = first_installment.date
      , installment = first_installment.amount
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ repayment_plan, commission_percentage } as model) =
    case msg of
        AddInstallment ->
            let
                new_rows =
                    List.append model.repayment_plan.rows [ Installment.empty ]

                new_repayment_plan =
                    { repayment_plan | rows = new_rows }
            in
            ( { model | repayment_plan = new_repayment_plan }, Cmd.none )

        DelInstallment index ->
            let
                new_rows =
                    model.repayment_plan.rows
                        |> List.indexedMap
                            (\i x ->
                                if i /= index then
                                    Just x

                                else
                                    Nothing
                            )
                        |> List.filterMap identity

                new_repayment_plan =
                    { repayment_plan | rows = new_rows }
            in
            ( { model | repayment_plan = new_repayment_plan }, Cmd.none )

        UpdateInstallment index field value ->
            let
                new_rows =
                    model.repayment_plan.rows
                        |> List.indexedMap
                            (\i x ->
                                if i /= index then
                                    Just x

                                else
                                    case field of
                                        Installment.Date ->
                                            Just { x | date = value }

                                        Installment.Amount ->
                                            case Decimal.fromString value of
                                                Just decimalValue ->
                                                    let
                                                        capital_amount =
                                                            Decimal.add Decimal.one commission_percentage
                                                                |> Decimal.fastdiv decimalValue
                                                                |> Maybe.withDefault Decimal.zero
                                                                |> Decimal.round 2

                                                        commission_amount =
                                                            Decimal.sub decimalValue capital_amount
                                                    in
                                                    Just
                                                        { x
                                                            | amount = value
                                                            , cash_flow = decimalValue
                                                            , capital_amount = capital_amount
                                                            , commission_amount = commission_amount
                                                        }

                                                Nothing ->
                                                    Just { x | amount = value }
                            )
                        |> List.filterMap identity

                new_repayment_plan =
                    { repayment_plan | rows = new_rows }
            in
            ( { model | repayment_plan = new_repayment_plan }, Cmd.none )

        UpdateGenerator field value ->
            case field of
                Generator.Date ->
                    ( { model | date = value }, Cmd.none )

                Generator.Installment ->
                    ( { model
                        | installment = value
                        , installment_amount =
                            Decimal.fromString value
                                |> Maybe.withDefault Decimal.zero
                      }
                    , Cmd.none
                    )

                Generator.Capital ->
                    ( { model
                        | capital = value
                        , capital_amount =
                            Decimal.fromString value
                                |> Maybe.withDefault Decimal.zero
                      }
                    , Cmd.none
                    )

                Generator.Commission ->
                    let
                        decimalValue =
                            Decimal.fromString value
                                |> Maybe.withDefault Decimal.zero
                    in
                    ( { model
                        | markup = value
                        , commission_percentage =
                            Decimal.fastdiv decimalValue (Decimal.fromInt 100)
                                |> Maybe.withDefault Decimal.zero
                      }
                    , Cmd.none
                    )

        Generate ->
            ( { model | repayment_plan = Generator.generate model }, Cmd.none )


view : Model -> Html Msg
view ({ repayment_plan } as model) =
    div []
        [ RepaymentPlan.generator model
        , Html.hr [] []
        , RepaymentPlan.form repayment_plan
        , Html.hr [] []
        , RepaymentPlan.summary repayment_plan
        , Html.hr [] []
        , RepaymentPlan.json repayment_plan
        ]
