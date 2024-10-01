module Main exposing (..)

import Browser
import Html exposing (Html, div, text, ul, li, h1, button, input, select, option)
import Html.Attributes exposing (placeholder, value, class)
import Html.Events exposing (onClick, onInput)
import List exposing (sum)
import Time exposing (Posix, millisToPosix, posixToMillis, now)
import Task exposing (Task)

-- MODEL

type alias Payment =
    { id : Int, amount : Float, description : String, date : String }

type alias Model =
    { payments : List Payment
    , expenses : List Payment
    , currentAmount : String
    , currentExpenseAmount : String
    , currentExpenseDescription : String
    , currentEditDate : String
    , showExpenseInputs : Bool
    , showPaymentInputs : Bool
    , editMode : Maybe (Int, String)
    , viewMode : ViewMode
    , viewPage : ViewPage -- Track which page to show
    }

type ViewMode
    = ShowBoth
    | ShowPayments
    | ShowExpenses

type ViewPage
    = MainPage
    | StatsPage

-- INITIAL DATA

initialPayments : List Payment
initialPayments =
    []

initialExpenses : List Payment
initialExpenses =
    []

initialModel : Model
initialModel =
    { payments = initialPayments
    , expenses = initialExpenses
    , currentAmount = ""
    , currentExpenseAmount = ""
    , currentExpenseDescription = ""
    , currentEditDate = ""
    , showExpenseInputs = False
    , showPaymentInputs = False
    , editMode = Nothing
    , viewMode = ShowBoth
    , viewPage = MainPage
    }

-- VIEW

view : Model -> Html Msg
view model =
    case model.viewPage of
        MainPage ->
            div [ class "max-w-lg mx-auto p-4" ]
                [ h1 [ class "text-2xl font-bold mb-4" ] [ text ("Payments - Total: $" ++ String.fromFloat (total model)) ]
                , div [ class "text-xl mb-4" ] [ text ("Total Payments This Month: $" ++ String.fromFloat (totalPaymentsThisMonth model)) ]
                , filterButtons model
                , button [ onClick TogglePaymentInputs, class "bg-blue-500 text-white px-4 py-2 rounded mt-4" ] [ text "Add Payment" ]
                , viewPaymentInputs model
                , button [ onClick ToggleExpenseInputs, class "bg-red-500 text-white px-4 py-2 rounded mt-4" ] [ text "Add Expense" ]
                , viewExpenseInputs model
                , button [ onClick (NavigateTo StatsPage), class "bg-green-500 text-white px-4 py-2 rounded mt-4" ] [ text "View Stats" ]
                , ul [ class "mt-4" ] (viewFiltered model)
                ]

        StatsPage ->
            viewStats model

filterButtons : Model -> Html Msg
filterButtons model =
    div [ class "mb-4" ]
        [ select [ onInput SetViewMode, class "border border-gray-300 p-2 rounded" ]
            [ option [ value "both" ] [ text "Show Both" ]
            , option [ value "payments" ] [ text "Show Payments" ]
            , option [ value "expenses" ] [ text "Show Expenses" ]
            ]
        ]

viewPaymentInputs : Model -> Html Msg
viewPaymentInputs model =
    if model.showPaymentInputs then
        div [ class "my-4" ]
            [ input
                [ placeholder "Payment Amount"
                , value model.currentAmount
                , onInput UpdateAmount
                , class "border border-gray-300 p-2 rounded w-full"
                ] []
            , button [ onClick AddPayment, class "bg-blue-500 text-white px-4 py-2 rounded mt-2" ] [ text "Submit Payment" ]
            ]
    else
        text ""

viewExpenseInputs : Model -> Html Msg
viewExpenseInputs model =
    if model.showExpenseInputs then
        div [ class "my-4" ]
            [ input
                [ placeholder "Expense Description"
                , value model.currentExpenseDescription
                , onInput UpdateExpenseDescription
                , class "border border-gray-300 p-2 rounded w-full mb-2"
                ] []
            , input
                [ placeholder "Expense Amount"
                , value model.currentExpenseAmount
                , onInput UpdateExpenseAmount
                , class "border border-gray-300 p-2 rounded w-full mb-2"
                ] []
            , button [ onClick AddExpense, class "bg-red-500 text-white px-4 py-2 rounded mt-2" ] [ text "Submit Expense" ]
            ]
    else
        text ""

viewFiltered : Model -> List (Html Msg)
viewFiltered model =
    case model.viewMode of
        ShowPayments ->
            List.map (viewPayment model) model.payments

        ShowExpenses ->
            List.map (viewExpense model) model.expenses

        ShowBoth ->
            List.map (viewPayment model) model.payments ++ List.map (viewExpense model) model.expenses

viewPayment : Model -> Payment -> Html Msg
viewPayment model payment =
    let
        isEditing = case model.editMode of
            Just (id, "payment") -> id == payment.id
            _ -> False
    in
    li [ class "border-b py-2" ]
        [ if isEditing then
            div []
                [ input
                    [ placeholder "Edit Payment Description"
                    , value payment.description
                    , onInput (UpdateEditDescription payment.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , input
                    [ placeholder "Edit Payment Amount"
                    , value (String.fromFloat payment.amount)
                    , onInput (UpdateEditAmount payment.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , input
                    [ placeholder "Edit Payment Date"
                    , value model.currentEditDate
                    , onInput (UpdateEditDate payment.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , button [ onClick (SaveEdit payment.id), class "bg-green-500 text-white px-4 py-2 rounded mt-2" ] [ text "Save" ]
                , button [ onClick (CancelEdit payment.id), class "bg-gray-500 text-white px-4 py-2 rounded mt-2 ml-2" ] [ text "Cancel" ]
                , button [ onClick (DeleteEntry (payment.id, "payment")), class "bg-red-500 text-white px-4 py-2 rounded mt-2 ml-2" ] [ text "Delete" ]
                ]
          else
            div [ class "flex justify-between" ]
                [ text (payment.description ++ ": $" ++ String.fromFloat payment.amount ++ " on " ++ payment.date)
                , button [ onClick (EditEntry (payment.id, "payment")), class "bg-blue-500 text-white px-4 py-2 rounded" ] [ text "Edit" ]
                ]
        ]

viewExpense : Model -> Payment -> Html Msg
viewExpense model expense =
    let
        isEditing = case model.editMode of
            Just (id, "expense") -> id == expense.id
            _ -> False
    in
    li [ class "border-b py-2" ]
        [ if isEditing then
            div []
                [ input
                    [ placeholder "Edit Expense Description"
                    , value expense.description
                    , onInput (UpdateEditDescription expense.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , input
                    [ placeholder "Edit Expense Amount"
                    , value (String.fromFloat expense.amount)
                    , onInput (UpdateEditAmount expense.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , input
                    [ placeholder "Edit Expense Date"
                    , value model.currentEditDate
                    , onInput (UpdateEditDate expense.id)
                    , class "border border-gray-300 p-2 rounded w-full mb-2"
                    ] []
                , button [ onClick (SaveEdit expense.id), class "bg-green-500 text-white px-4 py-2 rounded mt-2" ] [ text "Save" ]
                , button [ onClick (CancelEdit expense.id), class "bg-gray-500 text-white px-4 py-2 rounded mt-2 ml-2" ] [ text "Cancel" ]
                , button [ onClick (DeleteEntry (expense.id, "expense")), class "bg-red-500 text-white px-4 py-2 rounded mt-2 ml-2" ] [ text "Delete" ]
                ]
          else
            div [ class "flex justify-between" ]
                [ text (expense.description ++ ": -$" ++ String.fromFloat expense.amount ++ " on " ++ expense.date)
                , button [ onClick (EditEntry (expense.id, "expense")), class "bg-blue-500 text-white px-4 py-2 rounded" ] [ text "Edit" ]
                ]
        ]

-- VIEW STATS

viewStats : Model -> Html Msg
viewStats model =
    let
        target = 3500
        totalPayments = sum (List.map .amount model.payments)
        averageWeeklyPayment = totalPayments / 4
    in
    div [ class "max-w-lg mx-auto p-4" ]
        [ h1 [ class "text-2xl font-bold mb-4" ] [ text "Stats" ]
        , div [ class "text-xl mb-4" ] [ text ("Total Payments: $" ++ String.fromFloat totalPayments) ]
        , div [ class "text-xl mb-4" ] [ text ("Average Weekly Payment: $" ++ String.fromFloat averageWeeklyPayment) ]
        , div [ class "text-xl mb-4" ] [ text ("Target: $" ++ String.fromFloat target) ]
        , button [ onClick (NavigateTo MainPage), class "bg-blue-500 text-white px-4 py-2 rounded mt-4" ] [ text "Back to Main" ]
        ]


-- UPDATE

type Msg
    = AddPayment
    | UpdateAmount String
    | AddExpense
    | UpdateExpenseAmount String
    | UpdateExpenseDescription String
    | ToggleExpenseInputs
    | TogglePaymentInputs
    | SetViewMode String
    | EditEntry (Int, String)
    | UpdateEditAmount Int String
    | UpdateEditDescription Int String
    | UpdateEditDate Int String
    | SaveEdit Int
    | CancelEdit Int
    | DeleteEntry (Int, String)
    | NavigateTo ViewPage -- New message for navigation

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPayment ->
            let
                amount = String.toFloat model.currentAmount
            in
            case amount of
                Just amt ->
                    let
                        currentDate = "2024-09-29"  -- Placeholder for date, replace with proper date handling
                        newPayment =
                            { id = List.length model.payments + 1
                            , amount = amt
                            , description = "Car Payment"
                            , date = currentDate
                            }
                    in
                    ( { model
                        | payments = model.payments ++ [ newPayment ]
                        , currentAmount = ""
                        , showPaymentInputs = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    (model, Cmd.none)

        UpdateAmount value ->
            ( { model | currentAmount = value }, Cmd.none )

        AddExpense ->
            let
                amount = String.toFloat model.currentExpenseAmount
            in
            case amount of
                Just amt ->
                    let
                        currentDate = "2024-09-29"  -- Placeholder for date, replace with proper date handling
                        newExpense =
                            { id = List.length model.expenses + 1
                            , amount = amt
                            , description = model.currentExpenseDescription
                            , date = currentDate
                            }
                    in
                    ( { model
                        | expenses = model.expenses ++ [ newExpense ]
                        , currentExpenseAmount = ""
                        , currentExpenseDescription = ""
                        , showExpenseInputs = False
                      }
                    , Cmd.none
                    )

                Nothing ->
                    (model, Cmd.none)

        UpdateExpenseAmount value ->
            ( { model | currentExpenseAmount = value }, Cmd.none )

        UpdateExpenseDescription value ->
            ( { model | currentExpenseDescription = value }, Cmd.none )

        ToggleExpenseInputs ->
            ( { model | showExpenseInputs = not model.showExpenseInputs }, Cmd.none )

        TogglePaymentInputs ->
            ( { model | showPaymentInputs = not model.showPaymentInputs }, Cmd.none )

        SetViewMode value ->
            case value of
                "both" ->
                    ( { model | viewMode = ShowBoth }, Cmd.none )

                "payments" ->
                    ( { model | viewMode = ShowPayments }, Cmd.none )

                "expenses" ->
                    ( { model | viewMode = ShowExpenses }, Cmd.none )

                _ ->
                    (model, Cmd.none)

        EditEntry (id, entryType) ->
            ( { model | editMode = Just (id, entryType), currentEditDate = "" }, Cmd.none )

        UpdateEditAmount id value ->
            let
                amount = String.toFloat value
            in
            case amount of
                Just amt ->
                    ( { model | payments = List.map (updateAmount id amt) model.payments
                              , expenses = List.map (updateAmount id amt) model.expenses
                              }
                    , Cmd.none )

                Nothing ->
                    (model, Cmd.none)

        UpdateEditDescription id value ->
            ( { model | payments = List.map (updateDescription id value) model.payments
                      , expenses = List.map (updateDescription id value) model.expenses
                      }
            , Cmd.none )

        UpdateEditDate id value ->
            ( { model | currentEditDate = value }, Cmd.none )

        SaveEdit id ->
            case model.editMode of
                Just (entryId, entryType) ->
                    ( { model | editMode = Nothing }
                    , Cmd.none )

                Nothing ->
                    (model, Cmd.none)

        CancelEdit id ->
            ( { model | editMode = Nothing }, Cmd.none )

        DeleteEntry (id, entryType) ->
            case entryType of
                "payment" ->
                    ( { model | payments = List.filter (\p -> p.id /= id) model.payments }, Cmd.none )

                "expense" ->
                    ( { model | expenses = List.filter (\e -> e.id /= id) model.expenses }, Cmd.none )

                _ ->
                    (model, Cmd.none)

        NavigateTo page ->
            ( { model | viewPage = page }, Cmd.none)

-- FUNCTIONALITY

updateAmount : Int -> Float -> Payment -> Payment
updateAmount id newAmount payment =
    if payment.id == id then
        { payment | amount = newAmount }
    else
        payment

updateDescription : Int -> String -> Payment -> Payment
updateDescription id newDescription payment =
    if payment.id == id then
        { payment | description = newDescription }
    else
        payment

-- TOTAL CALCULATION

total : Model -> Float
total model =
    List.foldl (\payment acc -> acc + payment.amount) 0 model.payments
        - List.foldl (\expense acc -> acc + expense.amount) 0 model.expenses

totalPaymentsThisMonth : Model -> Float
totalPaymentsThisMonth model =
    let
        currentMonth = "2024-09"  -- Replace with proper date handling to get the current month
    in
    List.foldl 
        (\payment acc -> 
            if String.startsWith currentMonth payment.date then 
                acc + payment.amount 
            else 
                acc
        ) 
        0 
        model.payments

-- MAIN

main =
    Browser.element
        { init = init, update = update, view = view, subscriptions = subscriptions }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none






