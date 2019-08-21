module Page.YourFitPrice exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Api as Api
import Json.Encode as Encode
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Http as Http
import Route as Route
type alias Model =
    { session : Session
    , check : Bool
    , getList : List Price
    , product_id : Int
    , product_name : String
    }


type alias PriceData = 
    { data : List Price }


type alias Price = 
    { day_num : Int
    , description : String
    , id : Int
    , is_pay : Bool
    , name : String
    , price : Int }

type alias OrderData = 
    { data : Order }

type alias Order = 
    { amount : Int
    , buyer_email : String
    , buyer_name : String
    , digital : Bool
    , merchant_uid : String
    , name : String }

orderApi product_id session = 
    let
        body = 
            Encode.object 
                [("product_id", Encode.int product_id )]
                |> Http.jsonBody
    in
    Api.post Endpoint.orderGo (Session.cred session) OrderComplete body (Decoder.orderData OrderData Order)

priceApi session = 
    Api.get GetList Endpoint.priceData (Session.cred session) (Decoder.priceData PriceData Price)
-- Decode.priceData
init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
    { session = session 
    , check = mobile 
    , getList = []
    , product_id = 0
    , product_name = ""
    }
    , priceApi session
    )

type Msg 
    = NoOp
    | GetList (Result Http.Error PriceData)
    | BackPage
    | OrderComplete (Result Http.Error OrderData)
    | PayStart Int String
    | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session},
            Cmd.none)
        PayStart id name ->
            case model.session of
                LoggedIn _ val ->
                    ({model | product_id = id, product_name = name}
                    , orderApi id model.session
                    )
                Guest _ ->
                    ({model | product_id = id, product_name = name}, Route.pushUrl (Session.navKey model.session) Route.Login)
        OrderComplete (Ok ok) ->
            let _ = Debug.log "ok" ok.data
                orderData = 
                    Encode.object 
                        [ ("amount", Encode.int ok.data.amount)
                        , ("buyer_email", Encode.string ok.data.buyer_email)
                        , ("buyer_name", Encode.string ok.data.buyer_name)
                        , ("digital", Encode.bool ok.data.digital)
                        , ("merchant_uid", Encode.string ok.data.merchant_uid)
                        , ("name", Encode.string ok.data.name)]
            in
            (model, Api.payment orderData)
        OrderComplete (Err err) ->
            let _ = Debug.log "a" err
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model,  (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        BackPage ->
            (model, Route.backUrl (Session.navKey model.session) 1)
        NoOp ->
            ( model, Cmd.none )
        GetList (Ok ok) ->
            ({model | getList = ok.data}, Cmd.none)
        GetList (Err err) ->
            let _ = Debug.log "err" err
            in
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = 
    Session.changes GotSession (Session.navKey model.session)


view : Model -> { title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content = 
        if model.check then
            div []
                [ appHeaderRDetailClick "유어핏 가격" "myPageHeader whiteColor" BackPage "fas fa-angle-left"
                , applayout model
                ]
        else
            div [] [weblayout model ]
    }



priceLayout item = 
    div [ class ("plan " ++ if item.is_pay then (
        if item.day_num >= 30 then "ultimite" else "standard"
    ) else "basic")]
    [ div [ class "plan-inner" ]
        [ 
            if item.is_pay then
            div [ class "entry-title" ]
            [ h3 []
                [ text item.name ]
            , h1 [ class "yf_price" ]
                [ text ("₩ "++ String.fromInt item.price++" 원") ]
            , h1 [ class "yf_price_date" ]
                [ text ("서비스기간 "++ String.fromInt item.day_num ++"일") ]
            ]
            else
            div [ class "entry-title" ]
                [ h3 []
                    [ text "무료 체험" ]
                , h1 [ class "yf_price" ]
                    [ text "무료" ]
                , h1 [ class "yf_price_date" ]
                    [ text ("체험기간 "++ String.fromInt item.day_num ++"일") ]
                ]
        , div [ class "entry-content" ]
            [ text item.description]
        , div [ class "btnprice", onClick (PayStart item.id item.name) ]
            [ div [ class "button is-primary" ]
                [ text (if item.is_pay then "결제 하기" else "체험 하기") ]
            ]
        ]
    ]

weblayout model = 
    div [ class "yf_pricewrap" ]
    [ div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column is-full pagename" ]
                [ div [ class "yficon" ]
                      [ i [ class "fas fa-won-sign" ]
                        []
                    ]
                , div [ class "yftext" ]
                    [ strong []
                        [ text "유어핏 가격표" ]
                    ]
                ]
            ]
        ]

   , div [ class "searchbox_wrap" ]
        [ div [ class "pirce_searchbox" ]
            [ div [ class "mediabox" ]
                (List.map priceLayout model.getList)
                -- [ 
                --     basicPrice
                -- , mediumPrice
                -- , highPrice
                -- ]
            ]
        ]

    ]


applayout model = 
    div [ class "yf_pricewrap" ]
        [div [ class "searchbox_wrap" ]
        [ div [ class "pirce_searchbox" ]
            [ div [ class "mediabox" ]
                (List.map priceLayout model.getList)
                -- [ 
                --     basicPrice
                -- , mediumPrice
                -- , highPrice
                -- ]
            ]
        ]

        ]