module Page.YourFitPrice exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Api as Api
import Json.Encode as Encode
import Json.Decode as Decode
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
    , ableToWatch : Bool
    , price : List String
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

type alias WatchCheckData = 
    { data : Bool }



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

promoteApi product_id session = 
    let
        body = 
            Encode.object
                [("product_id", Encode.int product_id)]
                |> Http.jsonBody
    in
    Api.post Endpoint.promote (Session.cred session) PromoteComplete body Decoder.resultD

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
    { session = session 
    , check = mobile 
    , getList = []
    , product_id = 0
    , product_name = ""
    , ableToWatch = False
    , price = []
    }
    , priceApi session
    )

type Msg 
    = NoOp
    | GetList (Result Http.Error PriceData)
    | BackPage
    | OrderComplete (Result Http.Error OrderData)
    | PayStart Int Bool
    | GotSession Session
    | PossibleToWatch (Result Http.Error WatchCheckData)
    | PriceFormat Encode.Value
    | PromoteComplete ( Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PromoteComplete (Ok ok) ->
            (model, Cmd.batch[Route.pushUrl(Session.navKey model.session) Route.MJList
            , Api.showToast (Encode.string "구매되었습니다.")])
        PromoteComplete (Err err) ->
            (model, Api.showToast (Encode.string "이미 상품 구독 중 입니다."))
        PriceFormat format ->  
            let
               formtDecode =    
                    Decode.decodeValue (Decode.list Decode.string) format
            in
                case formtDecode of
                    Ok ok ->
                        ({model | price = ok}, Cmd.none)
                    Err err ->
                        (model, Cmd.none)
        PossibleToWatch (Ok ok) ->
            ({model | ableToWatch = ok.data}, Cmd.none)
        PossibleToWatch (Err err) ->
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session},
            Cmd.none)
        PayStart id is_pay ->
            case model.session of
                LoggedIn _ val ->
                    if is_pay then
                        ({model | product_id = id}
                        , orderApi id model.session
                        )
                    else
                        ({model | product_id = id}
                        ,promoteApi id model.session
                        )
                Guest _ ->
                    ({model | product_id = id}, Route.pushUrl (Session.navKey model.session) Route.Login)
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
            (model, Api.showToast (Encode.string "이미 상품 구독 중 입니다."))
        BackPage ->
            (model, Route.backUrl (Session.navKey model.session) 1)
        NoOp ->
            ( model, Cmd.none )
        GetList (Ok ok) ->
            let
                price = 
                    List.map(\x -> x.price) ok.data
                priceEncode = 
                    Encode.object
                        [ ("price", (Encode.list (Encode.int)) price) ]
            in
            ({model | getList = ok.data}, 
            Cmd.batch [Api.get PossibleToWatch (Endpoint.possibleToCheck) (Session.cred model.session) (Decoder.possibleToWatch WatchCheckData)
            , Api.comma priceEncode])
        GetList (Err err) ->
            let _ = Debug.log "err" err
            in
            (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch[
        Session.changes GotSession (Session.navKey model.session)
        , Api.commaF PriceFormat]


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

commaFormat item = 
    span [][text item]

priceLayout item model price = 
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
                [ text (price ++ "원")
                ]
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
        , if model.ableToWatch then
            div [class "impossible_pay"][text "※ 이미 상품 구독 중 입니다."]
        else
            div [ class "btnprice", onClick (PayStart item.id  item.is_pay) ]
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
                (List.map2 (\x y -> priceLayout x model y ) model.getList model.price)
            ]
        ]

    ]


applayout model = 
    div [ class "yf_pricewrap" ]
        [div [ class "searchbox_wrap" ]
        [ div [ class "pirce_searchbox" ]
            [ div [ class "mediabox" ]
                 (List.map2 (\x y -> priceLayout x model y) model.getList model.price)
            ]
        ]

        ]