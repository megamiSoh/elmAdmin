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

priceApi session = 
    Api.get GetList Endpoint.priceData (Session.cred session) (Decoder.priceData PriceData Price)
-- Decode.priceData
init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
    { session = session 
    , check = mobile 
    , getList = []}
    , priceApi session
    )

type Msg 
    = NoOp
    | GetList (Result Http.Error PriceData)
    | BackPage

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    Sub.none


view : Model -> { title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content = 
        if model.check then
            div []
                [ appHeaderRDetailClick "유어핏 가격" "myPageHeader whiteColor" BackPage "fas fa-angle-left"
                , weblayout model
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
        , div [ class "btnprice" ]
            [ a [ class "button is-dark" ]
                [ text (if item.is_pay then "결제 하기" else "체험 하기") ]
            ]
        ]
    ]

weblayout model = 
    div [ class "searchbox_wrap" ]
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