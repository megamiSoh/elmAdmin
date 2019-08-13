module Page.YourFitPrice exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Api as Api
import Json.Encode as Encode

type alias Model =
    { session : Session
    , check : Bool
    }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
    { session = session 
    , check = mobile }
    , Cmd.none
    )

type Msg 
    = NoOp

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none


view : Model -> { title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content = div [] [webLayout]
    }


webLayout = 
    div [ class "containerwrap" ]
        [ div [ class "container" ]
            [ div [ class "notification yf_workout" ]
                [ div [ class "titlewrap" ]
                    [ div [ class "columns" ]
                        [ div [ class "column is-full pagename" ]
                            [ div [ class "yficon" ]
                                [ img [  src "../image/icon_cart.png", alt "icon" ]
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
                    [ div [ class "searchbox" ]
                        [ div [ class "mediabox" ]
                            [ div [ class "background" ]
                                [ div [ class "container" ]
                                    [ div [ class "panel pricing-table" ]
                                        [ div [ class "pricing-plan" ]
                                            [ h2 [ class "pricing-header" ]
                                                [ text "무료 체험" ]
                                            , ul [ class "pricing-features" ]
                                                [ li [ class "pricing-features-item" ]
                                                    [ text "유어핏 맞춤 운동 서비스" ]
                                                , li [ class "pricing-features-item" ]
                                                    [ text "맞춤형 문진 운동 서비스 7일" ]
                                                ]
                                            , span [ class "pricing-price" ]
                                                [ text "Free" ]
                                            , a [ href "#/", class "pricing-button" ]
                                                [ text "무료" ]
                                            ]
                                        , div [ class "pricing-plan" ]
                                            [ h2 [ class "pricing-header" ]
                                                [ text "프리미엄 서비스" ]
                                            , ul [ class "pricing-features" ]
                                                [ li [ class "pricing-features-item" ]
                                                    [ text "유어핏 맞춤 운동 서비스" ]
                                                , li [ class "pricing-features-item" ]
                                                    [ text "맞춤형 문진 운동 서비스 7일" ]
                                                ]
                                            , span [ class "pricing-price" ]
                                                [ text "₩ 3,300원" ]
                                            , a [ href "#/", class "pricing-button is-featured" ]
                                                [ text "결제" ]
                                            ]
                                        , div [ class "pricing-plan" ]
                                            [ h2 [ class "pricing-header" ]
                                                [ text "VIP 서비스" ]
                                            , ul [ class "pricing-features" ]
                                                [ li [ class "pricing-features-item" ]
                                                    [ text "유어핏 맞춤 운동 서비스" ]
                                                , li [ class "pricing-features-item" ]
                                                    [ text "맞춤형 문진 운동 서비스 30일" ]
                                                ]
                                            , span [ class "pricing-price" ]
                                                [ text "₩ 9,900원" ]
                                            , a [ href "#/", class "pricing-button is-featured" ]
                                                [ text "결제" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]