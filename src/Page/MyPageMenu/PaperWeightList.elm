module Page.MyPageMenu.PaperWeightList exposing (..)
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
    , showMenu : Bool
    }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
    { session = session 
    , check = mobile 
    , showMenu = False}
    , Cmd.none
    )

type Msg 
    = NoOp
    | ClickRight
    | ClickLeft
    | GoAnotherPage
    | ShowMenu

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowMenu ->
            ({model | showMenu = not model.showMenu}, Cmd.none)
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (Encode.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none


view : Model -> { title : String , content : Html Msg}
view model =

    if model.check then
    { title = "YourFitExer"
    , content = div [] [apprecentBuylist model ]
    }

    else
    { title = "YourFitExer"
    , content = div [] [recentBuylist model ]
    }




recentBuylist model = 
    div []
        [   div [class "mypageHiddenMenu", onClick ShowMenu] []
            , div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage model.showMenu]
            , div [ class "container" ]
            [ 
                    -- div [ class "titlewrap" ]
                    -- [ div [ class "columns" ]
                    --     [ div [ class "column is-full pagename" ]
                    --         [ div [ class "yficon" ]
                    --             [ img [ src "../image/icon_cart.png", alt "icon" ]
                    --                 []
                    --             ]
                    --         , div [ class "yftext" ]
                    --             [ strong []
                    --                 [ text "최근구매내역" ]
                    --             ]
                    --         ]
                    --     ]
                    -- ]
                commonJustHeader "/image/icon_cart.png" "최근구매내역"
                , div [ class "searchbox_wrap" ]
                    [ div [ class "cart_warning" ]
                        [ h1 [class"cart_warning_h1"]
                            [ text "구매한 상품 중 기간이 종료된 추천 운동 영상이 있습니다." ]
                        ]
                    , div [ class "control" ]
                        [ label [ class "radio" ]
                            [ input [ type_ "radio", name "answer" ]
                                [], text "유료" 
                            ]
                        , label [ class "radio" ]
                            [ input [ type_ "radio", name "answer" ]
                                [], text "무료" 
                            ]
                        ]
                    , div [ class "searchbox" ]
                        [ div [ class "cart_mediabox" ]
                            [ table [ class "cart_table" ]
                                [ tbody [ class "cart_tbody" ]
                                    [ tr [ class "cart_tr" ]
                                        [ th []
                                            [ text "운동제목" ]
                                        , th []
                                            [ text "결제일" ]
                                        , th []
                                            [ text "유효기간" ]
                                        , th []
                                            [ text "결제금액" ]
                                        , th []
                                            [ text "결제상태" ]
                                        , th []
                                            [ text "자세히보기" ]
                                        ]
                                    , tr []
                                        [ td []
                                            [ text "유어핏 초보자 운동" ]
                                        , td []
                                            [ text "19/01/01"  , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td []
                                            [ text "~19/01/31" , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td []
                                            [ text "₩1,000원" ]
                                        , td []
                                            [ text "결제완료" ]
                                        , td []
                                            [ a [ class "button is-primary" ]
                                                [ text "자세히보기" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        


apprecentBuylist model = 
    div []
         [ div [ class "topbox" ]
        [ div [ class "backbtn" ]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "topboxtitle" ]
            [ text "최근구매내역 " ]
        , div [ class "nextbtn" ]
            []
        ]
                , div [ class "searchbox_wrap" ]
                    [ div [ class "cart_warning" ]
                        [ h1 [class"cart_warning_h1"]
                            [ text "구매한 상품 중 기간이 종료된 추천 운동 영상이 있습니다." ]
                        ]
                    , div [ class "control" ]
                        [ label [ class "radio" ]
                            [ input [ type_ "radio", name "answer" ]
                                [], text "유료" 
                            ]
                        , label [ class "radio" ]
                            [ input [ type_ "radio", name "answer" ]
                                [], text "무료" 
                            ]
                        ]
                    , div [ class "searchbox" ]
                        [ div [ class "cart_mediabox" ]
                            [ table [ class "cart_table" ]
                                [ tbody [ class "cart_tbody" ]
                                    [ tr [ class "cart_tr" ]
                                        [ th []
                                            [ text "운동제목" ]
                                        , th []
                                            [ text "결제일" ]
                                        , th []
                                            [ text "유효기간" ]
                                        , th []
                                            [ text "결제금액" ]
                                        , th []
                                            [ text "결제상태" ]
                                        , th []
                                            [ text "자세히보기" ]
                                        ]
                                    , tr []
                                        [ td []
                                            [ text "유어핏 초보자 운동" ]
                                        , td []
                                            [ text "19/01/01"  , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td []
                                            [ text "~19/01/31" , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td []
                                            [ text "₩1,000원" ]
                                        , td []
                                            [ text "결제완료" ]
                                        , td []
                                            [ a [ class "button is-primary" ]
                                                [ text "자세히보기" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            
        
        