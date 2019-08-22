module Page.MyPageMenu.PaperWeightList exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Api as Api
import Json.Encode as Encode
import Route as Route

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
                            [ table [ class "purchasehistory_table" ]
                                [ tbody [ class "history_tbody" ]
                                    [ tr [ class "history_tr" ]
                                        [ th []
                                            [ text "결제목록" ]
                                        , th []
                                            [ text "결제일" ]
                                        , th []
                                            [ text "유효기간" ]
                                        , th []
                                            [ text "결제금액" ]
                                        , th []
                                            [ text "결제상태" ]
                                        -- , th []
                                        --     [ text "자세히보기" ]
                                        ]
                                    , tr [class "history_tr"]
                                        [ td [class"history_td"]
                                            [ text "테스트 상품명" ]
                                        , td [class"history_td"]
                                            [ text "19/01/01"  , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td [class"history_td"]
                                            [ text "~19/01/31" , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td [class"history_td"]
                                            [ text "₩1,000원" ]
                                        , td [class"history_td"]
                                            [ text "결제완료" ]
                                        -- , td [class"history_td"]
                                        --     [ a [ class "button is-primary" ]
                                        --         [ text "자세히보기" ]
                                        
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            -- ]
        


apprecentBuylist model = 
    div []
         [
        appHeaderRDetail "최근구매내역" "myPageHeader whiteColor" Route.MyPage "fas fa-angle-left"
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
                            [ table [ class "purchasehistory_table" ]
                                [ tbody [ class "history_tbody" ]
                                    [ tr [ class "history_tr" ]
                                        [ th []
                                            [ text "결제목록" ]
                                        , th []
                                            [ text "결제일" ]
                                        , th []
                                            [ text "유효기간" ]
                                        , th []
                                            [ text "결제금액" ]
                                        , th []
                                            [ text "결제상태" ]
                                        -- , th []
                                        --     [ text "자세히보기" ]
                                        ]
                                    , tr [class "history_tr"]
                                        [ td [class"history_td"]
                                            [ text "테스트 상품명" ]
                                        , td [class"history_td"]
                                            [ text "19/01/01"  , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td [class"history_td"]
                                            [ text "~19/01/31" , br []
                                                [], text "00:00:00" 
                                            ]
                                        , td [class"history_td"]
                                            [ text "₩1,000원" ]
                                        , td [class"history_td"]
                                            [ text "결제완료" ]
                                        -- , td [class"history_td"]
                                        --     [ a [ class "button is-primary" ]
                                        --         [ text "자세히보기" ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                -- ]
            
        
        