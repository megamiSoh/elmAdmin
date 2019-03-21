module Page.MyPageMenu.MyScrap exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port exposing(..)
import Api as Api

type alias Model 
    = {
        session : Session
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , check = mobile}
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

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
     div [ class "container" ]
        [
            commonJustHeader "/image/icon_list.png" "나의 스크랩",
            div [ class "yf_yfworkout_search_wrap" ]
            [
                contentsCount,
                div [class "myScrap_mediabox"] [
                    scrapItem,
                    pagenation
                ]
            ]
        ]
    }

contentsCount =
    div [ class "tapbox" ]
        [ div [ class "myScrap_yf_large" ]
            [ text "총 n건의 결과" ]
        ]
 

scrapItem = 
    div [ class "myScrap_yf_box" ]
        [ img [ src "/image/dummy_video_image.png" ]
            []
        , i [ class "fas fa-bookmark myScrap_yf_icon_mark" ]
            []
        , div [ class "myScrap_box_wrap" ]
            [ div [ class "myScrap_yf_box_title" ]
                [ text "유어핏 내가만든운동" ]
            , ul []
                [ li [ class "myScrap_box_name" ]
                    [ text "유어핏관리자" ]
                , li [ class "myScrap_box_date" ]
                    [ text "19/01/01" ]
                ]
            ]
        ]

pagenation=
    div [ class "yf_Pagination" ]
        [ nav [ class "pagination is-centered" ]
            [ ul [ class "pagination-list" ]
                [ li [ class "" ]
                    [ a [ class "pagination-link"]
                        [ text "<" , text "<" ]
                    ]
                , a [ class "pagination-link"]
                    [ text "<" ]
                , li []
                    [ a [ class "pagination-link is-current yf_cut" ]
                        [ text "5" ]
                    ]
                , li []
                    [ a [ class "pagination-link"]
                        [ text ">" ]
                    ]
                , a [ class "pagination-link" ]
                    [ text ">>" ]
                ]
            ]
        ]