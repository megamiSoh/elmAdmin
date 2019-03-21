module Page.MyPageMenu.Info exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Route exposing (..)
import Page.Common exposing (..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder

type alias Model =
    { session : Session
    , checkDevice : String
    , check : Bool
    
    }



-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        ,  check = mobile}
        ,Cmd.batch[ P.checkMobile ()
        ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    P.check CheckDevice

type Msg 
    = CheckDevice E.Value
    | BackBtn

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model | checkDevice = string}, Cmd.none)
                
                    Err _ ->
                        ({model | checkDevice = ""} ,Cmd.none)
        
        BackBtn ->
            (model , Route.pushUrl (Session.navKey model.session) Route.MyPage)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
       if model.checkDevice == "pc" then
       web
       else
       app
    }

web = 
    div [ class "container" ]
        [
            commonJustHeader "/image/icon_notice.png" "공지사항",
            contentsBody,
            pagenation
        ]
app = 
    div [class "container"] [
        appHeaderback "공지사항" "myPageHeader" BackBtn,
        div [ class "table yf_table" ]
        [ tbody []
            (List.map appContentsBody infoData)
    ]
    ]

appContentsBody item =
    a [class "tableRow", Route.href Route.InfoD] [
        td[class "m_infor_tableCell"][text item.title],
        td[class"notice_date m_infor_notice_date_tableCell"][text item.createDate]
    ]

contentsBody =
    div [ class "info_mediabox" ]
        [ div [ class "table info_yf_table" ]
            [  div [class "tableRow infor_tableRow"]
                    [ div [class "tableCell info_num"]
                        [ text "번호" ]
                    , div [class "tableCell info_title"]
                        [ text "내용" ]
                    , div [class "tableCell info_date"]
                        [ text "등록일" ]
                ]
            , tbody []
                (List.indexedMap (contentsBodyLayout) infoData)
            ]
        ]
contentsBodyLayout idx item =
        a [ class "tableRow", Route.href Route.InfoD ]
            [
                div [class "tableCell info_num_text"] [text (String.fromInt ( idx + 1 ))],
                div [class "tableCell info_title_text"] [text item.title],
                div [class "tableCell info_date_text"] [text item.createDate]
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

infoData = 
    [
        {
           title ="유어핏 개인정보 처리방침 개정안내",
           createDate = "19-01-01" 
        },
        {
           title ="[유어핏] 유어핏 개인정보 및 서비스 이전 안내",
           createDate = "19-01-01" 
        },
        {
           title ="유어핏 사칭 코인 세일 피싱 사이트 피해 주의 안내" ,
           createDate = "19-01-01" 
        },
        {
           title = "유어핏 위치기반서비스 이용약관 변경 안내",
           createDate = "19-01-01" 
        },
        {
           title ="유어핏 서비스 운영정책 변경 안내",
           createDate = "19-01-01" 
        },
        {
           title ="유어핏 쇼핑 개인정보 이전 안내",
           createDate = "19-01-01" 
        },
        {
           title ="유어핏 통합 약관 및 서비스 약관 변경 안내",
           createDate = "19-01-01" 
        },
        {
           title ="문의하기 창구를 잠시 닫습니다.",
           createDate = "19-01-01" 
        },
        {
           title ="유어핏 개인정보처리 개정안내" ,
           createDate = "19-01-01" 
        },
        {
           title = "유어핏어플 그랜드 오픈!",
           createDate = "19-01-01" 
        }
    ]