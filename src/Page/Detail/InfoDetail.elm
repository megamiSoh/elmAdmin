module Page.Detail.InfoDetail exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route exposing(..)
import Port as P
import Json.Decode as Decode
import Json.Encode as E
import Api as Api

type alias Model 
    = {
        session : Session,
        checkDevice: String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = ""
        , check = mobile}
        , P.checkMobile ()
    )

type Msg 
    = CheckDevice E.Value

subscriptions : Model -> Sub Msg
subscriptions model =
    P.check CheckDevice

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
                        ({model | checkDevice = ""}, Cmd.none)
            

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
    if model.checkDevice =="pc" then
    web
    else
    app
    
    }

web = 
    div [class "container"] [
        commonJustHeader "/image/icon_notice.png" "공지사항" ,
        contentsBox article,
        backBox
    ]
app =
    div [class "container"] [
        appHeaderinforDetail  article.title "myPageHeader" Route.Info "fas fa-times",
        appContentsBox article
    ]
contentsBox item= 
    div [ class "info_mediabox" ]
        [ div [ class "infoDetail_titlebox" ]
            [ div [ class "infoDetail_title" ]
                [ text item.title  ]
            , div [ class "infoDetail_yf_date" ]
                [ text item.createDate ]
            ]
        , div [ class "infoDetail_textbox" ]
            [ 
                text item.article
            ]
        ]

appContentsBox item= 
    div [ class "mediabox" ]
        [ div [ class "titlebox" ]
            [  div [ class "m_infoDetail_yf_date" ]
                [ text item.createDate ]
            ]
        , div [ class "m_infoDetail_textbox" ]
            [ 
                text item.article
            ]
        ]

backBox = 
    div [ class "make_yf_butbox" ]
        [ a [ class "button infoDetail_yf_back", Route.href Route.Info]
            [ text "뒤로" ]
        ]

article =   {
    title = "유어핏어플 그랜드 오픈!",
    createDate = "2019-01-01",
    article = "안녕하세요. 유어핏입니다.금일 유어핏에서 비정상적인 종료 현상이 발생되어 관련 내용 안내드립니다.현재 불법적인 방법과 비정상적인 방법으로 어플을 실행하는 사례가 발견되고 있으며, 현재 전체적인 조사를 통해 어플 사용자의 비정상 활동에 대해 조치중입니다.부당한 방법으로 어플을 활용할시 운영정책에 의거하여 어플 사용이 제한될 수 있습니다. 이 점을 양지하시며 어플 이용에 불편함이 없으시길 바랍니다.감사합니다."
    }