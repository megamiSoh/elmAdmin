module Page.Detail.FaqDetail exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Route exposing (..)
import Page.Common exposing (..)
import Api as Api

type alias Model 
    = {
        session : Session
        ,checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = ""
        , check = mobile}
        , Cmd.none
    )

type Msg 
    = CheckDevice E.Value

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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
                        ({model | checkDevice = string},Cmd.none)
                
                    Err _ ->
                        ({model | checkDevice = ""},Cmd.none)
                        
            

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = web}

web =
    div [class "container"] [
        commonJustHeader "/image/icon_qna.png" "1:1문의",
        ques qnaData,
        answer qnaData,
        backBtn
    ]

ques item= 
        div [ class "info_mediabox contentsH" ]
            [ div [ class "infoDetail_titlebox" ]
                [ div [ class "infoDetail_title" ]
                    [ text item.title ]
                , div [ class "infoDetail_yf_date" ]
                    [ text item.createDate ]
                ]
            , div [ class "infoDetail_textbox" ]
                [ text item.article ]
            , p [class"faqDetail_btnbox"]
                [ a [ class "button faqDetail_edit" ]
                    [ i [ class "fas fa-edit" ]
                        [], text "수정" 
                    ]
                , a [ class "button" ]
                    [ i [ class "far fa-trash-alt" ]
                        [], text "삭제" 
                    ]
                ]
            ]

answer item= 
    div [ class "info_mediabox contentsH" ]
        [ div [ class "infoDetail_titlebox" ]
            [ div [ class "infoDetail_title" ]
                [ text item.answerTitle ]
            , div [ class "infoDetail_yf_date" ]
                [ text item.answerDate ]
            ]
        , div [ class "infoDetail_textbox" ]
            [text item.answerArticle]
            ]
backBtn =
    div [ class "make_yf_butbox" ]
    [ a [ class "button infoDetail_yf_back", Route.href Route.Faq ]
        [ text "뒤로" ]
    ]       
qnaData = 
    {title = "서비스 신청 주기는 어떻게 되나요?",
    article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
    createDate ="2019-01-01",
    answerDate ="2019-01-01",
    answerTitle = "답변드립니다.",
    answerArticle ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."}