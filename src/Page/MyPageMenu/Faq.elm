module Page.MyPageMenu.Faq exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Route exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Api as Api
type alias Model 
    = {
        session : Session
        , checkDevice : String
        , idx : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = "",
        idx = ""
        , check = mobile}
        , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type Msg 
    = CheckDevice E.Value
    | Show Int

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
                        
        Show idx ->
            if String.fromInt(idx) == model.idx then
            ({model| idx = ""}, Cmd.none)    
            else
            ({model| idx = String.fromInt(idx)}, Cmd.none)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        div [] [
            if model.checkDevice == "pc" then
            web
            else
            app model
        ]
    }
web = 
    div [ class "container" ]
        [
            commonJustHeader "/image/icon_qna.png" "1:1문의",
            contentsBody,
            pagenation,
            faqWrite
        ]
app model = 
    div [class "container"] [
        appHeaderBothfnq "1:1문의" "myPageHeader" Route.MyPage "fas fa-angle-left" "글쓰기" Route.FaqW,
        appContentsBody model,
        floating

    ]
appContentsBody model=
    div [class "settingbox"] [
        div [] (List.indexedMap (\idx x ->  itemLayout idx x model) faqData)
    ]
itemLayout idx item model =
     div [][
        div [class "eventArea" ,onClick (Show idx)] [
            div [class "titleLeft"] [text item.title],
            div [class "qna_date inlineB"] [
                ul [] [
                    li [] [text item.createDate],
                    li [] [
                        if item.isAnswer then
                        span [class "faq_done"] [text "답변완료"]
                        else
                        span [class "faq_waiting"] [text "대기중"]
                    ]
                ]
            ]
        ]
       , expandQ idx model item.article item.answerDate item.answer item.isAnswer
       ]
expandAnswer date  = 
        div [class"answerbox"] [
        p [ class "yf_answerbox" ]
            [ p[class"answerbox_text"][text "답변드립니다."]]
        ,p [ class ("inlineB " ++ date) ]
            [ text "19-01-01" ]
        ]
showAnswer answer= 
    tr [class"tr_showAnswer"]
        [ td [ colspan 2 ]
            [  text answer ]
        ]



expandQ idx model userText aDate ans isAnswer =
    div [classList [
        ("heightZero", True),
        ("heightShow", String.fromInt(idx)  == model.idx )
    ]] [
        div [][
            p[class"faq_q"] [text userText, text (String.fromInt (idx))],
            p [class"m_fnq_btn"  ]
                [ a [ class "button faq_q_btn "]
                    [ i [ class "fas fa-edit " ]
                        [], text "수정" 
                    ]
                , a [ class "button faq_q_btn" ]
                    [ i [ class "far fa-trash-alt " ]
                        [], text "삭제" 
                    ]
                ]
            ],
            if isAnswer then
                div [ class "tableSet"] [
                expandAnswer aDate,
                showAnswer ans
                ]
            else 
            span [] []
                
            
    ]


contentsBody =
    div [ class "info_mediabox" ]
        [ div [ class "table info_yf_table" ]
            [ div [class "tableRow"]
                    [ div [class "tableCell faq_num"]
                        [ text "번호" ]
                    , div [class "tableCell faq_title"]
                        [ text "내용" ]
                    , div [class "tableCell faq_ing"]
                        [ text "진행사항" ]
                    , div [class "tableCell faq_date"]
                        [ text "등록일" ]
                    ]
            , tbody []
                (List.indexedMap contentsLayout faqData)
            ]
        ]

contentsLayout idx item= 
    a [class "tableRow", Route.href Route.FaqD] [
        div [class "tableCell qna_numtext"] [text (String.fromInt(idx + 1))],
        div [class "tableCell qna_title_text"] [text item.title],
        div [class "tableCell qna_ing_text"] [
            if item.isAnswer then
            span [class "faq_done"] [text "답변 완료"]
            else
            span [class "faq_waiting"][text "대기중"]
        ],
        div [class "tableCell qna_title_date"] [text item.createDate]
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
faqWrite = 
    div [ class " yf_dark" ]
        [ a [ class "button is-dark", Route.href Route.FaqW]
            [ text "글쓰기" ]
        ]
floating = 
    a [ Route.href Route.FaqW, class "float" ]
        [ i [ class "fas fa-pen icon_pen" ]
            []
        ]
faqData = 
    [
        {
            title = "기기 등록 서비스는 각각 가입해야 하나요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "기기 등록 서비스 가입은 어떻게 하나요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "PC에서 사용 가능한 기기 등록 서비스는 무엇인가요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "개인정보에 등록 된 휴대폰 번호(이메일주소)를 변경하고싶어요.", 
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "스페셜 패키지의 아이템을 실수로 다른 캐릭터로 수령했어요.이동 가능한가요?", 
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "회원가입 시 부모동의를 받았는데 추가 회원가입 시 다시 받아야 하나요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = True,
                answer ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라.",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "회원탈퇴 기간은 얼마나 걸리나요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = False,
            answer = "",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "계정을 이용하고 있는데 추가계정 생성이 가능한가요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = False,
            answer = "",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "개명을 하여 이름이 변경되었습니다. 어떻게 해야 하나요?",
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = False,
            answer = "",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        },
        {
            title = "서비스 신청 주기는 어떻게 되나요?" ,
            answerDate = "19-01-01",
            createDate = "19-01-01",
            isAnswer = False,
            answer = "",
                article ="발휘하기 되는 위하여 찾아 튼튼하며, 소금이라 이상 천하를 있는가? 실현에 쓸쓸한 발휘하기 사랑의 무엇을 작고 속에 이상은 공자는 힘있다. 얼음과 희망의 새가 사랑의 아니한 것이 있으며, 것이다. 미인을 목숨이 못할 커다란 청춘의 무엇을 황금시대를 끓는다. 뭇 못하다 봄바람을 갑 찬미를 사막이다. 구하지 무한한 되려니와, 아니한 뜨고, 못할 그들의 같은 같은 봄바람이다. 청춘을 뭇 인간에 실로 시들어 약동하다. 발휘하기 얼음 이것을 것이다. 방황하여도, 물방아 위하여 청춘의 이상은 끓는다. 그들은 그들의 노년에게서 쓸쓸하랴? 그들에게 그들의 그러므로 있음으로써 온갖 보라."
        }
    ]
