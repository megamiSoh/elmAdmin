module Page.Detail.FaqWrite exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Api as Api

type alias Model 
    = {
        session : Session
        , checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , check = mobile
        , checkDevice = ""}
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
                        ({model | checkDevice =string}, Cmd.none)
                
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
app =
    div [class "container"] [
        appHeaderBothBtn "문의하기" "myPageHeader" Route.Faq "fas fa-angle-left" "등록" Route.Faq
        , apptitle
        , apptextArea
    ]
web = 
    div [class "container"] [
        commonJustHeader "/image/icon_qna.png" "1:1문의"
        , title
        , textArea
        , uploadBtn
        , backBtn
    ]
title = 
        input [ class "input tapbox", type_ "text", placeholder "제목을 입력해주세요" ]
                []
textArea =
        textarea [ class "textarea tapbox", placeholder "내용을 입력해주세요", rows 10 ]
        []
uploadBtn = 
    div [ class "togetherWrite_yf_dark" ]
        [ a [ class "button is-dark", Route.href Route.Faq]
            [ text "올리기" ]
        ]
backBtn = 
    div [ class "faqWrite_backbtn" ]
        [ a [ class "button yf_back",Route.href Route.Faq]
            [ text "뒤로" ]
        ]

apptitle = 
        input [ class "input tapbox fnqapptitle", type_ "text", placeholder "제목을 입력해주세요" ]
                []
apptextArea =
        textarea [ class "textarea fnqtapbox", placeholder "내용을 입력해주세요", rows 10 ]
        []