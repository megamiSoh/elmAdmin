module Page.Detail.TogetherWrite exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Json.Decode as Decode
import Json.Encode as E
import String
import Route exposing (..)
import Port as P
import Api as Api

type alias Model 
    = {
        session : Session
        ,fileName : String
        ,checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , fileName = ""
        , checkDevice = ""
        , check = mobile}
        , P.checkMobile ()
    )

type Msg 
    = GetFile String
    | CheckDevice E.Value
    | BackBtn

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions : Model -> Sub Msg
subscriptions model =
    P.check CheckDevice

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetFile filename ->
                ({model | fileName = filename}, Cmd.none)
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ ->
                        ({model | checkDevice = "pc"}, Cmd.none)
        BackBtn ->
            (model, Route.pushUrl (Session.navKey model.session) Route.Together)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
        if model.checkDevice =="pc" then
         web model
        else
         app model
    }

web model = 
  div [class "container"] [
    div [class "notification yf_workout"] [
        commonJustHeader "../image/icon_together.png" "함께해요",
        div [class "yf_yfworkout_search_wrap together"] [
            bodyContents model
            ,goBtn
        ]
    ]  ]

app model= 
    div [class "container"] [
        appHeaderConfirmDetail2 "게시물작성" "togetherHeader" BackBtn Route.Together "등록" ,
        bodyContents_app model
    ]
bodyContents model = 
    div [ class "togetherWrite_searchbox" ]
        [ div [ class "togetherWrite_mediabox" ]
            [ textarea [ class "textarea", placeholder "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요", rows 10 ]
                []
            ]
        , div [ class "file has-name is-fullwidth togetherWrite_yf_upload" ]
            [ label [ class "file-label" ]
                [ input [ class "file-input", type_ "file", name "resume" , onChange GetFile ]
                    []
                , span [ class "file-cta" ]
                    [ span [ class "file-icon" ]
                        [ i [ class "fas fa-upload" ]
                            []
                        ]
                    , span [ class "file-label" ]
                        [ text "사진올리기" ]
                    ]
                , span [ class "file-name" ]
                    [ 
                        if model.fileName == "" then
                            text "사진을 선택 해 주세요."
                        else 
                            text model.fileName
                    ]
                ]
            ]
        ]

bodyContents_app model = 
    div [ class "m_togetherWrite_searchbox" ]
        [ div [ class "m_togetherWrite_mediabox" ]
            [ textarea [ class "textarea m_togetherWrite_textarea", placeholder "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요", rows 10 ]
                []
            ]
    
        ]
                
                    
goBtn = 
    div [] [
        div [ class " togetherWrite_yf_dark" ]
            [ a [ class "button is-dark togetherWrite_yf_dark" , Route.href Route.Together]
                [ text "올리기" ]
            ]
        , div [ class "yf_butbox" ]
            [ a [ class "button togetherWrite_yf_butbox", Route.href Route.Together ]
                [ text "뒤로" ]
            ]
    ]


onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Decode.map tagger targetValue)

targetFiles : Decode.Decoder (List String)
targetFiles = 
    Decode.at ["target", "files"] (Decode.list Decode.string)



