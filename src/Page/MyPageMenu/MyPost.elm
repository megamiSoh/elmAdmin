module Page.MyPageMenu.MyPost exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port as P
import Json.Decode as Decode
import Json.Encode as E
import Route exposing(..)
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
        , P.checkMobile ()
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
                        ({model | checkDevice = ""}, Cmd.none)
        BackBtn ->
            (model, Route.backUrl(Session.navKey model.session) 1)
            

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
            commonJustHeader "/image/icon_management.png" "나의 게시물관리"
            , contentsBody
        ]
app =
    div[class "container"] [
        appHeaderback "내 게시물관리" "myPageHeader" BackBtn,
        appcontentsBody
    ]
contentsBody =
    div [ class "myPost_searchbox" ]
        [ div [ class "myPost_mediabox" ]
            [ ul [ class "cbp_tmtimeline" ]
               (List.map contentsLayout contentsData)
        ]
        ]
appcontentsBody =
    div [ class "myPost_searchbox" ]
        [ div [ class "m_myPost_mediabox" ]
            [ ul [ class "m_cbp_tmtimeline" ]
               (List.map appcontentsLayout contentsData)
        ]
        ]
contentsLayout item= 
    li []
        [ time [ class "cbp_tmtime" ]
            [ span []
                [ text item.createDate ]
            , span []
                []
            ]
        , div [ class "cbp_tmicon icon1" ]
            [ 
                if item.category == "together" then
                    i [ class "fas fa-pen" ]
                    []
                else
                    i [ class "fas fa-video" ]
                    []
            ]
        , div [ class "cbp_tmlabel" ]
            [ h2 []
                [ text item.title ]
            , p []
                [ a [ class "button" ]
                    [ text "삭제" ]
                ]
            ]
        ]

appcontentsLayout item= 
    li []
        [ time [ class "m_cbp_tmtime" ]
            [ span []
                [ text item.createDate ]
            , span []
                []
            ]
        , div [ class "m_cbp_tmicon icon1" ]
            [ 
                if item.category == "together" then
                    i [ class "fas fa-pen" ]
                    []
                else
                    i [ class "fas fa-video" ]
                    []
            ]
        , div [ class "m_cbp_tmlabel" ]
            [ h2 []
                [ text item.title ]
            , p []
                [ a [ class "button m_mypost_btn" ]
                    [ text "삭제" ]
                ]
            ]
        ]
    
    
contentsData =
    [
        {
            title = "운동 팁좀 공유해주세요!!",
            createDate = "19/01/01",
            category = "together"
        },
        {
            title = "여름맞이 뱃살빼기 운동영상",
            createDate = "19/01/01",
            category = "video"
        },
        {
            title = "운동 팁좀 공유해주세요!!",
            createDate = "19/01/01",
            category = "together"
        },
        {
            title = "여름맞이 뱃살빼기 운동영상",
            createDate = "19/01/01",
            category = "video"
        },
        {
            title = "여름맞이 뱃살빼기 운동영상",
            createDate = "19/01/01",
            category = "video"
        },
        {
            title = "운동 팁좀 공유해주세요!!",
            createDate = "19/01/01",
            category = "together"
        },
        {
            title = "여름맞이 뱃살빼기 운동영상",
            createDate = "19/01/01",
            category = "video"
        },
        {
            title = "운동 팁좀 공유해주세요!!",
            createDate = "19/01/01",
            category = "together"
        },
        {
            title = "여름맞이 뱃살빼기 운동영상",
            createDate = "19/01/01",
            category = "video"
        },
        {
            title = "운동 팁좀 공유해주세요!!",
            createDate = "19/01/01",
            category = "together"
        }
    ]