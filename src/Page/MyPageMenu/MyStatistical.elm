module Page.MyPageMenu.MyStatistical exposing (..)
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
type alias Model 
    = {
        session : Session,
        activeTab : String,
        checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,activeTab = "weekly"
        , check = mobile
        ,checkDevice =""}
        , P.checkMobile ()
    )

type Msg 
    = ActiveTab String
    | CheckDevice E.Value
    | BackBtn

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    P.check CheckDevice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActiveTab item ->
            ( {model | activeTab = item}, P.checkMobile () )
        CheckDevice str ->
           let 
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
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
            web model
        else
            app model BackBtn
    }


web model = 
        div [ class "container" ]
            [
                commonJustHeader "/image/icon_stats.png" "나의 통계" ,
                div [ class "yf_yfworkout_search_wrap" ]
                [
                    tabBox model,
                    bodyContents
                ]
            ]

app model btn = 
    div [class "container"] [
        appHeaderConfirmDetailR "나의 통계" "myPageHeader" btn btn "확인",
        div [ class "yf_yfworkout_search_wrap" ]
                [
                    apptabBox model,
                    bodyContents
                ]
    ]
tabBox model = 
    div [ class "tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large " ]
            [ ul []
                [ li [ classList [
                    ("myStatistical_yf_active", model.activeTab == "weekly")
                ], onClick (ActiveTab "weekly") ]
                    [ 
                         text "주간별" 
                    ]
                , li [ classList [
                    ("myStatistical_yf_active", model.activeTab == "monthly")
                ], onClick (ActiveTab "monthly")]
                    [ 
                         text "월별"
                    ]
                ]
            ]
        ]
apptabBox model = 
    div [ class "m_tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth m_myStatistical_tabs" ]
            [ ul []
                [ li [ classList [
                    ("m_myStatistical_yf_active", model.activeTab == "weekly")
                ], onClick (ActiveTab "weekly") ]
                    [ 
                         text "주간별" 
                    ]
                , li [ classList [
                    ("m_myStatistical_yf_active", model.activeTab == "monthly")
                ], onClick (ActiveTab "monthly")]
                    [ 
                         text "월별"
                    ]
                ]
            ]
        ]
bodyContents =
    div [ class "myStatistical_searchbox" ]
        [ div [ class "myStatistical_mediabox" ]
            [ div [ class "myStatistical_inbox" ]
                [ text "체중" ]
            , div [ class "myStatistical_inbox" ]
                [ text "운동" ]
            , div [ class "myStatistical_inbox" ]
                [ text "섭취칼로리" ]
            ]
        ]