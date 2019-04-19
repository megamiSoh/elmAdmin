port module Page.Home exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing(..)
import Session exposing(Session)
import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)
import Api as Api
import Page as P
type alias Model 
    = {
        session : Session,
        title : String
         , check : Bool
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile=
    (
        { session = session
        , title = "" 
        , check = mobile}
       , Cmd.none
    )
type Msg = 
    NoOp 
    -- | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Sub.none
    -- Session.changes GotSession (Session.navKey model.session)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        -- GotSession session ->
        --     ({model | session = session}
        --     , Cmd.none
        --     )
    
view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFit"
    , content = 
        webOrApp model
    }

webOrApp model =
    if model.check then
        div [class "appWrap"] [
            home,
            div [ class "m_homemenu" ]
            (List.map menuLayout menu)
        ]
    else 
         div [ class "yf_contentswarp" ]
            [ 
                div [ class "columns" ]
                [ div [ class "column yf_col" ]
                    [ img [ src "/image/home_main.png" ]
                        []
                    ]
                , div [ class "column yf_col" ]
                    [ img [ src "/image/home_pic.png" ]
                        []
                    ]
                ],P.viewFooter
            ]
    
        
home =
    div [ class "m_home_topbox" ]
            [ img [ src "image/logo.png", alt "logo" ]
                []
            ]
menuLayout item= 
    a [ class "m_home_titlemenu" , Route.href item.routing]
            [ div [ class "m_home_iconbox" ]
                [ img [ src item.thumb , alt "menu" ]
                    []
                ]
            , div [ class "m_home_menutext" ]
                [ ul []
                    [ li [ class "m_home_subtext" ]
                        [ text item.description ]
                    , li [ class "m_home_maintext" ]
                        [ strong []
                            [ text item.menuTitle ]
                        ]
                    ]
                ]
            ]
menu = 
    [
        {
            thumb = "image/m.workout.png",
            description = "하루 15분 당신만을 위한",
            menuTitle = "유어핏운동",
            routing = Route.YourFitExer
        },
        {
            thumb = "image/m.customworkout.png",
            description = "내가 만들어가는 운동레시피",
            menuTitle = "맞춤운동",
            routing = Route.MakeExer
        },
        {
            thumb = "image/m.together.png",
            description = "우리가 만들어가는 스토리",
            menuTitle = "함께해요",
            routing = Route.Together
        },
        {
            thumb = "image/m.mypage.png",
            description = "내 정보를 한눈에",
            menuTitle = "마이페이지",
            routing = Route.MyPage
        }
    ]
