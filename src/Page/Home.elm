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
import Html.Lazy exposing (lazy)
type alias Model 
    = {
        session : Session,
        title : String
         , check : Bool
         , image : String
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile=
    (
        { session = session
        , title = "" 
        , check = mobile
        , image = "/image/lazy_bg_back.jpg"}
       , Cmd.none
    )
type Msg 
    = NoOp 
    | LoadImg
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

onLoad msg =
    on "load" (Decode.succeed msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadImg ->
            ({model | image = "/image/bg_back.png"}, Cmd.none)
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
                     lazy lazyview model.image 
                     
        , 
        -- 
        -- ,
    div [ class "home_main_middle" ]
    [ div [ class "columns home_yf_columns" ]
        [ div [ class "column1" ]
            [ p [ class "main_middle_1" ]
                [ i [ class "fas fa-arrow-circle-right" ]
                    []
                , text "D.I.Y"
                ]
            , h1 [ class "main_middle_2" ]
                [ text "사용자 직접 만드는 운동 커리큘럼" ]
            ]
            
        , div [ class "column2" ]
            [ p [ class "main_middle_1" ]
                [ i [ class "fas fa-arrow-circle-right" ]
                    []
                , text "743"
                ]

                ,
                 h1 [ class "main_middle_2" ]
                  [ text "743개의 다양한 운동영상" ]
             ]

          , div [ class "column3" ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-arrow-circle-right" ]
                            []
                        , text "나,너,우리"
                        ]
                    , h1 [ class "main_middle_2" ]
                        [ text "사용자 커뮤니티형 운동 시스템" ]

                    ]
        ]
    ,
            div [ class "main_end" ]
                    [ img [ src "image/login_logo.png", alt"logo"]
                        []

                    , h1 [ class "home_last_title" ]
                     [text "사용자 커스텀 D.I.Y 트레이닝 유어핏,PC, 모바일 어디서나 접속하세요!"]

                    
                    ]                       
    ]   
                ,P.viewFooter
            ]
  


lazyview image= 
    div [ class "home_main_top lazyimage", 
                     style "background-size" "cover" ,
                     style "background" ("0px -20rem / cover no-repeat url(" ++ image ++") fixed") 
                    --  , style "filter" "blur(4px)"
                    --  , style onLoad LoadImg 
                   
                    
                     ]
        [ div [ class "home_main_box_warp" ]
            [ div [ class "home_main_box" ]
                [ h1 [ class "home_main_title" ]
                    [ text "당신만을 위한 운동트레이닝" ]
                , p [ class "home_main_title2" ]
                    [ text "YOUR FIT" ]
                ]
            ]
            , img [src "image/bg_back.png", onLoad LoadImg, class "shut"] []
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
