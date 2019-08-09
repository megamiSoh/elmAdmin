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
import Page.Common exposing (..)
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Http as Http

type alias Model = 
    { session : Session
    , title : String
    , check : Bool
    , image : String
    , splash : Bool
    }


type alias SessionCheck = 
    { id : Int
    , username : String }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile=
    (
        { session = session
        , title = "" 
        , check = mobile
        , splash = if Session.viewer session == Nothing then False else True
        , image = "/image/lazy_bg_back.jpg"}
       , Cmd.batch[scrollToTop NoOp
       ,
       if Session.viewer session == Nothing then
       Cmd.none
       else
        Api.get Check Endpoint.sessionCheck (Session.cred session) (Decoder.sessionCheck SessionCheck)
        , Api.progressCalcuration ()]
    )
type Msg 
    = NoOp 
    | LoadImg
    | Check (Result Http.Error SessionCheck )
    | Complete E.Value
    -- | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Api.calcurationComplete Complete
    -- Session.changes GotSession (Session.navKey model.session)

onLoad msg =
    on "load" (Decode.succeed msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Complete val ->
            ({model | splash = False}, Cmd.none)
        Check (Ok ok) ->
            (model, Cmd.none)
        Check (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, Api.logout)     
            else 
            (model, Cmd.none)
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
            hometopTitle,
            div [class "homemenu"]
            

            [div [ class "home_main_middle" ]
    [ div [ class "columns home_yf_columns" ]
        [ div [ class "home_yf_columns_column1" ]
            [ p [ class "main_middle_1" ]
          
                      [ i [ class "fas fa-align-justify" ]
                            []
                        , text "공지사항"
                        ]
            ]
            
          , div [ class "home_yf_columns_column1" ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-won-sign" ]
                            []
                        , text "유어핏 가격"
                        ]
         
                    ]

          , div [ class "home_yf_columns_column1" ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-question" ]
                            []
                        , text "자주하는 질문"
                        ]
         
                    ]
        ]
                   
    ] ,  


    div [ class "home_videobox" ]
    [ h1 [ class "home_videobox_title1" ]
        [ text "추천합니다" ]
    , h1 [ class "home_videobox_title2" ]
        [ text "오늘의 유어핏 운동" ]
    , div [ class "main_videowrap" ]
        [ div [ class "main_videobox1" ]
            [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        ,div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        ]
       ]
      ]
    ]
       



    else 
         div [class"yf_home_wrap"]
        [div [ class "home_main_top lazyimage"

                    
                     ]
        [ div [ class "home_main_box_warp" ]
            [ div [ class "home_main_box" ]
              [] ],
            img [src "image/bg_back.png", onLoad LoadImg, class "shut"] []
        ],

         
         div [ class "container is-widescreen" ]
                 [
                    --  div [class "splash", style "display" (if model.splash then "block" else "none")][],
                     
        --              lazy lazyview model.image 
                     
        -- , 

        --    div[class "home_main_text"]
        --     [h1 [ class "home_main_text_h1" ]
        --         [ text "나를 위한 운동 방법 유어핏은 간단합니다." ],
        --         h1 [ class "home_main_text_h1" ]
        --         [ text "지금 PC와 모바일 어디서나 접속하세요!" ],
        --     img [ src "image/device.png", alt "device" ]
        --         []
        --          ]
        --    ,

    div [ class "home_main_middle" ]
    [ div [ class "columns home_yf_columns" ]
        [ div [ class "home_yf_columns_column1" ]
            [ p [ class "main_middle_1" ]
          
                      [ i [ class "fas fa-align-justify" ]
                            []
                        , text "공지사항"
                        ]
            ]
            
          , div [ class "home_yf_columns_column1" ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-won-sign" ]
                            []
                        , text "유어핏 가격"
                        ]
         
                    ]

          , div [ class "home_yf_columns_column1" ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-question" ]
                            []
                        , text "자주하는 질문"
                        ]
         
                    ]
        ]
                   
    ] ,  


    div [ class "home_videobox" ]
    [ h1 [ class "home_videobox_title1" ]
        [ text "추천합니다" ]
    , h1 [ class "home_videobox_title2" ]
        [ text "오늘의 유어핏 운동" ]
    , div [ class "main_videowrap" ]
        [ div [ class "main_videobox1" ]
            [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        ,div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        , div [ class "main_videobox1" ]
             [ div [ class "main_videobox1_thumbnail" ]
                []
            , h1 [ class "main_videobox_text" ]
                 [ text "식스팩운동" ]
            ]
        ]
    ]
                 
              
                   ,P.viewFooter
                 ]
        ]
        
  


lazyview image= 
    -- div [class "yf_banner_wrap"]
    --    [div [class "yf_banner_menu"]
       
    --    [div [ class "yf_banner_side_menu" ]
    -- [ div [ class "yf_banner_side_menu1" ]
    --     [ text "공지사항" ]
    -- , div [ class "yf_banner_side_menu2" ]
    --     [ text "요금제보기" ]
    -- , div [ class "yf_banner_side_menu3" ]
    --     [ text "자주하는질문" ]
    -- , div [ class "yf_banner_side_menu4" ]
    --     [ text "1:1문의" ]
    -- ]

    --     ]
           
    --    ,
     
     
     div [ class "home_main_top lazyimage", 
                     style "background-size" "cover" ,
                     style "background" ("0px -20rem / cover no-repeat url(" ++ image ++") fixed") 
                    --  , style "filter" "blur(4px)"
                    --  , style onLoad LoadImg 
                   
                    
                     ]
        [ div [ class "home_main_box_warp" ]
            [ div [ class "home_main_box" ]
              []
            ]
            ,
            img [src "image/bg_back.png", onLoad LoadImg, class "shut"] []
        ]
    



    
        
home =
     div [class "headerSpace"] [
    div [ class " m_home_topbox" ]
            [ img [ src "image/logo.png", alt "logo" ]
                []
            ]
     ]
hometopTitle = 
    div [class "home_subtitle"] []
        --  text "안녕하세요!" ,br []
        -- [] , text "당신만의 트레이너" 
        -- , span [ class "yourfit_text" ]
        -- [ text "유어핏" ], text "입니다."    
    
    

menuLayout idx item = 
        a [ class ("titlemenu" ++ String.fromInt (idx + 1)) , Route.href item.routing]
           [div [class "m_home_iconbox"] [
                img [src item.thumb ][]
            ]
            , div [class "menutext"] [
                p [class "maintext"] [text item.menuTitle]
            ]
    ]
menu = 
    [
        {
            thumb = "image/icon01.png",
            menuTitle = "유어핏운동",
            routing = Route.YourFitExer
        },
        {
            thumb = "image/icon00.png",
            menuTitle = "공지사항",
            routing = Route.Info
        },
        {
            thumb = "image/icon02.png",
            menuTitle = "맞춤운동",
            routing = Route.MakeExer
        },
        {
            thumb = "image/icon03.png",
            menuTitle = "함께해요",
            routing = Route.Together
        },
        {
            thumb = "image/icon04.png",
            menuTitle = "마이페이지",
            routing = Route.MyPage
        },
        {
            thumb = "image/icon05.png",
            menuTitle = "FAQ",
            routing = Route.Faq
        }
    ]
