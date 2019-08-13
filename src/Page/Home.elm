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
    , bannerList : List BannerList
    }


type alias SessionCheck = 
    { id : Int
    , username : String }

type alias BannerListData = 
    { data : List BannerList }

type alias BannerList =
    { description : String
    , id : Int
    , is_link : Bool
    , link : Maybe String
    , src : String
    , target : Maybe String
    , title : String}

bannerApi session = 
    Api.get BannerComplete Endpoint.bannerList (Session.cred session)(Decoder.bannerListdata BannerListData BannerList)

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile=
    (
        { session = session
        , title = "" 
        , check = mobile
        , splash = if Session.viewer session == Nothing then False else True
        , image = "/image/lazy_bg_back.jpg"
        , bannerList = []
        }
       , Cmd.batch[scrollToTop NoOp
       ,
       if Session.viewer session == Nothing then
       Cmd.none
       else
        Api.get Check Endpoint.sessionCheck (Session.cred session) (Decoder.sessionCheck SessionCheck)
        , Api.progressCalcuration ()
        , bannerApi session]
    )
type Msg 
    = NoOp 
    | LoadImg
    | Check (Result Http.Error SessionCheck )
    | Complete E.Value
    | BannerComplete (Result Http.Error BannerListData)
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
        BannerComplete (Ok ok) ->
            ({model | bannerList = ok.data}, Cmd.none)
        BannerComplete (Err err) ->
            (model, Cmd.none)
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
            -- home,
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
        [div [ class "home_main_top lazyimage"]
        [ div [ class "home_main_box_warp" ]
          (List.map banner model.bannerList),

         
         div [ class "container is-widescreen" ]
        [ homeDirectMenu
        , recommendWorkOutList
        , P.viewFooter
        ]
        ]
        ]
        
  
homeDirectMenu = 
    div [ class "home_main_middle" ]
    [ div [ class "columns home_yf_columns" ]
        [ a [ class "home_yf_columns_column1" , Route.href Route.Info ]
            [  p [ class "main_middle_1"]
          
                      [ i [ class "fas fa-align-justify" ]
                            []
                        , text "공지사항"
                        ]
            ]
            
          , a [ class "home_yf_columns_column1", Route.href Route.YP ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-won-sign" ]
                            []
                        , text "유어핏 가격"
                        ]
         
                    ]

          , a [ class "home_yf_columns_column1", Route.href Route.Faq  ]
                    [ p [ class "main_middle_1"]
                        [ i [ class "fas fa-question" ]
                            []
                        , text "자주하는 질문"
                        ]
         
                    ]
        ]
                   
    ] 


lazyview image= 
     div [ class "home_main_top lazyimage", 
        style "background-size" "cover" ,
        style "background" ("0px -20rem / cover no-repeat url(" ++ image ++") fixed") 
        ]
        [ div [ class "home_main_box_warp" ]
            [ div [ class "home_main_box" ]
              []
            ]
            ,
            img [src "image/bg_back.png", onLoad LoadImg, class "shut"] []
        ]
    



recommendWorkOutList = 
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

    
        
        
banner item =
    img [src item.src, onLoad LoadImg] []