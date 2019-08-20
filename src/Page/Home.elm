port module Page.Home exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Atrr exposing(..) 
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
    , bannerIndex : Int
    , bannerPosition : String
    , transition : String
    , checkId : String
    , last_bullet : Int
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
    , title : String
    , backcolor : Maybe String}

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
        , bannerIndex = 0
        , bannerPosition = ""
        , transition = "shiftThing"
        , checkId = ""
        , last_bullet = 5
        }
       , Cmd.batch[scrollToTop NoOp
       ,
       if Session.viewer session == Nothing then
       Cmd.none
       else
        Api.get Check Endpoint.sessionCheck (Session.cred session) (Decoder.sessionCheck SessionCheck)
        , Api.progressCalcuration ()
        , bannerApi session
        ]
    )
type Msg 
    = NoOp 
    | LoadImg
    | Check (Result Http.Error SessionCheck )
    | Complete E.Value
    | BannerComplete (Result Http.Error BannerListData)
    | SlideMove String
    | SilderRestart E.Value
    | AutoSlide E.Value
    | TransitionCheck E.Value
    | SwipeDirection E.Value
    | BulletGo Int
    -- | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Sub.batch
    [ Api.calcurationComplete Complete
    , Api.sliderRestart SilderRestart
    , Api.autoSlide AutoSlide
    , Api.transitionCheck TransitionCheck
    , Api.swipe SwipeDirection
    ]
    -- Session.changes GotSession (Session.navKey model.session)

onLoad msg =
    on "load" (Decode.succeed msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BulletGo idx ->
            ({model | transition = "", bannerPosition = "-" ++ String.fromInt (idx * 100), bannerIndex = idx, last_bullet = 150}, Cmd.none)
        SwipeDirection direction ->
            let _ = Debug.log "direction" direction
            in
            case Decode.decodeValue Decode.string direction of
                Ok ok ->
                    case ok of
                        "right" ->
                            update (SlideMove "right") model
                        "left" ->
                            update (SlideMove "left") model
                        _ ->
                            (model, Cmd.none)
                Err err ->
                    (model, Cmd.none)
        TransitionCheck check ->
            case Decode.decodeValue Decode.string check of
                Ok ok ->
                    case ok of 
                        "right" ->
                            ({model | transition = "", bannerPosition = "0", bannerIndex = 0}, Cmd.none)
                        "left" ->
                            ({model | transition = "", bannerPosition = "-" ++ String.fromInt ((List.length model.bannerList) * 100)}, Cmd.none)
                        _ ->
                            ({model | checkId = ""}, Cmd.none)
                Err err ->
                    (model, Cmd.none)
        AutoSlide auto ->
            let
                index = 
                    if List.length model.bannerList >= model.bannerIndex then model.bannerIndex + 1 else 0
            in
            if model.bannerPosition == "-" ++ String.fromInt ((List.length model.bannerList) * 100) then
            (model, Cmd.none)
            else
                    ({model | bannerIndex = index
                    , bannerPosition = "-" ++ String.fromInt ((model.bannerIndex + 1) * 100)
                    , transition = "shiftThing"
                    , last_bullet = if List.length model.bannerList == index then 0 else 150}
                    , Cmd.none)
        SilderRestart restart ->
            (model, Cmd.none)
        SlideMove direction ->
            case direction of
                "right" ->
                    let
                        index = 
                            if List.length model.bannerList >= model.bannerIndex then model.bannerIndex + 1 else 0
                    in
                    if model.bannerPosition == "-" ++ String.fromInt ((List.length model.bannerList) * 100) then
                    (model, Cmd.none)
                    else
                            ({model | bannerIndex = index
                            , bannerPosition = "-" ++ String.fromInt ((model.bannerIndex + 1) * 100)
                            , transition = "shiftThing"
                            , checkId = "stopInterval"
                            , last_bullet = if List.length model.bannerList == index then 0 else 150}
                            , Cmd.none)
                    
                "left" ->
                    let
                        index = 
                            if 1 < model.bannerIndex then model.bannerIndex - 1 else List.length model.bannerList
                    in
                    if model.bannerPosition ==  "-0"  then
                    (model, Cmd.none)
                    else
                    ({model | bannerIndex = index
                    , bannerPosition = "-" ++ String.fromInt ((model.bannerIndex - 1) * 100)
                    , transition = "shiftThing"
                    , checkId = "stopInterval"
                    , last_bullet = if List.length model.bannerList == index then 0 else 150}
                    , Cmd.none)
                _ ->
                    (model, Cmd.none)
        BannerComplete (Ok ok) ->
            ({model | bannerList = ok.data}
            , Api.slide (E.string (String.fromInt (List.length ok.data))))
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


caseList item = 
    case List.head item of
                Just list ->
                    list
                Nothing ->
                    { description = ""
                    , id = 0
                    , is_link = False
                    , link = Nothing
                    , src = ""
                    , target = Nothing
                    , title = ""
                    , backcolor = Nothing}

webOrApp model =
    let
        bannerFirst = 
            caseList model.bannerList
    in
    if model.check then
        div [class "appWrap"] [
            home,
             div [class "home_main_top "]
            [ div [ class "app_bannerimg_container", id model.checkId]
                [ i [ class "fas fa-chevron-left appsliderBtn" , onClick (SlideMove "left")]
                []
                , div [class ("bannerList_items " ++  model.transition), id "slide", style "left" (model.bannerPosition ++ "%")] (  List.map banner model.bannerList 
                ++ [banner bannerFirst]
                )  
                , i [ class "fas fa-chevron-right appsliderBtn" , onClick (SlideMove "right")] []
                , 
                div [class "bullet_container"] (List.indexedMap (\idx x -> bulletitems idx x model) model.bannerList)
                ]
            
            ],
            div [class "homemenu"]
            [ div [ class "home_main_middle" ]
            [ 
                apphomeDirecMenu        
            ] ,  
            apprecommendWorkOutList
      ]
    ]
    else 
         div [class"yf_home_wrap"]
        [ 
            div [class "home_main_top "]
            [ div [ class "bannerList_container", id model.checkId]
                [ i [ class "fas fa-chevron-left sliderBtn" , onClick (SlideMove "left")]
                []
                , div [class ("bannerList_items " ++  model.transition), id "slide", style "left" (model.bannerPosition ++ "%")] (  List.map banner model.bannerList 
                ++ [banner bannerFirst]
                )  
                , i [ class "fas fa-chevron-right sliderBtn" , onClick (SlideMove "right")] []
                , 
                    div [class "bullet_container"] (List.indexedMap (\idx x -> bulletitems idx x model) model.bannerList)
                ]
            ],
         div [ class "container is-widescreen" ]
            [ homeDirectMenu
            , recommendWorkOutList
            , P.viewFooter
            ]
        ]
        
bulletitems idx item model = 
    div [classList 
        [("bullet_items", True)
        , ("selected_bullet", model.bannerIndex == idx)
        , ("selected_bullet",  model.last_bullet == idx )
        ]
        , onClick (BulletGo idx)
        ][]

apphomeDirecMenu =
    div [ class "columns home_yf_columns" ]
        [ a [ class "home_yf_columns_column1" , Route.href Route.Info ]
            [ p [ class "main_middle_1" ]
          
                      [ i [ class "fas fa-align-justify" ]
                            []
                        , text "공지사항"
                        ]
            ]
            
          , a [ class "home_yf_columns_column1", Route.href Route.YP]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-won-sign" ]
                            []
                        , text "유어핏 가격"
                        ]
         
                    ]

          , a [ class "home_yf_columns_column1" , Route.href Route.Faq ]
                    [ p [ class "main_middle_1" ]
                        [ i [ class "fas fa-question" ]
                            []
                        , text "자주하는 질문"
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
    

apprecommendWorkOutList = 
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

caseString item= 
    case item of
        Just ok ->
            ok
        Nothing ->
            ""
        
banner item =
        a [class "bannerimg_container", style "background-color" (caseString item.backcolor), Atrr.href (caseString item.link), target (caseString item.target)][img [src item.src, onLoad LoadImg, class "slideItems"] []]



home =
     div [class "headerSpace"] [
    div [ class " m_home_topbox" ]
            [ img [ src "image/logo.png", alt "logo" ]
                []
            ]
     ]