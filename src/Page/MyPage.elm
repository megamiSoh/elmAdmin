port module Page.MyPage exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Page.MyPageMenu.MyPageInfoLayout exposing(..)
import Route exposing (..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Api as Api
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Http as Http

type alias Model =
    { session : Session
    , selecTab : String
    , checkDevice : String
    , check : Bool
    , mydata : MyData
    }

type alias DataWrap = 
    { data : MyData }

type alias MyData =
    { exercise : Int
    , share : Int
    , user : UserData }

type alias UserData =
    { id : Int
    , nickname : Maybe String
    , username : String}

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile = 
    let _ = Debug.log "login" Session.cred
        
    in
    
    (
        { session = session
        , selecTab = "myInfo"
        , checkDevice = ""
        , check = mobile
        , mydata = 
            { exercise = 0
            , share = 0
            , user = 
                { id = 0
                , nickname = Nothing
                , username = ""}
             }}
        , Cmd.batch
        [ P.checkMobile ()
        , Api.get MyInfoData Endpoint.myInfo (Session.cred session) (Decoder.dataWRap DataWrap MyData UserData)
        ]
    )

type Msg 
    = ClickRight
    | ClickLeft
    | SelectTab String
    | CheckDevice E.Value
    | MyInfoData (Result Http.Error DataWrap)

subscriptions :Model -> Sub Msg
subscriptions model=
    P.check CheckDevice

port scrollRight : () -> Cmd msg
port scrollLeft : () -> Cmd msg

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyInfoData (Ok ok) ->
            ({model | mydata = ok.data}, Cmd.none)
        MyInfoData (Err err) ->
            let _ = Debug.log "err" err
                
            in
            
            (model, Cmd.none)
        ClickRight ->
            ( model, scrollRight () )
        ClickLeft ->
            (model , scrollLeft ())
        SelectTab tab->
            ({model | selecTab = tab} , Cmd.none)
        CheckDevice str ->
           let
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ -> 
                        ({model | checkDevice = "mobile"}, Cmd.none)



view : Model -> {title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content =       
        if model.check then
        div [] [
            appHeadermypage "마이페이지" "myPageHeader" ,
            app model
        ]
        else 
        web model
        
    }
app model = 
     div [class "container"] [
         appTopMenu model,
         div [](List.map menuBottom (menu))
     ]
appTopMenu model = 
    div [ class "m_mypage_loginbox" ]
        [ div [ class "m_mypage_profilebox" ]
            [ img [ src "/image/profile.png" ]
                []
            , p []
                [ text (justData model.mydata.user.nickname) ]
            ]
        -- , div [ class "m_mypage_subbox" ]
        --     [ p []
        --         [ text "1" ]
        --     , p []
        --         [ text "게시판" ]
        --     ]
        , div [ class "m_mypage_subbox" ]
            [ p []
                [ text (String.fromInt(model.mydata.exercise))]
            , p []
                [ text "맞춤운동" ]
            ]
        , div [ class "m_mypage_subbox" ]
            [ p []
                [ text (String.fromInt(model.mydata.share)) ]
            , p []
                [ text "스크랩" ]
            ]
        , a [ class "button is-dark m_mypage_mybtn" , Route.href Route.MyAccount]
                [ text "내 정보설정" ]
        ]
menuBottom item = 
        a [ class "m_mypage_mypagemenu" , Route.href item.route]
        [ img [ src item.icon ]
            [], text item.title
        ]
justData cases = 
    case cases of
        Just a ->
            a
    
        Nothing ->
            "내 정보 설정에서 닉네임을 설정해 주세요."
web model = 
    div [] [
            -- myPageCommonHeader ClickRight ClickLeft
            -- ,
            div [ class "containerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonJustHeader "/image/icon_mypage.png" "내 정보설정" ,
                        tabPanner model
                    ]
                ]
            ]
        ]
tabPanner model = 
        div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "tabs is-toggle is-fullwidth is-large " ]
                [ ul []
                    [ li [ 
                        classList [
                            ("myPage_yf_active", model.selecTab == "myInfo")
                        ] , onClick (SelectTab "myInfo")
                     ]
                        [ p []
                            [ span []
                                [ text "내 정보설정" ]
                            ]
                        ]
                    -- , li [
                    --      classList [
                    --         ("myPage_yf_active", model.selecTab == "bodyInfo")
                    --     ] , onClick (SelectTab "bodyInfo")
                    -- ]
                    --     [ p []
                    --         [ span []
                    --             [ text "신체기록관리" ]
                    --         ]
                    --     ]
                    , li [
                         classList [
                            ("myPage_yf_active", model.selecTab == "accountInfo")
                        ] , onClick (SelectTab "accountInfo")
                    ]
                        [ p []
                            [ span []
                                [ text "계정관리" ]
                            ]
                        ]
                    ]
                ]
            ]
            ,
            div [ class "myPage_mediabox" ]
            [
                if model.selecTab == "myInfo" then
                    myInfo
                else 
                    if model.selecTab == "bodyInfo" then
                    bodyInfo
                    else
                    accountInfo
            ]
        ]

menu = 
        
            [
            -- {icon = "/image/icon_calendar.png", title ="캘린더", route = Route.MyC},
            -- {icon = "/image/icon_diet.png", title ="식단기록", route = Route.MealRM},
            -- {icon = "/image/icon_stats.png", title ="나의 통계", route = Route.MyS},
            -- {icon = "/image/icon_list.png", title ="스크랩리스트", route = Route.MyScrap},
            -- {icon = "/image/icon_management.png", title ="내 게시물 관리", route= MyPost},
            -- {icon = "/image/icon_notice.png", title ="공지사항", route = Route.Info},
            -- {icon = "/image/icon_qna.png", title ="1:1 문의", route = Route.Faq},
            {icon = "/image/icon_qna.png", title ="로그아웃", route = Route.Logout}
            ]