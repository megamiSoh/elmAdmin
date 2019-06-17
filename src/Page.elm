module Page exposing (..)

-- import VideoCall exposing(VidioCallPortMsg)
import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route exposing (Route)
import Session exposing(Session)
import Json.Encode as E
import Json.Decode as Decode
import Api exposing(..)
import Page.Common exposing(..)
type Page 
    = Home
    | Other
    | MakeExer
    | MyPage
    | Together
    | YourfitExer
    | YourfitDetail
    | YourFitList
    | Filter
    | FilterS1
    | FilterS2
    | TogetherW
    | Faq
    | Info
    | MealR
    | MyC
    | MyPost
    | MyScrap
    | MyS
    | MakeDetail
    | MyAccount
    | MealRM
    | InfoD
    | FaqD
    | FaqW
    | Login
    | Signup
    | SC
    | MakeEdit
    | Empty
    | EditFilter
    | PostD
    | ScrapD
    | MSearch
    | MakeEditLast
    | Private
    | SetPwd
    | FPwd
    | TA
    | C
    | CD
    | MJList

-- view:Maybe Cred -> Api.Check  -> Page -> {check : String , title : String, content: Html msg} -> Browser.Document msg
view maybeViewer checkB  page { title, content}  =
    if checkB then
        {title = title , body =
         appContents content page maybeViewer
         :: [footerCommon page]}
                
    else
        { title = title 
        , body = viewHeader page maybeViewer:: webContents content page maybeViewer:: []
        }




footerCommon page = 
    if page == MakeExer then
        appFooter page
    else if page == MyPage then
        appFooter page
    else if page == Together then
        appFooter page
    else if page == YourfitExer then
        appFooter page
    else
        div [] []


need2login = 
    div [ class "need2login" ]
        [img [src "../image/login.png"]
        []
    ,div [class"need2login_text"] [text "로그인 후 이용가능한 서비스 입니다."] 
        , a [class "button is-link need2login_btn", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
    ]
    

need2loginApp = 
    div [ class "m_nave2login" ]
        [img [src "../image/login.png"]
        []
    ,div [class"need2login_text"] [text "로그인 후 이용가능한 서비스 입니다."] 
        , a [class "button is-link need2login_btn", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
    ]
       

type Msg = NoOp


webContents content page maybeViewer=
    case maybeViewer of
        Just _ ->
            if page == MyPage then
            div [ class "" ][ 
                content, webtoastPop
                ,logoutlayer ]
            else
            div [ class "" ][ 
                content, webtoastPop
                ,logoutlayer ]
        Nothing ->
            if page == MakeExer then
            need2login
            else if page == MyPage then
            need2login
            
            else 
            div [] [content, webtoastPop]
  

appContents content page maybeViewer=
  case maybeViewer of
        Just _ ->
            
            div [ class "appWrap" ][ content, apptoastPop,mlogoutlayer ]
        Nothing ->
            if page == MakeExer  then
            div [][ 
                appHeaderSearch "맞춤운동" "makeExerHeader"
                , need2loginApp
            ]
            else if page == MyPage then
            div [][ 
                justappHeader "마이페이지" "myPageHeader"
                , div [ class "loginHeight"] [
                    div [class "loginbottom"] [text "로그인 후 이용가능한 서비스 입니다."] 
                , a [class "button", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
                ]
                , div [](List.map menuBottom (menu))

            ]
            else if page == C then
            div [][ 
                justappHeader "1:1문의" "myPageHeader"
                , need2loginApp

            ]
            else
            div [] [content, apptoastPop]
  

menuBottom item = 
        a [ class "m_mypage_mypagemenu" , Route.href item.route]
        [ img [ src item.icon ]
            [], text item.title
        ]

menu = 
    [
    -- {icon = "/image/icon_calendar.png", title ="캘린더", route = Route.MyC},
    -- {icon = "/image/icon_diet.png", title ="식단기록", route = Route.MealRM},
    -- {icon = "/image/icon_stats.png", title ="나의 통계", route = Route.MyS},
    -- {icon = "/image/icon_list.png", title ="스크랩리스트", route = Route.MyScrap},
    -- {icon = "/image/icon_management.png", title ="내 게시물 관리", route= MyPost},
    {icon = "/image/icon_notice.png", title ="공지사항", route = Route.Info},
    -- {icon = "/image/icon_qna.png", title ="1:1 문의", route = Route.Faq},
    {icon = "/image/icon_qna.png", title ="로그인", route = Route.Login}
    ]


webtoastPop = 
    div [ id "webToast" ]
    [ text "" ]
                
apptoastPop = 
    div [ id "appToast" ]
    [ text "" ]

viewHeader : Page -> Maybe Cred ->  Html msg
viewHeader page maybeViewer =
    case maybeViewer of
        Just _ ->
            nav [ class "navbar yf_navbar" ]
                [ div [ class "navbar-brand yf_brand" ]
                    [ a [ class "navbar-item yf_logo", Route.href Route.Home ]
                        []
                    , div [  class "navbar-burger burger" ]
                        [ span []
                            []
                        , span []
                            []
                        , span []
                            []
                        ]
                    ]
                , div [ id "expandMenu", class "navbar-menu yf_menu " ]
                    [ div [ class "navbar-start yf_start" ]
                        [ 
                        a [ class "navbar-item yf_logoRight", Route.href Route.Home ]
                        []
                        , a [ class "navbar-item yf_item", Route.href Route.Logout ]
                            [ text "로그아웃" ]
                        , a [ class "navbar-item yf_item", Route.href Route.YourFitExer ]
                            [ text "유어핏운동" ]
                        , a [ class "navbar-item yf_item", Route.href Route.MakeExer ]
                            [ text "맞춤운동" ]
                        , a [ class "navbar-item yf_item", Route.href Route.Together ]
                            [ text "함께해요" ]
                        , 
                        case page of
                            MyPage ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            Faq ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            MyC ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            Info ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            ScrapD ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            PostD ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            FaqW ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            FaqD ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            InfoD ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            MealR ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            MyPost ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            MyScrap  ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            C  ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            MyS  ->
                                a [class "navbar-item yf_item",  Route.href Route.MyPageBottomMenu ] [text "마이페이지"]
                            _ ->
                                a [ class "navbar-item yf_item",Route.href Route.MyPage ]
                                [ text "마이페이지"]
                        ]
                    ]
                ]

                
        Nothing ->
            nav [ class "navbar yf_navbar" ]
                [ div [ class "navbar-brand yf_brand" ]
                    [ a [ class "navbar-item yf_logo", Route.href Route.Home ]
                        []
                    , div [  class "navbar-burger burger" ]
                        [ span []
                            []
                        , span []
                            []
                        , span []
                            []
                        ]
                    ]
                , div [ id "expandMenu", class "navbar-menu yf_menu" ]
                    [ div [ class "navbar-start yf_start" ]
                        [a [ class "navbar-item yf_logoRight", Route.href Route.Home ]
                        []
                        , a [ class "navbar-item yf_item", Route.href Route.Login ]
                            [ text "로그인/회원가입" ]
                        , a [ class "navbar-item yf_item", Route.href Route.YourFitExer ]
                            [ text "유어핏운동" ]
                        , a [ class "navbar-item yf_item", Route.href Route.MakeExer ]
                            [ text "맞춤운동" ]
                        , a [ class "navbar-item yf_item", Route.href Route.Together ]
                            [ text "함께해요" ]
                        , a [ class "navbar-item yf_item",Route.href Route.MyPage ]
                            [ text "마이페이지"]
                        ]
                    ]
                ]

appFooter : Page -> Html msg
appFooter page= 
    footer [ class "m_yf_navfooter", id "footer" ]
        [ a [ class "m_appmanu" , Route.href Route.Home,classList [("bgFooter", page == Home) ]]
            [ img [ src "/image/m.icon.home.png"]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.YourFitExer ,classList [("bgFooter", page == YourfitExer) ]]
            [ img [ src "/image/m.icon.workout.png" ]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.MakeExer ,classList [("bgFooter", page == MakeExer) ]]
            [ img [ src "/image/m.icon.customworkout.png" ]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.Together ,classList [("bgFooter", page == Together) ]]
            [ img [ src "/image/m.icon.together.png"]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.MyPage ,classList [("bgFooter", page == MyPage) ]]
            [ img [ src "/image/m.icon.mypage.png" ]
                []
            ]
        ]
-- viewFooter : Html msg
viewFooter =
        footer [ class "footer yf_footer" ]
            [ div [ class "yf_address" ]
                [ ul [ class "yf_footer_address" ]
                    [ li []
                        [ text "주식회사 파이널 컴퍼니" ]
                    , li []
                        [ text "대표이사 최승병 │ 사업자번호 119-86-86816  |  대표전화 02-722-9502~9503" ]
                    , li []
                        [ text "사업장소재지 서울특별시 종로구 종로 19, 르메이르스포츠센터 B3층 연구소  │ 이메일 info@finalcompany.co.kr" ]
                    , li []
                        [ text "Copyright © 2019 Final Company, Inc. All Rights Reserved" ]
                    -- , li [] [
                    --     button [onClick Calling] []
                    -- ]
                    ]
                ]
            ]





isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        (Home, Route.Home) -> 
            True
        _ ->
            False



viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]

mlogoutlayer =
    div [ id "logoutPop"] [
        div [ class "myf_popup" ]
    [ img [ src "/image/icon_logout.png" ]
        []
    , h1 [ class "popup_yf_h1" ]
        [ text "로그아웃 하시겠습니까?" ]
    , p [ class "yf_logoout_butbox" ]
        [ a [ class "button is-danger logout_danger", Route.href Route.LogoutConfirm]
            [ text "로그아웃" ]
        , a [ class "button is-light logout_cencel", Route.href Route.Logout ]
            [ text "취소" ]
        ]
    ]
    ]

logoutlayer =
    div [ id "logoutPop"] [
        div [ class "yf_popup" ]
    [ img [ src "/image/icon_logout.png" ]
        []
    , h1 [ class "popup_yf_h1" ]
        [ text "로그아웃 하시겠습니까?" ]
    , p [ class "yf_logoout_butbox" ]
        [ a [ class "button is-danger logout_danger", Route.href Route.LogoutConfirm]
            [ text "로그아웃" ]
        , a [ class "button is-light logout_cencel", Route.href Route.Logout ]
            [ text "취소" ]
        ]
    ]
    ]
  