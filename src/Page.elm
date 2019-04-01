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
    


-- view:Maybe Cred -> Api.Check  -> Page -> {check : String , title : String, content: Html msg} -> Browser.Document msg
view maybeViewer checkB  page { title, content}  =
    if checkB then
        {title = title , body = appContents content page maybeViewer:: [footerCommon page]}
                
    else
        { title = title 
        , body = viewHeader page maybeViewer:: webContents content page maybeViewer:: [viewFooter]
        }



footerCommon page = 
    if page == MakeExer then
        appFooter
    else if page == MyPage then
        appFooter
    else if page == Together then
        appFooter
    else if page == YourfitExer then
        appFooter
    else
        div [] []


need2login = 
    div [ class "have2login" ][ 
        div [] [text "로그인 후 이용가능한 서비스 입니다."] 
        , a [class "button", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
    ]

need2loginApp = 
    div [class "have2login"] [
                    div [  ] [text "로그인 후 이용가능한 서비스 입니다."] 
                , a [class "button", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
                ]




webContents content page maybeViewer=
    case maybeViewer of
        Just _ ->
            div [ class "" ][ 
                content, webtoastPop ]
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
            div [ class "appWrap" ][ content, apptoastPop ]
        Nothing ->
            if page == MakeExer  then
            div [][ 
                appHeaderSearch "맞춤운동" "makeExerHeader"
                , need2loginApp
            ]
            else if page == MyPage then
            div [][ 
                appHeadermypage "마이페이지" "myPageHeader"
                , div [ class "loginHeight"] [
                    div [class "loginbottom"] [text "로그인 후 이용가능한 서비스 입니다."] 
                , a [class "button", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
                ]
                , div [](List.map menuBottom (menu))

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
                    , a [  class "navbar-burger burger" ]
                        [ span []
                            []
                        , span []
                            []
                        , span []
                            []
                        ]
                    ]
                , div [ id "navbarBasicExample", class "navbar-menu yf_menu" ]
                    [ div [ class "navbar-start yf_start" ]
                        [ a [ class "navbar-item yf_item", Route.href Route.Logout ]
                            [ text "로그아웃" ]
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
        Nothing ->
            nav [ class "navbar yf_navbar" ]
                [ div [ class "navbar-brand yf_brand" ]
                    [ a [ class "navbar-item yf_logo", Route.href Route.Home ]
                        []
                    , a [  class "navbar-burger burger" ]
                        [ span []
                            []
                        , span []
                            []
                        , span []
                            []
                        ]
                    ]
                , div [ id "navbarBasicExample", class "navbar-menu yf_menu" ]
                    [ div [ class "navbar-start yf_start" ]
                        [ a [ class "navbar-item yf_item", Route.href Route.Login ]
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

appFooter : Html msg
appFooter = 
    footer [ class "m_yf_navfooter" ]
        [ a [ class "m_appmanu" , Route.href Route.Home ]
            [ img [ src "/image/m.icon.home.png"]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.YourFitExer ]
            [ img [ src "/image/m.icon.workout.png" ]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.MakeExer ]
            [ img [ src "/image/m.icon.customworkout.png" ]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.Together ]
            [ img [ src "/image/m.icon.together.png"]
                []
            ]
        , a [ class "m_appmanu", Route.href Route.MyPage ]
            [ img [ src "/image/m.icon.mypage.png" ]
                []
            ]
        ]
viewFooter : Html msg
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
        -- (UserManage, Route.UserManage) -> 
        --     True
        -- (AdminManage, Route.AdminManage) ->
        --     True
        -- (VideoUnit, Route.VideoUnit) ->
        --     True
        -- (Video, Route.Video) ->
        --     True
        -- (ApiVideo, Route.ApiVideo) ->
        --     True
        -- (FoodCalorie, Route.FoodCalorie)->
        --     True
        -- (UserPost, Route.UserPost)->
        --     True
        -- (Info, Route.Info)->
        --     True
        -- (Faq, Route.Faq)->
        --     True
        -- (UserInfo, Route.UserInfo)->
        --     True
        -- (UserDetail, Route.UserMDetail)->
        --     True
        -- (AdminRegist, Route.AdminRegist)->
        --     True
        -- (AdminDetail, Route.AdminDetail) ->
        --     True
        -- (AdminEdit, Route.AdminEdit)->
        --     True
        -- (UnitVideoEdit, Route.UvideoEdit)->
        --     True
        -- (UnitVideoDetail, Route.UvideoDetail)->
        --     True
        -- (UnitVideoRegist, Route.UvideoRegist)->
        --     True
        -- (VideoRegist, Route.VideoRegist)->
        --     True
        -- (VideoEdit, Route.VideoEdit)->
        --     True
        -- (VideoDetail, Route.VideoDetail)->
        --     True
        -- (ApiVideoRegist, Route.ApiVideoRegist)->    
        --     True
        -- (ApiVideoDetail, Route.ApiDetail)->
        --     True
        -- (ApiVideoEdit, Route.ApiEdit)->
        --     True
        -- (InfoRegist, Route.InfoRegist )->
        --     True
        -- (InfoDetail, Route.InfoDetail)->
        --     True
        -- (InfoEdit, Route.InfoEdit)->
        --     True
        -- (FaqDetail, Route.FaqDetail)->
        --     True
        -- (FaqRegist, Route.FaqRegist)->
        --     True
        -- (FaqEdit,Route.FaqEdit)->
        --     True

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