module Page.Common exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Route as Route
import Task exposing (Task)
import Browser.Dom as Dom
import Json.Decode as Decode
import File exposing(File)
commonHeader : String -> String -> Html msg
commonHeader icon title =
    div [ class "titlewrap" ]
        [ div [ class "columns" ]
            [ div [ class "column is-half pagename" ]
                [ div [ class "yficon" ]
                    [ img [ src icon , alt "icon" ]
                        []
                    ]
                , div [ class "yftext" ]
                    [ strong []
                        [ text title ]
                    ]
                ]
            , div [ class "column is-halfright" ]
                [ 
                ]
            ]
        ]

commonHeader2 : String -> String -> Html msg
commonHeader2 icon title =
    div [ class "titlewrap" ]
        [ div [ class "columns" ]
            [ div [ class "column is-full pagename" ]
                [ div [ class "yficon" ]
                    [ img [ src icon , alt "icon" ]
                        []
                    ]
                , div [ class "yftext" ]
                    [ strong []
                        [ text title ]
                    ]
                ]

            ]
        ]
commonJustHeader : String -> String -> Html msg        
commonJustHeader icon title =
       div [ class "titlewrap" ]
        [ div [ class "columns" ]
            [ div [ class "column is-full pagename" ]
                [ div [ class "yficon" ]
                    [ img [  src icon, alt "icon" ]
                        []
                    ]
                , div [ class "yftext" ]
                    [ strong []
                        [ text title ]
                    ]
                ]
            ]
        ]

myPageCommonHeader scrRight scrLeft another show= 
    div [ class "submenu", id "mypageMenu", style "height"  (if show then "110px" else "0") ]
        [  
            i [ class "fas fa-caret-left scrStyle", onClick scrLeft ]
            [] ,
            div [ class "haderIconWrap", id "scrollCtr", style "height"  (if show then "110px" else "0"), style "zindex" "99999999999999999" ]
            [ 
                
                a [ class "headerIcons", Route.href Route.MyPage]
                [ img [ src "../image/icon_mypage.png" ]
                    []
                , p []
                    [ text "내 정보 설정" ]
                ]
                , a [ class "headerIcons" , Route.href Route.MyC]
                [ img [ src "../image/icon_calendar.png" ]
                    []
                , p []
                    [ text "캘린더" ]
                ]
            , a [ class "headerIcons", Route.href Route.MyScrap]
                [ img [ src "../image/icon_list.png" ]
                    []
                , p []
                    [ text "스크랩리스트" ]
                ]
            , a [ class "headerIcons", Route.href Route.MyS]
                [ img [ src "../image/icon_stats.png" ]
                    []
                , p []
                    [ text "나의 통계" ]
                ]
            , a [ class "headerIcons", Route.href Route.MyPost]
                [ img [ src "../image/icon_management.png" ]
                    []
                , p []
                    [ text "내 게시물관리" ]
                ]
            , a [ class "headerIcons", Route.href Route.Info]
                [ img [ src "../image/icon_notice.png" ]
                    []
                , p []
                    [ text "공지사항" ]
                ]
            , a [ class "headerIcons", Route.href Route.C]
                [ img [ src "../image/icon_qna.png" ]
                    []
                , p []
                    [ text "1:1문의" ]
                ]
            , a [ class "headerIcons", Route.href Route.Faq]
                [ img [ src "../image/icon_stats.png" ]
                    []
                , p []
                    [ text "자주하는 질문" ]
                ]
            ],
            i [ class "fas fa-caret-right scrStyle", onClick scrRight ]
            [] 
        ]

appHeaderSearch title style= 
    div [class "headerSpace"] [
    ul [ class ("commonHeader " ++ style) ]
        [ 
        --     div [ class "m_backbtn" ]
        --     []
        -- , 
        li [ class "m_yf_topboxtitle" ]
            [ text title ]
        , li [ class "m_nextbtn" ]
            [ i [ class "fas fa-search" ]
                []
            ]
        ]
    ]

appHeadermypage title style= 
    div [class "headerSpace"] [
    ul [ class ("commonHeader " ++ style) ]
            [ li [ class "m_backbtn" ]
                []
            , li [ class "m_yf_topboxtitle" ]
                [ text title ]
            , li [ class "m_nextbtn" ]
                [ ]
                ]
    ]
justappHeader title style = 
    div [class "headerSpace"] [
        div [ class ("justHeader " ++ style) ]
        [  text title ]
    ]

appHeaderDetail title style back=
    div [ class ("commonHeader " ++ style) ]
        [ div [ class "m_backbtn", onClick back]
            [ i [ class "fas fa-times" ]
                []
            ]
        , div [ class "m_topboxtitle" ]
            [ text title ]
        ]

appHeaderback title style back=
    div [ class ("commonHeader " ++ style) ]
        [ div [ class "m_backbtn" , onClick back]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "m_topboxback" ]
            [ text title]
        , div [ class "m_nextbtn" ]
            []
        ]


appHeaderBDetail title style back=
    div [ class ("commonHeader " ++ style) ]
        [ div [ class "m_backbtn", onClick back]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "m3_topboxtitle" ]
            [ text title ]

        ]
appHeaderBothBtn title style back icon btnText go=
    div [class "headerSpace"] [
        ul [ class ("commonHeaderBoth " ++ style), id "focusInput" ]
            [ li [][
                a [ class "m_backbtn", Route.href back ]
                [ i [ class icon ]
                    []
                ]
            ]
                
            , li [ class "" ]
                [ text title ]
            , li [class "app_detail_text"] [
                a [ Route.href go ]
                [ text btnText ]
            ]
            ] 
        ]
appHeaderBothfnq title style back icon btnText go=
    div [ class ("commonHeader " ++ style) ]
        [ a [ class "m_backbtn", Route.href back ]
            [ i [ class icon ]
                []
            ]
        , div [ class "m3_topboxtitle" ]
            [ text title ]
        , a [ class "m_qna_w_btn", Route.href go ]
            [ text btnText ]
        ] 


appHeaderRDetail title style back icon=
    div [class "headerSpace"] [
    ul [ class ("commonHeaderBack " ++ style) ]
        [ li [] [
            a [ class "", Route.href back]
            [ i [ class icon ]
                []
            ]
        ]
        , li [ class "toptitle" ]
            [ text title ]
 
        ]
    ]

appHeaderRDetailClick title style back icon=
    div [class "headerSpace"] [
    ul [ class ("commonHeaderBack " ++ style) ]
        [ li [] [
            div [ class "", onClick back]
            [ i [ class icon ]
                []
            ]
        ]
        , li [ class "toptitle" ]
            [ text title ]
 
        ]
    ]

appHeaderRDetailClick2 title style back icon=
    div [class "headerSpace"] [
    ul [ class ("commonHeaderBack " ++ style) ]
        [ li [] [
            div [ class "", onClick back]
            [ i [ class icon ]
                []
            ]
        ]
        , li [ class "toptitle_2" ]
            [ text title ]
 
        ]
    ]

appHeaderinforDetail title style back icon=
    div [ class ("commonHeader " ++ style) ]
        [ a [ class "", Route.href back]
            [ i [ class icon ]
                []
            ]
        , div [ class "" ]
            [ text title ]
 
        ]

appHeaderConfirmDetail title style back btnStyle confirm btnText =
    div [class "headerSpace"] [
    ul [ class ("commonHeaderBoth " ++ style) ,id "focusInput"]
            [ li [  ]
                [   a [Route.href back] [
                    i [ class btnStyle ]
                    []
                ]
                ]
            , li [ class "" ]
                [ text title ]
            , li [ class "app_detail_text" ]
                [ 
                    a [Route.href confirm] [text btnText]
                     ]
            ]
    ]

appHeaderConfirmDetailleft title style back confirm btnText =
    div [class "headerSpace"] [
    ul [ class ("commonHeaderBoth " ++ style) ,id "focusInput"]
            [ li [ class "", onClick back ]
                [ i [ class "fas fa-angle-left" ]
                    []
                ]
            , li [ class "" ]
                [ text title ]
            , li [ class "app_detail_text", onClick confirm ]
                [ text btnText ]
            ]
    ]
    
appHeaderConfirmDetailR title style back confirm btnText =
    div [class "headerSpace"] [
        ul [ class ("commonHeaderBoth " ++ style) ,id "focusInput"]
        [ li [ class "", onClick back ]
            [ i [ class "fas fa-times" ]
                []
            ]
        , li [ class "" ]
            [ text title ]
        , li [ class "app_detail_text", onClick confirm ]
            [ text btnText ]
        ]
    ]

appHeaderConfirmDetailmypage title style back confirm btnText =
        ul [ class ("commonHeaderBoth " ++ style) ,id "focusInput"]
        [ li [ class "", onClick back ]
            [ i [ class "fas fa-times" ]
                []
            ]
        , li [ class "" ]
            [ text title ]
        , li [ class "app_detail_text", onClick confirm ]
            [ text btnText ]
        ]
appHeaderConfirmDetail2 title style back confirm btnText =
    div [ class ("commonHeader " ++ style) ]
        [ div [ class "m_backbtn", onClick back ]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "m3_topboxtitle" ]
            [ text title ]
        , div [ class "m2_nextbtn", Route.href confirm ]
            [ text btnText ]
        ]



appHeaderBackComplete title style back complete = 
    div [ class ("commonHeader " ++ style) ]
        [ div [ class "m_backbtn" , onClick back]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "m3_topboxtitle" ]
            [ text "운동추가" ]
        , div [ class "m_nextbtn", onClick complete ]
            [ text "완료" ]
        ]


spinner = 
    div [ class "loading-spinner" ]
        []
infiniteSpinner = 
   div [ class "sk-fading-circle" ]
        [ div [ class "sk-circle1 sk-circle" ]
            []
        , div [ class "sk-circle2 sk-circle" ]
            []
        , div [ class "sk-circle3 sk-circle" ]
            []
        , div [ class "sk-circle4 sk-circle" ]
            []
        , div [ class "sk-circle5 sk-circle" ]
            []
        , div [ class "sk-circle6 sk-circle" ]
            []
        , div [ class "sk-circle7 sk-circle" ]
            []
        , div [ class "sk-circle8 sk-circle" ]
            []
        , div [ class "sk-circle9 sk-circle" ]
            []
        , div [ class "sk-circle10 sk-circle" ]
            []
        , div [ class "sk-circle11 sk-circle" ]
            []
        , div [ class "sk-circle12 sk-circle" ]
            []
        ]
pagination btn page initNum= 
    let
        index = 
            if (page.total_count // page.per_page) == 0 then
                1
            else
                if (page.total_count // page.per_page) * page.per_page < page.total_count then
                    (page.total_count // page.per_page) + 1
                else
                    (page.total_count // page.per_page)
    in
    
    nav [ class "pagination" ]
        [ div [ class "pagination-previous", title "This is the first page",  onClick (
             if (initNum * 10) > 10 then
                btn (
                    (initNum - 1) * 10
                    , "prev")
            else
                btn ( 0 , "" )
        ) ]
            [ text "10페이지 뒤로" ]
        
        , ul [ class "pagination-list" ]
            (
                List.indexedMap (\idx x ->
                    
                    item idx x page.page btn
                ) (List.range (
                    if initNum  == 1 then
                        1
                    else
                        ((initNum - 1) * 10 ) + 1
                ) (
                    if index > (10 * initNum) then
                        10 * initNum
                    else if index < 10 then 
                        index
                    else
                        ((initNum - 1 ) * 10 ) + ( index - ((initNum - 1) * 10 ))
                )
            )
            )
        , div [ class "pagination-next" , onClick (
            if (initNum * 10) < index then
                btn ((initNum * 10 + 1), "next")
            else
                btn ( 0 , "" )
        )]
            [ text "10페이지 앞으로" ]
        ]


item  idx num current btn=
    a [class (
        if (num) == current then
        "pagination-link is-current"
        else
        "pagination-link"
        ), href "",  onClick (btn ((num), "go"))] [
            div [] [ text (String.fromInt (num)) ]
            -- ]
        ]
    

need2loginAppDetail route = 
    div [class "have2login"] [
                 
                    div [class"have2login_text"] [text "로그인 후 이용가능한 서비스 입니다."] 
                , a [class "button is-link have2login_btn", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
                , div [class "button have2login_btn", onClick route] [text "이전 페이지로 이동"]
                ]

need2loginAppDetailRoute: Route.Route -> Html msg
need2loginAppDetailRoute route = 
    div [class "have2login2"] [
                    
   
                    p[class"have2login_text"] [text "로그인 후 이용가능한 서비스 입니다."] 
                   
                , a [class "button is-link have2login_btn", Route.href Route.Login] [text "로그인 또는 회원가입하기"]
                , a [class "button have2login_btn", Route.href route] [text "이전 페이지로 이동"]
                ]

scrollToTop : msg -> Cmd msg
scrollToTop noop =
    Task.perform (\_ -> noop) (Dom.setViewport 0 0)

-- jumpToBottom : String -> Cmd msg
jumpToBottom id noop =
  Dom.getViewportOf id
    |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
    |> Task.attempt (\_ -> noop)

fileupload getFile thumb =
    div [ class "field is-horizontal" ] [
        div [ class "file has-name is-right is-fullwidth photo_width" ]
            [ label [ class "file-label" ]
                [ input [ class "file-input", type_ "file", multiple False, id "thumbFile", accept "image/x-png,image/gif,image/jpeg"
                , on "change" (Decode.map getFile targetFiles)  ]
                    []
                , span [ class "file-cta" ]
                    [ span [ class "file-icon" ]
                        [ i [ class "fas fa-upload" ]
                            []
                        ]
                    , span [ class "file-label" ]
                        [ text "사진찾기" ]
                    ]
                , span [ class "file-name" ]
                    [ 
                        if thumb == "" then
                        text "프로필 사진을 변경 해 주세요."
                        else
                        text thumb
                        -- text (Debug.toString thumb)
                    ]
                ]
            ]
        ]
fileuploadapp getFile thumb =
    div [ class "field is-horizontal " ] [
        div [ class "file has-name is-right is-fullwidth" ]
            [ label [ class "file-label" ]
                [ input [ class "file-input ", type_ "file", multiple False, id "thumbFile", accept "image/*" 
                , on "change" (Decode.map getFile targetFiles)  ]
                    []
                , span [ class "file-cta" ]
                    [ span [ class "file-icon" ]
                        [ i [ class "fas fa-upload" ]
                            []
                        ]
                    , span [ class "file-label" ]
                        [ text "사진찾기" ]
                    ]
                , span [ class "file-name" ]
                    [ 
                        if thumb == "" then
                        text "프로필 사진을 변경 해 주세요."
                        else
                        text thumb
                        -- text (Debug.toString thumb)
                    ]
                ]
            ]
        ]
-- onChange: (String -> msg) -> Html.Attribute msg
-- onChange tagger = 
--     on "change" (Decode.map tagger targetValue)

-- targetFiles : Decode.Decoder (List File)
targetFiles = 
    Decode.at ["target", "files"] (Decode.list File.decoder)


    