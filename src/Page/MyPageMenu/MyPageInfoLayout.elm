module Page.MyPageMenu.MyPageInfoLayout exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Route exposing (..)

-- myInfo : Html msg
myInfo item changeNick changeBtn wantChangeNickname changeGo pwdInput changePwd notmatchPwd repwdInput oldInput nick=         
    div [ class "columns" ]
    [ div [ class "column mypage_yf_profile" ]
        [ p []
            [ figure [ class "image is-128x128 mypage_yf_image" ]
                [ img [ src "../image/profile.png" ]
                    []
                ]
            ]
        ]
    , div [ class "column mypage_yf_box2" ]
        [ ul []
            [ li []
                [ text "계정정보" ]
            , li []
                [ text item.username ]
            , li []
                [ 
                div [] [text "닉네임"] ,
                 case item.nickname of
                     Just a ->
                        div [] [
                        div [] [text a]
                        , if wantChangeNickname == "nick" then
                            div [] [
                                    div [class "notmatchPwd"] [text notmatchPwd]
                                    , input [ class "input myPage_yf_input", type_ "text", placeholder "닉네임을 정해주세요" , onInput changeNick]
                                    []
                                    , div [ class "button mypage_nickbtn", onClick changeGo ]
                                        [ text "적용" ]
                                    ]
                        else
                         div [ class "button", onClick (changeBtn "nick")] [text "닉네임 변경"]
                        ]
                     Nothing ->
                         div [] [
                             div [class "notmatchPwd"] [text notmatchPwd]
                            , input [ class "input myPage_yf_input", type_ "text", placeholder "닉네임을 정해주세요" , onInput changeNick]
                            []
                            , div [ class "button mypage_nickbtn" , onClick changeGo]
                                [ text "적용" ]
                            ]
                
                , div [ class "button", onClick (changeBtn "pwd")] [text "비밀번호 변경"]
               
                , if wantChangeNickname == "pwd" then
                    div [] [
                            div [class "notmatchPwd"] [text notmatchPwd]
                            , input [ class "input myPage_yf_input", type_ "text", placeholder "기존의  패스워드를 입력 해 주세요." , onInput oldInput]
                            []
                            , input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 입력 해 주세요." , onInput pwdInput]
                            []
                            ,input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 한번 더 입력 해 주세요." , onInput repwdInput]
                            []
                            , div [ class "button mypage_nickbtn", onClick changePwd ]
                                [ text "적용" ]
                            ]
                else
                div [] []
                ]
            -- , p [ class "help is-danger" ]
            --     [ text "이미 사용중인 닉네임입니다" ]
            , li []
                [ p [ class "myPage_yf_terms" ]
                    [ a [Route.href Route.Private ]
                        [ text "개인정보 보호 및 약관확인" ]
                    ]
                ]
            -- , a [ class "button is-dark mypage_photo"] 
            --     [ text "사진변경" ]
            -- , a [ class "button"]
            --     [ text "사진삭제" ]
            ]
        ]
    ]
bodyInfo : Html msg
bodyInfo = 
    div [ class "wrap" ]
        [ div [ class "gender_title" ]
            [ label [ class "label" ]
                [ text "성별" ]
            , p []
                [ label [ class "radio" ]
                    [ input [ type_ "radio", name "question" ]
                        []
                    , i [ class "fas fa-mars myPage2_man" ]
                        [], text "남자" 
                    ]
                , label [ class "radio" ]
                    [ input [ type_ "radio", name "question" ]
                        []
                    , i [ class "fas fa-venus myPage2_woman" ]
                        [], text "여자" 
                    ]
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "체중" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "text" ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "목표체중" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "text" ]
                    [],text "Cm" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "신장" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "text" ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "생년월일" ]
            , p []
                [ input [ class "input myPage2_yf_input_box2", type_ "text" ]
                    [], text "년" 
                , input [ class "input myPage2_yf_input_box2", type_ "text" ]
                    [], text "월" 
                , input [ class "input myPage2_yf_input_box2", type_ "text" ]
                    [], text "일" 
                ]
            ]
        , a [ class "button is-dark yf_is-dark" ]
            [ text "저장" ]
        ]

accountInfo delete= 
    div [ class "ss" ]
        [ div [ class "columns myPage3_yf_cols" ]
            [
                -- div [ class "column yf_colns" ]
                -- [ img [ src "/image/setting_icon1.png" ]
                --     []
                -- , p [ class "myPage3_text" ]
                --     [ text "모든 운동기록을 초기화 합니다." ]
                -- , a [ class "button is-danger myPage3_yf_danger" ]
                --     [ text "초기화" ]
                -- ]
            -- , 
            div [ class "column yf_colns" ]
                [ img [ src "/image/setting_icon2.png" ]
                    []
                , p [ class "myPage3_text" ]
                    [ text "유어핏 서비스를 탈퇴합니다." ]
                , div [ class "button is-danger myPage3_yf_danger", onClick delete ]
                    [ text "회원탈퇴" ]
                ]
            ]
        ]