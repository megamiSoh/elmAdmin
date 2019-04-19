module Page.MyPageMenu.MyPageInfoLayout exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Route exposing (..)
import Page.Common as Common
-- myInfo : Html msg
myInfo item changeNick changeBtn wantChangeNickname changeGo pwdInput changePwd notmatchPwd repwdInput oldInput nick getFile profileImg cannotChangeImg goprofilechange resetimg=         
    div [ class "columns" ]
    [ div [ class "column mypage_yf_profile" ]
        [ p []
            [ figure [ class "image is-128x128 mypage_yf_image" ]
                [ 
                    case item.profile of
                        Just a->
                            img [ src a ]
                            []
                    
                        Nothing ->
                            img [ src "../image/profile.png" ]
                            []
                ]
            , Common.fileupload getFile profileImg
            , div [class "button is-info photo_change", onClick goprofilechange] [text "프로필 사진 변경 하기"]
            , div [onClick resetimg, class "button"][text "기본이미지로 변경"]
            , span [class"photo_warningtext"] [text cannotChangeImg]
            ]
        ]
    , div [ class "column mypage_yf_box2" ]
        [ ul []
            [ li []
                [ text "계정정보" ]
            , li [class"mypage_count"]
                [ text item.username ]
            , li []
                [ 
                div [] [text "닉네임"] ,
                case item.nickname of
                    Just a ->
                        div [class"mypage_nick_blank"] [text a]
                
                    Nothing ->
                        div [class"mypage_nickname_not"] [text "닉네임이 설정되지 않았습니다."]
                , if wantChangeNickname == "nick" then
                    div [] [
                            div [class "notmatchPwd"] []
                            , input [ class "input myPage_yf_input", type_ "text", placeholder "닉네임을 정해주세요" , onInput changeNick]
                            []
                            , div [ class "button is-info mypage_nickbtn", onClick changeGo ]
                                [ text "적용" ]
                            ]
                else
                    div [ class "button is-link mypage_nickname_btn", onClick (changeBtn "nick")] [text "닉네임 설정"]
                , a [ class "button", Route.href Route.SetPwd] [text "비밀번호 변경"]
                ]
            , li []
                [ p [ class "myPage_yf_terms" ]
                    [ a [Route.href Route.Private ]
                        [ text "개인정보 보호 및 약관확인" ]
                    ]
                ]
            ]
        ]
    ]
-- bodyInfo : Html msg
bodyInfo model inputTagger bodySave ismale= 
    div [ class "wrap" ]
        [ div [ class "gender_title" ]
            [ label [ class "label" ]
                [ text "성별" ]
            , p []
                [ label [ class ("radio genderRadio " ++
                if model.is_male then "activeGender" else ""
                ) ]
                    [ input [ type_ "radio", name "question", onClick (ismale True) ]
                        []
                    , i [ class "fas fa-mars myPage2_man" ]
                        []
                    ]
                , label [ class ("radio genderRadio " ++
                if model.is_male then "" else "activeGender"
                )  ]
                    [ input [ type_ "radio", name "question", onClick (ismale False) ]
                        []
                    , i [ class "fas fa-venus myPage2_woman" ]
                        []
                    ]
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "체중" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "number", value (if model.weight == 0 then "" else String.fromInt model.weight), onInput (inputTagger "weight") ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "목표체중" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "number", value (if model.goalWeight == 0 then "" else String.fromInt model.goalWeight), onInput (inputTagger "goalWeight") ]
                    [],text "Cm" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "신장" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", type_ "number", value (if model.height == 0 then "" else String.fromInt model.height), onInput (inputTagger "height") ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "생년월일" ]
            , p []
                [ input [ class "input myPage2_yf_input_box2", type_ "date", value model.birth, onInput (inputTagger "birth") ]
                    [] 
                ]
            ]
        ,div [ class "button is-dark yf_is-dark", onClick bodySave ]
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