module Page.MyPageMenu.MyPageInfoLayout exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Route exposing (..)
import Page.Common as Common
import Date exposing (..)
import DatePicker exposing (Msg(..))
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
            [ li [class"mypage_font"]
                [ text "계정정보" ]
            , li [class"mypage_count"]
                [ text item.username ]
            , li []
                [ 
                div [class"mypage_font"] [text "닉네임"] ,
                case item.nickname of
                    Just a ->
                        div [class"mypage_nick_blank"] [text a]
                
                    Nothing ->
                        div [class"mypage_nickname_not"] [text "닉네임이 설정되지 않았습니다."]
                , if wantChangeNickname == "nick" then
                    div [] [
                            div [class "notmatchPwd"] []
                            , input [ class "input myPage_yf_input", type_ "text", placeholder "닉네임을 정해주세요" , onInput changeNick, maxlength 10]
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
bodyInfo model inputTagger bodySave ismale  datePicker firstdate datepickerShow cannotSave= 
    div [ class "wrap", id "datepickerPosition" ]
        [
            p [class "bodyRecordsstyle"] [
                if String.isEmpty cannotSave then
                text "자신의 신체정보를 기록합니다."
                else
                span [class "red"] [text cannotSave]
                ]
            , div [ class "gender_title" ]
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
                [ input [ class "input myPage2_yf_input_box",  value model.weight, onInput (inputTagger "weight") ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "목표체중" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", value  model.goalWeight, onInput (inputTagger "goalWeight") ]
                    [],text "Cm" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "신장" ]
            , p []
                [ input [ class "input myPage2_yf_input_box", value model.height, onInput (inputTagger "height") ]
                    [], text "Kg" 
                ]
            ]
        , div [ class "myPage2_title" ]
            [ label [ class "label" ]
                [ text "생년월일" ]
            ,
            --  label []
            --     [ 
                    -- input [ class "input myPage2_yf_input_box2", type_ "date", value model.birth, onInput (inputTagger "birth") ]
                    -- [] 
                    div [ class "field datepickerOn" ]
                        [ p [ class ("control is-expanded has-icons-right datePickerBorder input datepickerIndividualStyle" )
                        , onClick datepickerShow
                        ]
                            [ div []
                                [text firstdate]
                            , span [ class "icon is-small is-right" ]
                                [ i [ class "fa fa-calendar-alt" ]
                                    []
                                ]
                            ]
                            , datePicker
                        ]
                -- ]
            ]
        
        ,div [ class "button is-dark yf_is-dark", onClick bodySave ]
            [ text "저장" ]
        ]

accountInfo delete deleteConfirm show= 
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
                , div [ class "button is-danger myPage3_yf_danger", onClick deleteConfirm ]
                    [ text "회원탈퇴" ]
                ]
            ],
            removelayer show delete 
        ]

removelayer show delete =
    div [class ("removePop " ++ show)][
        div [ class "yf_remove_popup" ]
        [ img [ src "image/setting_icon2.png" ]
            []
        , div [class "popup_yf_h2" ][ ul []
                [ li []
                    [ text "회원탈퇴를 하시게 되면" ]
                , li []
                    [ text "모든데이터가 사라집니다." ]
                , li []
                    [ text "그래도 진행하시겠습니까?" ]
                ]
            ]
        , p [ class "yf_logout_butbox" ]
            [ div [ class "button is-danger logout_danger" , onClick (delete "go")]
                [ text "회원탈퇴" ]
            , div [ class "button is-light logout_cencel", onClick (delete "cancel") ]
                [ text "취소" ]
            ]
        ]
    ]
mremovelayer show delete =
    div [class ("mremovePop " ++ show)][
        div [ class "yf_remove_popup" ]
        [ img [ src "image/setting_icon2.png" ]
            []
        , div [class "popup_yf_h2" ][ ul []
                [ li []
                    [ text "회원탈퇴를 하시게 되면" ]
                , li []
                    [ text "모든데이터가 사라집니다." ]
                , li []
                    [ text "그래도 진행하시겠습니까?" ]
                ]
            ]
        , p [ class "yf_logout_butbox" ]
            [ div [ class "button is-danger logout_danger" , onClick (delete "go")]
                [ text "회원탈퇴" ]
            , div [ class "button is-light logout_cencel", onClick (delete "cancel") ]
                [ text "취소" ]
            ]
        ]
    ]





termsArticle style = 
    div[class"pre_box"][
    pre [ class style ]
        [
            
                text 
                """
유어핏 개인정보 취급방침

파이널컴퍼니주식회사(이하 '회사'라 함)는 정보통신망 이용촉진 및 정보보호 등에 관한 법률, 개인정보보호법, 통신비밀보호법, 전기통신사업법, 등 정보통신서비스제공자가 준수하여야 할 관련 법령상의 개인정보보호 규정을 준수하며, 관련 법령에 의거한 개인정보취급방침을 정하여 이용자 권익 보호에 최선을 다하고 있습니다.

1. 수집하는 개인정보의 항목 및 수집방법

가. 수집하는 개인정보의 항목
첫째, 회사는 회원가입, 원활한 고객상담, 각종 서비스의 제공을 위해 최초 회원가입 당시 또는 회원으로서 회사의 서비스 이용 시 아래와 같은 최소한의 개인정보를 필수항목으로 수집하고 있습니다.
- 아이디(ID) 또는 메일주소, 비밀번호, 사진(비필수)
둘째, 서비스 이용과정이나 사업처리 과정에서 아래와 같은 정보들이 자동으로 생성되어 수집될 수 있습니다.
- IP Address, 쿠키, 방문 일시, 서비스 이용 기록, 불량 이용 기록

나. 개인정보 수집방법
회사는 다음과 같은 방법으로 개인정보를 수집합니다.
- 서비스의 실행 또는 사용함으로써 자동으로 수집
- 서비스 회원가입, 서비스 이용, 이벤트 응모, 생성정보 수집 틀을 통한 수집, 회원정보수정, 팩스, 전화, 고객센터 문의하기
- 협력회사로부터의 제공
- 서비스 가입이나 사용 중 이용자의 자발적 제공을 통한 수집



2. 개인정보의 수집 및 이용목적

가. 서비스의 제공
이용자의 ID, 메일주소 등 개인정보를 활용하여 이용자에게 서비스를 제공합니다.

나. 서비스 제공에 관한 계약 이행 및 서비스 제공에 따른 요금정산
컨텐츠 제공, 특정 맞춤 서비스 제공, 물품배송 또는 청구서 등 발송, 본인인증, 구매 및 요금 결제, 요금추심

다. 회원관리
회원제 서비스 이용 및 제한적 본인 확인제에 따른 본인확인, 개인식별, 불량회원의 부정 이용방지와 비인가 사용방지, 가입의사 확인, 가입 및 가입횟수 제한, 만14세 미만 아동 개인정보 수집시 법정 대리인 동의여부 확인, 추후 법정 대리인 본인확인, 분쟁 조정을 위한 기록보존, 불만처리 등 민원처리, 고지사항 전달 

라. 신규 서비스 개발 및 마케팅, 광고에의 활용
신규 서비스 개발 및 맞춤 서비스 제공, 통계학적 특성에 따른 서비스 제공 및 광고 게재, 서비스의 유효성 확인, 이벤트 및 광고성 정보 제공 및 참여기회 제공, 접속빈도 파악, 회원의 서비스이용에 대한 통계

마. 고충처리 
이용자의 신원 확인, 사실조사를 위한 연락통지, 처리결과 통보 등의 목적으로 개인정보를 처리 



3. 개인정보의 공유 및 제공

회사는 이용자들의 개인정보를 '2. 개인정보의 수집목적 및 이용목적'에서 고지한 범위 내에서 사용하며, 이용자의 사전 동의 없이는 동 범위를 초과하여 이용하거나 원칙적으로 이용자의 개인정보를 외부에 공개하지 않습니다. 다만, 아래의 경우에는 예외로 합니다.

가. 이용자들이 사전에 동의한 경우

나. 법령의 규정에 의거하거나, 수사 목적으로 법령에 정해진 절차와 방법에 따라 수사기관의 요구가 있는 경우



4. 개인정보의 취급위탁

회사는 현재 개인정보의 취급위탁을 하지 않고 있습니다. 다만, 향후 서비스 향상을 위해서 개인정보를 위탁할 수 있으며, 그러한 경우 이용자에 대하여 위탁처리기관, 업무내용 및 개인정보의 보유 및 이용기간 등을 명확히 공지하는 등 관계법령을 준수합니다.



5. 개인정보의 보유 및 이용기간

이용자의 개인정보는 원칙적으로 개인정보의 수집 및 이용목적이 달성되면 지체 없이 파기합니다. 단, 다음의 정보에 대해서는 아래의 이유로 명시한 기간 동안 보존합니다.

가. 회사 내부 방침에 의한 정보보유 사유
- 부정 이용기록 
보존 이유 : 부정 이용 방지
보존 기간 : 1년
나. 관련법령에 의한 정보보유 사유
상법, 전자상거래 등에서의 소비자보호에 관한 법률 등 관계법령의 규정에 의하여 보존할 필요가 있는 경우 회사는 관계법령에서 정한 일정한 기간 동안 회원정보를 보관합니다. 이 경우 회사는 보관하는 정보를 그 보관의 목적으로만 이용하며 보존기간은 아래와 같습니다.
- 계약 또는 청약철회 등에 관한 기록
보존 이유 : 전자상거래 등에서의 소비자보호에 관한 법률
보존 기간 : 5년
- 대금결제 및 재화 등의 공급에 관한 기록
보존 이유 : 전자상거래 등에서의 소비자보호에 관한 법률
보존 기간 : 5년
- 소비자의 불만 또는 분쟁처리에 관한 기록
보존 이유 : 전자상거래 등에서의 소비자보호에 관한 법률
보존 기간 : 3년
- 본인확인에 관한 기록
보존 이유 : 정보통신망 이용촉진 및 정보보호 등에 관한 법률
보존 기간 : 6개월
- 방문에 관한 기록
보존 이유 : 통신비밀보호법
보존 기간 : 3개월


6. 개인정보 파기절차 및 방법

이용자의 개인정보는 원칙적으로 개인정보의 수집 및 이용목적이 달성되면 지체 없이 파기합니다. 회사의 개인정보 파기절차 및 방법은 다음과 같습니다.
가. 파기절차
- 이용자가 회원가입 등을 위해 입력한 정보는 목적이 달성된 후 별도의 DB로 옮겨져(종이의 경우 별도의 서류함) 내부 방침 및 기타 관련 법령에 의한 정보보호 사유에 따라(보유 및 이용기간 참조)일정 기간 저장된 후 파기됩니다.
- 동 개인정보는 법률에 의한 경우가 아니고서는 보유되는 이외의 다른 목적으로 이용되지 않습니다.

나. 파기방법
- 종이에 출력된 개인정보는 분쇄기로 분쇄하거나 소각을 통하여 파기합니다.
- 전자적 파일 형태로 저장된 개인정보는 기록을 재생할 수 없는 기술적 방법을 사용하여 삭제합니다.



7. 이용자 및 법정대리인의 권리와 그 행사방법

이용자 및 법정 대리인은 언제든지 등록되어 있는 자신 혹은 당해 만 14세 미만 아동의 개인정보를 조회하거나 수정할 수 있으며 회사의 개인정보의 처리에 동의하지 않는 경우 동의를 거부하거나 가입해지를 요청할 수도 있습니다. 다만, 그러한 경우 서비스의 일부 또는 전부 이용이 어려울 수 있습니다.
- 이용자 혹은 만 14세 미만 아동의 개인정보 조회, 수정을 위해서는 서비스내 '회원정보 변경' 을, 가입해지(동의철회)를 위해서는 서비스내 '회원탈퇴'를 클릭하여 직접 열람, 정정 또는 탈퇴가 가능합니다.
- 혹은 개인정보관리책임자에게 서면, 전화 또는 이메일로 연락하시면 지체 없이 조치하겠습니다.
- 이용자가 개인정보의 오류에 대한 정정을 요청하신 경우에는 정정을 완료하기 전까지 당해 개인정보를 이용 또는 제공하지 않습니다. 또한 잘못된 개인정보를 제3 자에게 이미 제공한 경우에는 정정 처리결과를 제3자에게 지체 없이 통지하여 정정이 이루어지도록 하겠습니다.
- 회사는 이용자 혹은 법정 대리인의 요청에 의해 해지 또는 삭제된 개인정보는 '5. 개인정보의 보유 및 이용기간'에 명시된 바에 따라 처리하고 그 외의 용도로 열람 또는 이용할 수 없도록 처리하고 있습니다.



8. 개인정보 자동 수집 장치의 설치/운영 및 거부에 관한 사항

가. 쿠키란?
- 회사는 개인화되고 맞춤화된 서비스를 제공하기 위해서 이용자의 정보를 저장하고 수시로 불러오는 '쿠키(cookie)'를 사용합니다.
- 쿠키는 웹사이트를 운영하는데 이용되는 서버가 이용자의 브라우저에게 보내는 아주 작은 텍스트 파일로 이용자 컴퓨터의 하드디스크에 저장됩니다. 이후 이용자가 웹 사이트에 방문할 경우 웹 사이트 서버는 이용자의 하드 디스크에 저장되어 있는 쿠키의 내용을 읽어 이용자의 환경설정을 유지하고 맞춤화된 서비스를 제공하기 위해 이용됩니다.
- 쿠키는 개인을 식별하는 정보를 자동적/능동적으로 수집하지 않으며, 이용자는 언제든지 이러한 쿠키의 저장을 거부하거나 삭제할 수 있습니다.

나. 회사의 쿠키 사용 목적
이용자들이 방문한 서비스의 이용형태, 보안접속 여부, 이용자 규모 등을 파악하여 이용자에게 광고를 포함한 최적화된 맞춤형 정보를 제공하기 위해 사용합니다.

다. 쿠키의 설치/운영 및 거부
- 이용자는 쿠키 설치에 대한 선택권을 가지고 있습니다. 따라서 이용자는 웹브라우저에서 옵션을 설정함으로써 모든 쿠키를 허용하거나, 쿠키가 저장될 때마다 확인을 거치거나, 아니면 모든 쿠키의 저장을 거부할 수도 있습니다.
- 다만, 쿠키의 저장을 거부할 경우에는 로그인이 필요한 일부 서비스는 이용에 어려움이 있을 수 있습니다.


9. 개인정보의 기술적/관리적 보호 대책
회사는 이용자들의 개인정보를 취급함에 있어 개인정보가 분실, 도난, 누출, 변조 또는 훼손되지 않도록 안전성 확보를 위하여 다음과 같은 기술적/관리적 대책을 강구하고 있습니다.

가. 비밀번호 암호화
    회원의 비밀번호는 암호화되어 저장 및 관리되고 있어 본인만이 알고 있으며, 개인정보의 확인 및 변경도 비밀번호를 알고 있는 본인에 의해서만 가능합니다.
나. 해킹 등에 대비한 대책
    회사는 해킹이나 컴퓨터 바이러스 등에 의해 회원의 개인정보가 유출되거나 훼손되는 것을 막기 위해 최선을 다하고 있습니다. 개인정보의 훼손에 대비해서 자료를 수시로 백업하고 있고, 최신 백신프로그램을 이용하여 이용자들의 개인정보나 자료가 누출되거나 손상되지 않도록 방지하고 있으며, 암호화통신 등을 통하여 네트워크상에서 개인정보를 안전하게 전송할 수 있도록 하고 있습니다. 그리고 침입차단시스템을 이용하여 외부로부터의 무단 접근을 통제하고 있으며, 기타 시스템적으로 보안성을 확보하기 위한 가능한 모든 기술적 장치를 갖추려 노력하고 있습니다.

라. 취급 직원의 최소화 및 교육
    회사의 개인정보관련 취급 직원은 담당자에 한정시키고 있고 이를 위한 별도의 비밀번호를 부여하여 정기적으로 갱신하고 있으며, 담당자에 대한 수시 교육을 통하여 개인정보취급방침의 준수를 항상 강조하고 있습니다.

마. 개인정보보호전담기구의 운영
    사내 개인정보보호전담기구 등을 통하여 개인정보취급방침의 이행사항 및 담당자의 준수여부를 확인하여 문제가 발견될 경우 즉시 수정하고 바로 잡을 수 있도록 노력하고 있습니다. 단, 이용자 본인의 부주의나 회사의 고의 또는 중대한 과실이 아닌 사유로 개인정보가 유출되어 발생한 문제에 대해 회사는 일체의 책임을 지지 않습니다.



10. 개인정보관리책임자 및 담당자의 연락처

귀하께서는 회사의 서비스를 이용하시며 발생하는 모든 개인정보보호 관련 민원을 개인정보관리책임자 혹은 담당부서로 신고하실 수 있습니다. 회사는 이용자들의 신고사항에 대해 신속하게 충분한 답변을 드릴 것입니다.

11. 고지의 의무

개인정보취급방침의 내용 추가, 삭제 및 수정이 있을 시에는 시행일자 최소 7일전부터 회사의 웹사이트 http://www.yfit.co.kr 연결화면을 통해 공고할 것입니다. 

공고일자 : 2019년 4월 24일
시행일자 : 2019년 5월 1일



유어핏 서비스 이용약관

제1조 (목적)

이 약관은 파이널컴퍼니주식회사(이하 '회사'라 함)가 제공하는 유어핏 서비스(이하 “서비스”라 함)의 이용과 관련하여 회사와 회원 간에 서비스의 이용조건 및 절차, 회사와 회원 간의 권리, 의무 및 기타 필요한 사항을 규정함을 목적으로 합니다. 본 약관은 웹 및 앱 등을 이용하는 전자상거래에 대해서도 그 성질에 반하지 않는 한 준용됩니다. 

제2조 (정의)

이 약관에서 사용하는 용어의 정의는 다음과 같습니다.

① '서비스'라 함은 구현되는 단말기(PC, TV, 휴대형단말기 등의 각종 유무선 장치를 포함)와 상관없이 '회사'가 제공하는 모든 재화 또는 용역을 대상으로 회원이 이용할 수 있는 서비스를 의미합니다. 

② '회원'는 회사의 서비스에 접속하여 이 약관에 따라 회사와 이용계약을 체결하고 회사가 제공하는 서비스를 이용하는 고객을 말하며, 회사가 제공하는 서비스를 계속적으로 이용할 수 있는 자를 의미합니다.  

③ '비회원”이라 함은 회원으로 가입하지 않고 회사가 제공하는 서비스를 이용하는 자를 의미합니다.         
④ '아이디(ID)'라 함은 회원의 식별과 서비스 이용을 위하여 회원이 정하고 회사가 승인하는 문자와 숫자의 조합을 의미하며, 회원의 전자메일 계정으로 대체할 수 있습니다. 

⑤ '비밀번호(Password)”라 함은 회원의 동일성 확인과 회원의 권익 및 비밀보호를 위하여 회원 스스로가 설정하여 사이트에 등록한 영문과 숫자 조합을 의미합니다. 

⑥ '게시물'이라 함은 회원이 서비스를 이용함에 있어 서비스상에 게시한 부호ㆍ문자ㆍ음성ㆍ음향ㆍ화상ㆍ동영상 등의 정보 형태의 글, 사진, 동영상 및 각종 파일과 링크 등을 의미합니다.


제3조 (약관의 게시와 개정)


① 회사는 이 약관의 내용과 상호 및 대표자 성명, 영업소 소재지 주소, 전화번호, 전자우편주소, 사업자등록번호 등을 이용자가 쉽게 알 수 있도록 사이트의 초기화면 또는 서비스 내 공지화면 등을 통하여 게시합니다. 다만, 약관의 구체적인 내용은 이용자가 연결화면을 통하여 볼 수 있습니다. 

② 회사는 '약관의 규제에 관한 법률', “전자거래 기본법”, “전자서명법”, '정보통신망 이용촉진 및 정보보호 등에 관한 법률(이하 '정보통신망법')', '소비자기본법', '전자상거래 등에서의 소비자 보호에 관한 법률” 등 관련법을 위배하지 않는 범위에서 이 약관을 개정할 수 있습니다.

③ 회사가 약관을 개정할 경우에는 적용일자 및 개정사유를 명시하여 현행약관과 함께 제1항의 방식에 따라 그 개정약관의 적용일자 30일 전부터 적용일자 전일까지 공지합니다. 다만, 회원에게 불리한 약관의 개정의 경우에는 공지 외에 일정기간 서비스 내 전자우편, 로그인시 동의 창 등의 전자적 수단을 통해 따로 명확히 통지하도록 합니다.

④ 본 조 제3항에 따라 공지된 적용일자 이후에 회원이 회사의 서비스를 계속 이용하는 경우에는 개정된 약관에 동의하는 것으로 봅니다. 개정된 약관에 동의하지 아니하는 회원은 언제든지 자유롭게 서비스 이용계약을 해지할 수 있습니다. 


제4조 (관련법령과의 관계)

본 약관 또는 개별약관에서 정하지 않은 사항은 전기통신사업법, 전자거래기본법, 정보통신망법, 전자상거래 등에서의 소비자 보호에 관한 법률, 개인정보보호법 등 관련 법령의 규정과 일반적인 상관례에 따릅니다. 


제5조 (이용계약의 성립)


① 이용계약은 회원이 되고자 하는 자(이하 '가입신청자')가 회사가 제공하는 서비스 실행 시 약관의 내용에 대하여 동의를 한 다음, 회사가 정한 가입양식에 따라 회원정보(이용자 ID, 비밀번호, 전자우편주소 등)를 기입하여 회원가입신청을 하고 회사가 이러한 신청에 대하여 승낙함으로써 체결됩니다.

② 회사는 가입신청자의 신청에 대하여 서비스 이용을 승낙함을 원칙으로 합니다. 다만, 회사는 다음 각 호에 해당하는 신청에 대하여는 승낙을 하지 않거나 사후에 이용계약을 해지할 수 있습니다.

(1) 가입신청자가 이 약관에 의하여 이전에 자격을 상실한 적이 있는 경우, 단 '회사'의 재가입 승낙을 얻은 경우에는 예외로 함.

(2) 실명이 아니거나 타인의 명의를 이용한 경우

(3) 회사가 실명확인절차를 실시할 경우에 이용자의 실명가입 신청이 사실 아님이 확인된 경우 

(4) 허위의 정보를 기재하거나, 회사가 제시하는 가입양식에 따른 내용을 기재하지 않은 경우

(5) 이미 가입된 회원과 전화번호 또는 전자우편주소가 동일한 경우 

(6) 이용자의 귀책사유로 인하여 승인이 불가능하거나 기타 규정한 제반 사항을 위반하며 신청하는 경우

(7) 부정한 용도 또는 영리를 추구할 목적으로 본 서비스를 이용하고자 하는 경우 

(8) 기타 이 약관에 위배되거나 사회의 안녕질서 혹은 미풍양속을 저해할 수 있는 목적으로 신청한 경우 

(9) 이용자의 귀책사유로 인하여 승인이 불가능하거나 기타 규정한 제반 사항을 위반하여 신청하는 경우 

③ 제1항에 따른 신청에 있어 회사는 회원의 종류에 따라 전문기관을 통한 실명확인 및 본인인증을 요청할 수 있습니다.

④ 회사는 서비스관련설비의 여유가 없거나, 기술상 또는 업무상 문제가 있는 경우에는 승낙을 유보할 수 있습니다.

⑤ 제2항과 제4항에 따라 회원가입신청의 승낙을 하지 아니하거나 유보한 경우, 회사는 원칙적으로 이를 가입신청자에게 알리도록 합니다.

⑥ 회사는 회원에 대해 회사정책에 따라 등급별로 구분하여 이용횟수, 이용항목 등을 세분하여 이용에 차등을 둘 수 있습니다.


제6조 (회원정보의 변경)

① 회원은 서비스 내 연결화면을 통하여 언제든지 본인의 개인정보를 열람하고 수정할 수 있습니다. 다만, 서비스 관리를 위해 필요한 아이디 등은 수정이 불가능합니다.

② 회원은 회원가입신청 시 기재한 사항이 변경되었을 경우 온라인으로 수정을 하거나 전자우편 기타 방법으로 회사에 대하여 그 변경사항을 알려야 합니다.

③ 제2항의 변경사항을 회사에 알리지 않아 발생한 불이익에 대하여 회사는 책임지지 않습니다.



제7조 (개인정보보호 의무)


① 회사는 회원의 개인정보를 보호하기 위하여 '정보통신망법' 및 “개인정보 보호법” 등 관계 법령에서 정하는 바를 준수합니다. 

② 회사는 회원의 개인정보를 보호하기 위하여 개인정보 취급방침을 제정, 서비스 초기화면에 게시합니다. 다만, 개인정보 취급방침의 구체적 내용은 서비스 내 연결화면을 통하여 볼 수 있습니다. 

③ 회사는 개인정보 취급방침에 따라 회원의 개인정보를 최대한 보호하기 위해 노력합니다. 다만, 회사의 공식 사이트 이외의 링크된 사이트에서는 회사의 개인정보보호정책이 적용되지 않습니다.

④ 회사는 다음과 같은 경우에 법이 허용하는 범위 내에서 회원의 개인정보를 제3자에게 제공할 수 있습니다. 

(1) 수사기관이나 기타 정부기관으로부터 정보 제공을 요청받은 경우 

(2) 회원의 법령 또는 약관의 위반을 포함하여 부정행위 확인 등의 정보보호 업무를 위해 필요한 경우 

(3) 기타 법률의 의해 요구되는 경우 




제8조 ('회원'의 '아이디' 및 '비밀번호'의 관리에 대한 의무)


① 회원의 아이디와 비밀번호에 관한 관리책임은 회원에게 있으며, 회원은 자신의 아이디 및 비밀번호를 제3자가 이용하도록 하여서는 안 됩니다.

② 회사는 회원의 아이디가 개인정보 유출 우려가 있거나, 반사회적 또는 미풍양속에 어긋나거나 회사 및 회사의 운영자로 오인할 우려가 있는 경우, 해당 아이디의 이용을 제한할 수 있습니다.

③ 회원은 아이디 및 비밀번호가 도용되거나 제3자가 사용하고 있음을 인지한 경우에는 이를 즉시 회사에 통지하고 회사의 안내에 따라야 합니다.

④ 제3항의 경우에 해당 회원이 회사에 그 사실을 통지하지 않거나, 통지한 경우에도 회사의 안내에 따르지 않아 발생한 불이익에 대하여 회사는 책임지지 않습니다.




제9조 ('회원'에 대한 통지)


① 회사가 회원에 대한 통지를 하는 경우 본 약관에 별도 규정이 없는 한 회원이 지정한 전자우편주소, 서비스 내 전자메모 및 공지 등으로 할 수 있습니다.




제10조 ('회사'의 의무)


① 회사는 관련법과 이 약관이 금지하거나 미풍양속에 반하는 행위를 하지 않으며, 계속적이고 안정적으로 서비스를 제공하기 위하여 최선을 다하여 노력합니다.

② 회사는 회원이 안전하게 서비스를 이용할 수 있도록 개인정보(신용정보 포함)보호를 위해 보안시스템을 갖추어야 하며 개인정보취급방침을 공시하고 준수합니다.

③ 회사는 서비스이용과 관련하여 회원으로부터 제기된 의견이나 불만이 정당하다고 인정할 경우에는 이를 처리하여야 합니다. 회원이 제기한 의견이나 불만사항에 대해서는 게시판을 활용하거나 전자우편 등을 통하여 회원에게 처리과정 및 결과를 전달합니다.




제11조 ('회원'의 의무)



① 회원은 다음 행위를 하여서는 안 됩니다.

(1) 서비스의 신청 또는 변경 시 허위내용의 등록

(2) 타인의 정보도용

(3) 회사가 게시한 정보의 변경

(4) 다른 회원의 개인정보 및 계정정보를 수집하는 행위

(5) 회사의 사전 동의 없이 영리 목적의 광고성 정보를 전송하기 위하여 이용하는 행위

(6) 리버스엔지니어링, 디컴파일, 디스어셈블 및 기타 일체의 가공행위를 통하여 서비스를 복제, 분해 또는 모방 기타 변형하는 행위

(7) 자동 접속 프로그램 등을 사용하는 등 정상적인 용법과 다른 방법으로 서비스를 이용하여 회사의 서버에 부하를 일으켜 회사의 정상적인 서비스를 방해하는 행위 

(8) 본인 아닌 제3자에게 접속권한을 부여하는 행위

(9) 회사와 기타 제3자의 저작권 등 지적재산권에 대한 침해

(10) 회사 및 기타 제3자의 명예를 손상시키거나 업무를 방해하는 행위

(11) 외설 또는 폭력적인 메시지, 화상, 음성, 기타 공서양속에 반하는 정보를 '서비스'에 공개 또는 게시하는 행위

(12) 회사의 동의 없이 영리를 목적으로 서비스를 사용하는 행위

(13) 기타 불법적이거나 부당한 행위

② 회원은 관계법, 본 약관의 규정, 이용안내 및 서비스와 관련하여 공지한 주의사항, 회사가 통지하는 사항 등을 준수하여야 하며, 기타 회사의 업무에 방해되는 행위를 하여서는 안 됩니다.




제12조 (개별 '서비스'에 대한 약관 및 이용조건)


회사는 제공하는 서비스내의 개별 서비스에 대한 별도의 약관 및 이용조건을 둘 수 있으며 개별서비스에서 별도로 적용되는 약관에 대한 동의는 회원이 개별서비스를 최초로 이용할 경우 별도의 동의 절차를 거치게 됩니다. 이 경우 개별 서비스에 대한 이용약관 등이 본 약관에 우선합니다. 




제13조 (서비스 이용시간) 


서비스의 이용은 회사의 업무상 또는 기술상 특별한 지장이 없는 한 연중무휴 1일 24시간을 원칙으로 합니다. 다만 정기 점검 등의 필요로 회사가 정한 날이나 시간은 제외됩니다. 정기점검 시간은 서비스 제공화면에 공지한 바에 따릅니다. 




제14조 ('서비스' 제공의 변경)


① 회사는 이용 감소로 인한 원활한 서비스 제공의 곤란 및 수익성 악화, 기술 진보에 따른 차세대 서비스로의 전환 필요성, 서비스 제공과 관련한 회사 정책의 변경 등 기타 상당한 이유가 있는 경우에 운영상, 기술상의 필요에 따라 제공하고 있는 전부 또는 일부 서비스를 변경 또는 중단할 수 있습니다.

② 회사는 무료로 제공되는 서비스의 일부 또는 전부를 회사의 정책 및 운영의 필요상 수정, 중단, 변경할 수 있으며, 이에 대하여 관련법에 특별한 규정이 없는 한 회원에게 별도의 보상을 하지 않습니다.

③ 서비스의 내용, 이용방법, 이용시간에 대하여 변경 또는 서비스 중단이 있는 경우에는 변경 또는 중단될 서비스의 내용 및 사유와 일자 등은 그 변경 또는 중단 전에 회사 웹사이트 또는 서비스 내 연결화면 등 회원이 충분히 인지할 수 있는 방법으로 30일의 기간을 두고 사전에 공지합니다.




제15조 (정보의 제공 및 광고의 게재)


① 회사는 회원이 서비스 이용 중 필요하다고 인정되는 다양한 정보를 서비스 내 또는 전자우편, SMS, 서비스 알림 메시지, 전자우편 등의 방법으로 회원에게 제공할 수 있습니다. 다만, 회원은 관련법에 따른 거래관련 정보 및 고객문의 등에 대한 답변 등을 제외하고는 언제든지 위 정보제공에 대해서 수신 거절을 할 수 있습니다.

② 회사는 서비스의 운영과 관련하여 서비스 화면, 홈페이지, 전자우편, SMS, 서비스 알림 메세지 등에 광고를 게재할 수 있습니다. 




제16조 ('게시물'의 저작권)


① 회원이 서비스 내에 게시한 게시물의 저작권은 해당 게시물의 저작자에게 귀속됩니다.

② 회사는 회원의 게시물을 이용하고자 하는 경우에는 전화, 팩스, 전자우편 등을 통해 사전에 회원의 동의를 얻어야 합니다.




제17조 ('게시물'의 관리)


① 회원의 게시물이 '정보통신망법' 및 '저작권법'등 관련법에 위반되는 내용을 포함하는 경우, 권리자는 관련법이 정한 절차에 따라 해당 '게시물'의 게시중단 및 삭제 등을 요청할 수 있으며, '회사'는 관련법에 따라 조치를 취하여야 합니다.

② 회사는 전항에 따른 권리자의 요청이 없는 경우라도 권리침해가 인정될 만한 사유가 있거나 기타 회사 정책 및 관련법에 위반되는 경우에는 관련법에 따라 해당 게시물에 대해 임시조치 등을 취할 수 있습니다.

③ 본 조에 따른 세부절차는 '정보통신망법' 및 '저작권법'이 규정한 범위 내에서 회사가 정한 절차에 따릅니다.




제18조 (권리의 귀속)


① 서비스에 대한 저작권 및 지적재산권은 회사에 귀속됩니다. 단, 회원의 게시물 및 제휴계약에 따라 제공된 저작물 등은 제외합니다.

② 회사가 제공하는 서비스의 디자인, 회사가 만든 텍스트, 스크립트(script), 그래픽, 회원 상호간 전송 기능 등 회사가 제공하는 서비스에 관련된 모든 상표, 서비스 마크, 로고 등에 관한 저작권 기타 지적재산권은 대한민국 및 외국의 법령에 기하여 회사가 보유하고 있거나 회사에게 소유권 또는 사용권이 있습니다.

③ 회원은 본 이용약관으로 인하여 서비스를 소유하거나 서비스에 관한 저작권을 보유하게 되는 것이 아니라, 회사로부터 서비스의 이용을 허락 받게 되는바, 서비스는 정보취득 또는 개인용도로만 제공되는 형태로 회원이 이용할 수 있습니다.

④ 회원은 명시적으로 허락된 내용을 제외하고는 서비스를 통해 얻어지는 회원 상태정보를 영리 목적으로 사용, 복사, 유통하는 것을 포함하여 회사가 만든 텍스트, 스크립트, 그래픽의 회원 상호간 전송기능 등을 복사하거나 유통할 수 없습니다.

⑤ 회사는 서비스와 관련하여 회원에게 회사가 정한 이용조건에 따라 계정, 아이디, 콘텐츠 등을 이용할 수 있는 이용권만을 부여하며, 회원은 이를 양도, 판매, 담보제공 등의 처분행위를 할 수 없습니다.




제19조 (이용계약의 종료)


① 회원은 언제든지 서비스 내 연결화면을 통하여 이용계약 해지 신청을 할 수 있으며, 회사는 관련법 등이 정하는 바에 따라 이를 즉시 처리하여야 합니다.

② 회원이 계약을 해지할 경우, 관련법 및 개인정보취급방침에 따라 회사가 회원정보를 보유하는 경우를 제외하고는 해지 즉시 회원의 모든 데이터는 소멸됩니다.

③ 이용계약의 종료와 관련하여 발생한 손해는 이용계약이 종료된 해당 회원이 책임을 부담하여야 하고 회사는 일체의 책임을 지지 않습니다. 




제20조 (이용제한 등)


① 회사는 회원이 이 약관의 의무를 위반하거나 서비스의 정상적인 운영을 방해한 경우, 경고, 일시정지, 영구이용정지 등으로 서비스 이용을 단계적으로 제한할 수 있습니다.

② 회사는 전항에도 불구하고,'저작권법' 및 '컴퓨터프로그램보호법'을 위반한 불법프로그램의 제공 및 운영방해, '정보통신망법'을 위반한 불법통신 및 해킹, 악성프로그램의 배포, 접속권한 초과행위 등과 같이 관련법을 위반한 경우에는 즉시 영구이용정지를 할 수 있습니다. 본 항에 따른 영구이용정지시 서비스 이용을 통해 기타 혜택 등도 모두 소멸되며, 회사는 이에 대해 별도로 보상하지 않습니다.

③ 회사는 회원이 계속해서 3개월 이상 로그인하지 않는 경우, 회원정보의 보호 및 운영의 효율성을 위해 이용을 제한할 수도 있습니다.

④ 본 조의 이용제한 범위 내에서 제한의 조건 및 세부내용은 회사의 이용제한정책에서 정하는 바에 의합니다.

⑤ 본 조에 따라 서비스 이용을 제한하거나 계약을 해지하는 경우에는 회사는 제9조['회원'에 대한 통지]에 따라 통지합니다.

⑥ 회원은 본 조에 따른 이용제한 등에 대해 회사가 정한 절차에 따라 이의신청을 할 수 있습니다. 이 때 이의가 정당하다고 회사가 인정하는 경우 회사는 즉시 서비스의 이용을 재개합니다.




제21조 (책임제한)


① 회사는 천재지변 또는 이에 준하는 불가항력으로 인하여 서비스를 제공할 수 없는 경우에는 서비스 제공에 관한 책임이 면제됩니다.

② 회사는 회원의 귀책사유로 인한 서비스 이용의 장애에 대하여는 책임을 지지 않습니다.

③ 회사는 회원이 서비스와 관련하여 게재한 정보, 자료, 사실의 신뢰도, 정확성 등의 내용에 관하여는 책임을 지지 않습니다.

④ 회사는 회원 간 또는 회원과 제3자 상호간에 서비스를 매개로 하여 거래 등을 한 경우에는 책임이 면제됩니다.

⑤ 회사는 무료로 제공되는 서비스 이용과 관련하여 관련법에 특별한 규정이 없는 한 책임을 지지 않습니다.

⑥ 회사는 제3자가 서비스 내 화면 또는 링크된 웹사이트를 통하여 광고한 제품 또는 서비스의 내용과 품질에 대하여 감시할 의무 기타 어떠한 책임도 지지 아니합니다.

⑦ 회사 및 회사의 임직원 그리고 대리인은 다음과 같은 사항으로부터 발생하는 손해에 대해 책임을 지지 아니합니다.

(1) 회원 상태정보의 허위 또는 부정확성에 기인하는 손해

(2) 그 성질과 경위를 불문하고 서비스에 대한 접속 및 서비스의 이용과정에서 발생하는 개인적인 손해

(3) 서버에 대한 제3자의 모든 불법적인 접속 또는 서버의 불법적인 이용으로부터 발생하는 손해

(4) 서버에 대한 전송 또는 서버로부터의 전송에 대한 제3자의 모든 불법적인 방해 또는 중단행위로부터 발생하는 손해

(5) 제3자가 서비스를 이용하여 불법적으로 전송, 유포하거나 또는 전송, 유포되도록 한 모든 바이러스, 스파이웨어 및 기타 악성 프로그램으로 인한 손해

(6) 전송된 데이터의 오류 및 생략, 누락, 파괴 등으로 발생되는 손해

(7) 회원 간의 회원 상태정보 등록 및 서비스 이용 과정에서 발생하는 명예훼손 기타 불법행위로 인한 각종 민형사상 책임




제22조 (해외이용)


회사는 대한민국 내에 설치된 서버를 기반으로 서비스를 제공, 관리하고 있습니다. 따라서 회사는 회원이 대한민국의 영토 이외의 지역에서 서비스를 이용하고자 하는 경우 서비스의 품질 또는 사용성을 보장하지 않습니다. 따라서 회원은 대한민국의 영토 이외의 지역에서 서비스를 이용하고자 하는 경우 스스로의 판단과 책임에 따라서 이용 여부를 결정하여야 하고, 특히 서비스의 이용과정에서 현지 법령을 준수할 책임은 회원에게 있습니다. 




제23조 (준거법 및 재판관할)


① 회사와 회원 간 제기된 소송은 대한민국 법을 준거법으로 합니다.

② 회사와 회원 간 발생한 분쟁에 관한 소송은 민사소송 법 상의 법원을 관할법원으로 합니다.


[부칙 1]
이 약관은 2019년 4월 24일부터 적용됩니다.
이 약관은 2019년 5월 1일자로 시행합니다.
                





                """
            
        ]
    ]

goBtn back = 
     div [ class "button is-dark yf_darkbut", onClick (back "signup") ]
    [ text "확인" ]

datepicker model startdatemsg= 
    if model.dateShow then
    div [class "datepickerPosition"] 
        [ div
            []
            [ DatePicker.view
                model.datePickerData
                getDatePickerProps
                |> Html.map startdatemsg
            ]
         ]
    else
    span [] []


getFormattedDate model whenday= 
    let
        year x = Date.year x
        month x= Date.month x
        day x= Date.day x
        encode d = 
            Date.fromCalendarDate (year d) (month d) (day d)
            |> Date.toIsoString
    in  
    case model of  
        Just d ->  
            encode d
        Nothing ->
            case whenday of
                Just d ->  
                    encode d
                Nothing ->
                    "날짜 로드 실패"


stringToDate model whenday birth= 
    let
        year x = Date.year x
        month x= Date.month x
        day x= Date.day x
        encode d = 
            Date.fromCalendarDate (year d) (month d) (day d)
            |> Date.toIsoString
    in  
    case model of  
        Just d ->  
            encode d
        Nothing ->
            if String.isEmpty birth then
            -- case  of
            --     Just d ->  
                    "생년월일을 선택 해 주세요."
                -- Nothing ->
            else
            birth
                    

calendarDate date =
    let
        year x = Date.year x
        month x= Date.month x
        day x= Date.day x
    in
    Date.fromCalendarDate (year date) (month date) (day date)

getDatePickerProps : DatePicker.Props
getDatePickerProps =
    let
        defaultProps =
            DatePicker.defaultProps
    in
        {defaultProps
            | okButtonText = "확인",
            cancelButtonText = "취소"}
type Day
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun

