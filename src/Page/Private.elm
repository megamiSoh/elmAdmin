module Page.Private exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route as Route
type alias Model 
    = {
        session : Session
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , check = mobile}
        , Cmd.none
    )

type Msg 
    = NoOp
    | Back

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Back ->
            (model, Route.backUrl (Session.navKey model.session) 1)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.check  then
        div [ class "containerwrap" ]
            [ div [ class "container" ]
                [ 
                        appHeaderRDetailClick2 "이용약관 및 개인정보 취급방침" "myPageHeader" Back "fas fa-angle-left"
                        , termsArticle "m_yf_termstext"
                    
                ]
            ]
        else
        div [ class "containerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonJustHeader "/image/icon_signup.png" "이용약관 및 개인정보 취급방침"
                        , termsArticle "yf_termstext"
                        , goBtn
                    ]
                ]
    ]
    }


termsArticle style = 
    ul [ class style ]
        [ li []
            [ text "제1장 총칙" ]
        , li []
            [ text "제1조 (목적) 이 약관은 파이널 컴퍼니(이하 “회사”라 합니다)가 모바일 기기를 통해 제공하는 게임 서비스 및 이에 부수하는 네트워크, 웹사이트, 기타 서비스(이하 “서비스”라 합니다)의 이용에 대한 회사와 서비스 이용자의 권리ㆍ의무 및 책임사항, 기타 필요한 사항을 규정함을 목적으로 합니다." ]
        , li []
            [ text "제2조 (용어의 정의) ① 이 약관에서 사용하는 용어의 정의는 다음과 같습니다." ]
        , li []
            [ text "1. “회사”라 함은 모바일 기기를 통하여 서비스를 제공하는 사업자를 의미합니다." ]
        , li []
            [ text "2. “회원”이란 이 약관에 따라 이용계약을 체결하고, 회사가 제공하는 서비스를 이용하는 자를 의미합니다." ]
        , li []
            [ text "3. “임시회원”이란 일부 정보만 제공하고 회사가 제공하는 서비스의 일부만 이용하는 자를 의미합니다." ]
        , li []
            [ text "4. “모바일 기기”란 콘텐츠를 다운로드 받거나 설치하여 사용할 수 있는 기기로서, 휴대폰, 스마트폰, 휴대정보단말기(PDA), 태블릿 등을 의미합니다." ]
        , li []
            [ text "5. “계정정보”란 회원의 회원번호와 외부계정정보, 기기정보, 별명, 프로필 사진, 친구목록 등 회원이 회사에 제공한 정보와 게임이용정보 (캐릭터 정보, 아이템, 레벨 등), 이용요금 결제 정보 등을 통칭합니다." ]
        , li []
            [ text "6. “콘텐츠”란 모바일 기기로 이용할 수 있도록 회사가 서비스 제공과 관련하여 디지털 방식으로 제작한 유료 또는 무료의 내용물 일체(게임 및 네트워크 서비스, 애플리케이션, 게임 머니, 게임 아이템 등)를 의미합니다." ]
        , li []
            [ text "7. “오픈마켓”이란 모바일 기기에서 게임 콘텐츠를 설치하고 결제할 수 있도록 구축된 전자상거래 환경을 의미합니다." ]
        , li []
            [ text "8. “애플리케이션”이란 회사가 제공하는 서비스를 이용하기 위하여 모바일 기기를 통해 다운로드 받거나 설치하여 사용하는 프로그램 일체를 의미합니다." ]
        , li []
            [ text "2. “회원”이란 이 약관에 따라 이용계약을 체결하고, 회사가 제공하는 서비스를 이용하는 자를 의미합니다." ]
        , li []
            [ text "제1조 (목적) 이 약관은 파이널 컴퍼니(이하 “회사”라 합니다)가 모바일 기기를 통해 제공하는 게임 서비스 및 이에 부수하는 네트워크, 웹사이트, 기타 서비스(이하 “서비스”라 합니다)의 이용에 대한 회사와 서비스 이용자의 권리ㆍ의무 및 책임사항, 기타 필요한 사항을 규정함을 목적으로 합니다." ]
        , li []
            [ text "제2조 (용어의 정의) ① 이 약관에서 사용하는 용어의 정의는 다음과 같습니다." ]
        , li []
            [ text "1. “회사”라 함은 모바일 기기를 통하여 서비스를 제공하는 사업자를 의미합니다." ]
        , li []
            [ text "2. “회원”이란 이 약관에 따라 이용계약을 체결하고, 회사가 제공하는 서비스를 이용하는 자를 의미합니다." ]
        , li []
            [ text "제1조 (목적) 이 약관은 파이널 컴퍼니(이하 “회사”라 합니다)가 모바일 기기를 통해 제공하는 게임 서비스 및 이에 부수하는 네트워크, 웹사이트, 기타 서비스(이하 “서비스”라 합니다)의 이용에 대한 회사와 서비스 이용자의 권리ㆍ의무 및 책임사항, 기타 필요한 사항을 규정함을 목적으로 합니다." ]
        , li []
            [ text "제2조 (용어의 정의) ① 이 약관에서 사용하는 용어의 정의는 다음과 같습니다." ]
        , li []
            [ text "1. “회사”라 함은 모바일 기기를 통하여 서비스를 제공하는 사업자를 의미합니다." ]
        , li []
            [ text "2. “회원”이란 이 약관에 따라 이용계약을 체결하고, 회사가 제공하는 서비스를 이용하는 자를 의미합니다." ]
        , li []
            [ text "3. “임시회원”이란 일부 정보만 제공하고 회사가 제공하는 서비스의 일부만 이용하는 자를 의미합니다." ]
        , li []
            [ text "4. “모바일 기기”란 콘텐츠를 다운로드 받거나 설치하여 사용할 수 있는 기기로서, 휴대폰, 스마트폰, 휴대정보단말기(PDA), 태블릿 등을 의미합니다." ]
        , li []
            [ text "5. “계정정보”란 회원의 회원번호와 외부계정정보, 기기정보, 별명, 프로필 사진, 친구목록 등 회원이 회사에 제공한 정보와 게임이용정보 (캐릭터 정보, 아이템, 레벨 등), 이용요금 결제 정보 등을 통칭합니다." ]
        , li []
            [ text "6. “콘텐츠”란 모바일 기기로 이용할 수 있도록 회사가 서비스 제공과 관련하여 디지털 방식으로 제작한 유료 또는 무료의 내용물 일체(게임 및 네트워크 서비스, 애플리케이션, 게임 머니, 게임 아이템 등)를 의미합니다." ]
        , li []
            [ text "7. “오픈마켓”이란 모바일 기기에서 게임 콘텐츠를 설치하고 결제할 수 있도록 구축된 전자상거래 환경을 의미합니다." ]
        , li []
            [ text "8. “애플리케이션”이란 회사가 제공하는 서비스를 이용하기 위하여 모바일 기기를 통해 다운로드 받거나 설치하여 사용하는 프로그램 일체를 의미합니다." ]
        , li []
            [ text "2. “회원”이란 이 약관에 따라 이용계약을 체결하고, 회사가 제공하는 서비스를 이용하는 자를 의미합니다." ]
        , li []
            [ text "제1조 (목적) 이 약관은 파이널 컴퍼니(이하 “회사”라 합니다)가 모바일 기기를 통해 제공하는 게임 서비스 및 이에 부수하는 네트워크, 웹사이트, 기타 서비스(이하 “서비스”라 합니다)의 이용에 대한 회사와 서비스 이용자의 권리ㆍ의무 및 책임사항, 기타 필요한 사항을 규정함을 목적으로 합니다." ]
        , li []
            []
        ]

goBtn = 
     div [ class "button is-dark yf_darkbut", onClick Back ]
    [ text "확인" ]