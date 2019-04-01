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
    , rePwd : String
    , mydata : MyData
    , nickname : String
    , deleteAuth : String
    , currentPage : String
    , wantChangeNickname : String
    , pwd : String
    , notMatchPwd : String
    , oldPwd : String
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
    (
        { session = session
        , selecTab = "myInfo"
        , checkDevice = ""
        , check = mobile
        , nickname = ""
        , oldPwd = ""
        , currentPage = ""
        , pwd = ""
        , deleteAuth = ""
        , notMatchPwd = ""
        , wantChangeNickname = ""
        , rePwd = ""
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

pwdEncode model session = 
    let
        list = 
            E.object
             [ ("password", E.string model.oldPwd)
             , ("new_password", E.string model.pwd)
             , ("new_password_confirmation", E.string model.rePwd)]   
                |> Http.jsonBody
    in
    Api.post Endpoint.pwdChange (Session.cred session) PwdComplete list Decoder.resultD

type Msg 
    = ClickRight
    | ClickLeft
    | SelectTab String
    | GotSession Session
    | MyInfoData (Result Http.Error DataWrap)
    | ChangeNick String
    | ChangeGo
    | SuccessNickname (Result Http.Error Decoder.Success)
    | WantChangeNickname String
    | AccountDelete
    | PwdInput String
    | ChangePwd
    | RePwdInput String
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | OldPwd String
    | PwdComplete (Result Http.Error Decoder.Success)

subscriptions :Model -> Sub Msg
subscriptions model=
    Session.changes GotSession (Session.navKey model.session)

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
        PwdComplete (Ok ok) ->
            let
                textEncode = E.string "변경되었습니다."
            in
            ({model | wantChangeNickname = ""}, Api.showToast textEncode)
        PwdComplete (Err err) ->
            let
                serverError = Api.decodeErrors err
            in
            if serverError == "401" then
            ({model | deleteAuth = "pwd"}, (Session.changeInterCeptor (Just serverError) model.session))
            else 
            ({model | notMatchPwd = "기존에 사용하시던 비밀번호를 다시 한번 확인 해 주세요."}, Cmd.none)
        OldPwd str ->
            ({model | oldPwd = str}, Cmd.none)
        RePwdInput str ->
            ({model |rePwd = str} , Cmd.none)
        PwdInput str ->
            ({model | pwd = str} , Cmd.none)
        ChangePwd -> 
            if model.oldPwd == "" then
            ({model| notMatchPwd = "기존에 사용하시던 패스워드를 입력 해 주세요."}, Cmd.none)
            else if model.rePwd /= model.pwd then
            ({model| notMatchPwd = "비밀번호가 일치하지 않습니다."}, Cmd.none)
            else if String.length model.rePwd < 5 then
            ({model | notMatchPwd = "비밀번호를 6자리 이상 입력 해 주세요."}, Cmd.none)
            else
            ({model | notMatchPwd = ""}, Cmd.batch[ pwdEncode model model.session ])

        DeleteSuccess (Ok ok) ->
            (model,Route.pushUrl (Session.navKey model.session) Route.Logout)
        DeleteSuccess (Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err    
            in
            ({model | deleteAuth = "delete"},(Session.changeInterCeptor (Just serverErrors) model.session))
        AccountDelete ->
            let
                body = E.object [("is_leave", E.bool True)]
                    |> Http.jsonBody
            in
            
            (model, Api.post Endpoint.accountDelete (Session.cred model.session) DeleteSuccess body  Decoder.resultD )
        WantChangeNickname str->
            ({model | wantChangeNickname = str} , Cmd.none)
        SuccessNickname (Ok ok) ->
            let
                textEncode = 
                    E.string "변경되었습니다."    
            in
            
            ({model | currentPage = "", wantChangeNickname = "", notMatchPwd = ""}, Cmd.batch [
                 Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) (Decoder.dataWRap DataWrap MyData UserData)
                 , Api.showToast textEncode
            ])
        SuccessNickname (Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err
            in
            ({model | deleteAuth = "nick"}, (Session.changeInterCeptor (Just serverErrors) model.session))
        ChangeGo ->
            let
                list = 
                    "nickname="
                    ++ model.nickname
                        |> Http.stringBody "application/x-www-form-urlencoded"
            in
            if model.nickname == "" then
            ({model | notMatchPwd = "변경할 닉네임을 입력 해 주세요."}, Cmd.none) 
            else
            ({model | wantChangeNickname = ""}, Cmd.batch 
            [ Api.post Endpoint.changeNick (Session.cred model.session)  SuccessNickname list Decoder.resultD])
        ChangeNick str ->
            ({model | nickname = str},Cmd.none)
        MyInfoData (Ok ok) ->
            ({model | mydata = ok.data}, Cmd.none)
        MyInfoData (Err err) ->
            let
                serverError = Api.decodeErrors err
            in
            
            (model, (Session.changeInterCeptor (Just serverError) model.session))
        ClickRight ->
            ( model, scrollRight () )
        ClickLeft ->
            (model , scrollLeft ())
        SelectTab tab->
            ({model | selecTab = tab} , Cmd.none)
        GotSession session ->
            if model.deleteAuth =="delete" then
            update AccountDelete {model | session = session}
            else if model.deleteAuth == "nick" then
            update ChangeGo {model | session =session}
            else if model.deleteAuth == "pwd" then
            update ChangePwd {model| session = session}
            else
           ({model | session = session}, 
            Api.get MyInfoData Endpoint.myInfo (Session.cred session) (Decoder.dataWRap DataWrap MyData UserData)
           )



view : Model -> {title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content =       
        if model.check then
        div [] [
            justappHeader "마이페이지" "myPageHeader" ,
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
            "닉네임 등록"
web model = 
    div [] [
            myPageCommonHeader ClickRight ClickLeft
            ,
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
                    myInfo model.mydata.user ChangeNick WantChangeNickname model.wantChangeNickname ChangeGo  PwdInput ChangePwd model.notMatchPwd RePwdInput OldPwd model.nickname
                else 
                    if model.selecTab == "bodyInfo" then
                    bodyInfo
                    else
                    accountInfo AccountDelete
            ]
        ]

menu = 
        
            [
            -- {icon = "/image/icon_calendar.png", title ="캘린더", route = Route.MyC},
            -- {icon = "/image/icon_diet.png", title ="식단기록", route = Route.MealRM},
            -- {icon = "/image/icon_stats.png", title ="나의 통계", route = Route.MyS},
            {icon = "/image/icon_list.png", title ="스크랩리스트", route = Route.MyScrap},
            {icon = "/image/icon_management.png", title ="내 게시물 관리", route= MyPost},
            {icon = "/image/icon_notice.png", title ="공지사항", route = Route.Info},
            -- {icon = "/image/icon_qna.png", title ="1:1 문의", route = Route.Faq},
            {icon = "/image/icon_qna.png", title ="로그아웃", route = Route.Logout}
            ]