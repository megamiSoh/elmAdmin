port module Page.MyPage exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Page.MyPageMenu.MyPageInfoLayout exposing(..)
import Route exposing (..)
import Json.Encode as E
import Json.Decode as Decode
import Api as Api
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Http as Http
import File as Files
import Task

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
    , show : String
    , is_male : Bool
    , weight : String
    , accountDeleteGo : String
    , goalWeight : String
    , height : String
    , birth : String
    , profileImg : List Files.File
    , cannotChange : String
    , profileFileName : Maybe String
    }

type alias FileData = 
    {data : File}

type alias File =
    { content_lenth : Int
    , content_type : String
    , extension : String
    , name : String
    , origin_name : String
    , path : String}

type alias DataWrap = 
    { data : MyData }

type alias MyData =
    { exercise : Int
    , share : Int
    , user : UserData }

type alias UserData =
    { id : Int
    , nickname : Maybe String
    , username : String
    , profile : Maybe String}

type alias BodyData = 
    {data : BodyInfoData }

type alias BodyInfoData =
    { birthday :String
    , body_no : Int
    , goal_weight :Int
    , height : Int
    , is_male : Bool
    , weight : Int 
    }

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
        , show = ""
        , cannotChange = ""
        , notMatchPwd = ""
        , accountDeleteGo = ""
        , is_male = True
        , wantChangeNickname = ""
        , rePwd = ""
        , weight = ""
        , profileImg = []
        , goalWeight = ""
        , height = ""
        , birth = ""
        , profileFileName = Nothing
        , mydata = 
            { exercise = 0
            , share = 0
            , user = 
                { id = 0
                , nickname = Nothing
                , username = ""
                , profile = Nothing}
             }}
        , Cmd.batch
        [  Decoder.dataWRap DataWrap MyData UserData
            |> Api.get MyInfoData Endpoint.myInfo (Session.cred session) 
        
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
    Decoder.resultD
    |> Api.post Endpoint.pwdChange (Session.cred session) PwdComplete list 

profileEncode profileImg session =
    let
        body = (List.map (Http.filePart "profile")profileImg)
            |> Http.multipartBody 
    in
    (Decoder.profileData FileData File)
    |>Api.post Endpoint.profileImg (Session.cred session) ChangeComplete body 

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
    | AccountDelete String
    | PwdInput String
    | ChangePwd
    | RePwdInput String
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | OldPwd String
    | PwdComplete (Result Http.Error Decoder.Success)
    | BodyRecordsInput String String
    | BodyInfoComplete (Result Http.Error BodyData)
    | BodySave
    | IsMale  Bool
    | SaveComplete (Result Http.Error Decoder.Success)
    | GoAnotherPage
    | SetPage E.Value
    | ChangeProfileImage (List Files.File)
    | GoProfileImage
    | ChangeComplete (Result Http.Error FileData)
    | ResetProfileImg
    | ResetComplete (Result Http.Error Decoder.Success)
    | AccountDeleteConfirm 

subscriptions :Model -> Sub Msg
subscriptions model=
    Sub.batch[
    Session.changes GotSession (Session.navKey model.session)
    , Api.setCookieSuccess SetPage ]


toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check
justInt int = 
    case String.toInt int of
        Just num ->
            num
    
        Nothing ->
            0
bodyInfoEncode model = 
    let
        body = 
            E.object 
                [ ("weight", E.int (justInt (model.weight)))
                , ("goal_weight", E.int (justInt (model.goalWeight)))
                , ("height", E.int (justInt (model.height)))
                , ("birthday", E.string model.birth)   
                , ("is_male", E.bool model.is_male) ]
                    |> Http.jsonBody
    in
    Decoder.resultD
    |>Api.post Endpoint.bodyRecord (Session.cred model.session) SaveComplete body 
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccountDeleteConfirm ->
            ({model | show = "logoutShow"}, Cmd.none)
        ResetComplete (Ok ok) ->
            (model, Cmd.batch [
                Api.showToast (E.string "기본이미지로 변경되었습니다.")
                ,(Decoder.dataWRap DataWrap MyData UserData)
                |> Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) 
            ])
        ResetComplete (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Api.showToast (E.string "기본 이미지를 변경할 수 없습니다."))
        ResetProfileImg ->
            (model,
            (Decoder.resultD)
             |> Api.get ResetComplete Endpoint.resetprofileImg (Session.cred model.session))
        ChangeComplete (Ok ok) ->
            (model, Cmd.batch [
                Api.showToast (E.string "이미지가 변경되었습니다.")
                , 
                (Decoder.dataWRap DataWrap MyData UserData)
                |> Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) 
            ])
        ChangeComplete (Err err) ->
            let
                    serverErrors =
                        Api.decodeErrors err
            in  
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Api.showToast (E.string "이미지를 변경할 수 없습니다."))
        GoProfileImage ->
            if model.profileFileName == Nothing then
            ({model | cannotChange = "프로필 사진을 선택 해 주세요."}, Cmd.none)
            else 
            (model, profileEncode model.profileImg model.session)
            -- (model, Cmd.none)
        ChangeProfileImage file->
            let
                title =
                    List.head (file)
            in
            case title of
                Just a ->
                   
                    ({model | profileImg = file, profileFileName = Just (Files.name a)}, Cmd.none
                    -- Task.perform SaveProfileImage ()
                    )
            
                Nothing ->
                    (model, Cmd.none)
        SetPage str ->
            let
                strDecode = Decode.decodeValue Decode.string str
            in
            
            case strDecode of
                Ok ok ->
                    (model, 
                    Route.pushUrl (Session.navKey model.session) Route.Info
                    -- Api.historyUpdate (E.string "info")
                    )        
            
                Err err ->
                    (model, Cmd.none)
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (E.int 1)
            ])
        SaveComplete (Ok ok)->
            let
                encodeText = 
                    E.string "저장되었습니다."    
            in
            (model, Api.showToast encodeText)
        SaveComplete (Err err)->
            (model, Cmd.none)
        IsMale str ->
            ({model | is_male = str}, Cmd.none)
        BodySave ->
            (model, bodyInfoEncode model)
        BodyInfoComplete (Ok ok)->
            ({model | weight = String.fromInt (ok.data.weight), goalWeight = String.fromInt (ok.data.goal_weight), height = String.fromInt(ok.data.height), is_male = ok.data.is_male, birth = ok.data.birthday}, Cmd.none)
        BodyInfoComplete (Err err)->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
        BodyRecordsInput category str ->
                  ( case category of
                        "weight" ->
                            {model | weight = str}
                        "goalWeight" ->
                            {model | goalWeight = str}
                        "height" ->
                            {model | height = str}
                        "birth" ->
                            {model | birth = str}
                        _ ->
                            model
                            
                    , Cmd.none)
            
                -- Nothing ->
                --     {model | birth = str, weight = str}
                -- , Cmd.none)
                    
            
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
            ({model| show = ""},
            Cmd.batch [
                Route.replaceUrl (Session.navKey model.session) Route.LogoutConfirm
                , Api.showToast (E.string "탈퇴되었습니다.")
            ])
        DeleteSuccess (Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err    
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            ({model | deleteAuth = "delete"},
            Cmd.batch [
                (Session.changeInterCeptor (Just serverErrors) model.session)
                , Api.showToast (E.string "탈퇴를 진행 할 수 없습니다.")
            ])
        AccountDelete how->
            let
                body = E.object [("is_leave", E.bool True)]
                    |> Http.jsonBody
            in
            if how == "go" then
            ({model | accountDeleteGo = how}, 
            Decoder.resultD
           |> Api.post Endpoint.accountDelete (Session.cred model.session) DeleteSuccess body   )
           else
            ({model | show = ""}, Cmd.none)
        WantChangeNickname str->
            ({model | wantChangeNickname = str} , Cmd.none)
        SuccessNickname (Ok ok) ->
            let
                textEncode = 
                    E.string "변경되었습니다."    
            in
            
            ({model | currentPage = "", wantChangeNickname = "", notMatchPwd = ""}, Cmd.batch [
                (Decoder.dataWRap DataWrap MyData UserData)
                |> Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) 
                 , Api.showToast textEncode
            ])
        SuccessNickname (Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | deleteAuth = "nick"}, (Session.changeInterCeptor (Just serverErrors) model.session))
            else 
            (model, Cmd.none)
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
            [ 
                Decoder.resultD
                |>Api.post Endpoint.changeNick (Session.cred model.session)  SuccessNickname list ])
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
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        SelectTab tab->
            if tab == "bodyInfo" then
            ({model | selecTab = tab}, 
            (Decoder.bodyInfo BodyData BodyInfoData)
            |> Api.get BodyInfoComplete Endpoint.getBodyInfo( Session.cred model.session) 
            )
            else
            ({model | selecTab = tab} , Cmd.none)
        GotSession session ->
            if model.deleteAuth =="delete" then
            update (AccountDelete model.accountDeleteGo) {model | session = session}
            else if model.deleteAuth == "nick" then
            update ChangeGo {model | session =session}
            else if model.deleteAuth == "pwd" then
            update ChangePwd {model| session = session}
            else
           ({model | session = session}, 
             (Decoder.dataWRap DataWrap MyData UserData)
            |> Api.get MyInfoData Endpoint.myInfo (Session.cred session)
           )



view : Model -> {title : String , content : Html Msg}
view model =
    case model.check of
        True ->
            { title = "마이페이지"
            , content =       
                div [] [
                        justappHeader "마이페이지" "myPageHeader" ,
                        app model
                    ]
                
            }
    
        False ->
            case model.selecTab of
                "myInfo" ->
                    { title = "마이페이지"
                    , content =       
                        div [] [
                            web model
                            , div [class "container"][
                            div [ class "myPage_mediabox" ]
                            [ myInfo model.mydata.user ChangeNick WantChangeNickname model.wantChangeNickname ChangeGo  PwdInput ChangePwd model.notMatchPwd RePwdInput OldPwd model.nickname ChangeProfileImage 
                             (case model.profileFileName  of
                                Just a ->
                                    a
                            
                                Nothing ->
                                    "")
                            model.cannotChange GoProfileImage
                            ResetProfileImg
                                        ]
                        ]
                        ]
                                        
                        }
                    
                "bodyInfo" ->
                    { title = ""
                    , content = div [] [web model 
                    , div [class "container"][
                    div [class "myPage_mediabox"] [
                        bodyInfo model BodyRecordsInput BodySave IsMale
                    ]]
                    ]
                    } 

                "accountInfo" ->
                    { title = ""
                    , content = div [] [web model , 
                    div [class "container"]
                    [ div [class "myPage_mediabox"] [
                        accountInfo AccountDelete AccountDeleteConfirm model.show
                    ] 
                    ]
                    ]
                    }
                    

                _ ->
                    { title = "마이페이지"
                    , content =       
                        div [] [
                            web model
                            ,div [] [text "컨텐츠를 불러올 수 없습니다."]
                        ]
                        
                    }
            
                    
    
app model = 
     div [class "container"] [
         appTopMenu model,
         div [class "myMenuStyle"](List.map menuBottom (menu))
     ]
appTopMenu model = 
    div [ class "m_mypage_loginbox" ]
        [ div [ class "m_mypage_profilebox" ]
            [ case model.mydata.user.profile of
                Just image ->
                    img [src image] []
            
                Nothing ->
                    img [src "../image/profile.png"] []
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
web model= 
    div [] [
            myPageCommonHeader ClickRight ClickLeft GoAnotherPage
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
-- ,
--             div [ class "myPage_mediabox" ]
--             [
--                     case model.selecTab of
--                         "myInfo" ->
--                             myInfo model.mydata.user ChangeNick WantChangeNickname model.wantChangeNickname ChangeGo  PwdInput ChangePwd model.notMatchPwd RePwdInput OldPwd model.nickname ChangeProfileImage 
--                             (case model.profileFileName  of
--                                 Just a ->
--                                     a
                            
--                                 Nothing ->
--                                     "")
--                             model.cannotChange GoProfileImage
--                             ResetProfileImg
                    
--                         "bodyInfo" ->
--                             bodyInfo model BodyRecordsInput BodySave IsMale 

--                         "accountInfo" ->
--                             accountInfo AccountDelete AccountDeleteConfirm model.show

--                         _ ->
--                             div [] [text "컨텐츠를 불러올 수 없습니다."]
--             ]
tabPanner model= 
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
                    , li [
                         classList [
                            ("myPage_yf_active", model.selecTab == "bodyInfo")
                        ] , onClick (SelectTab "bodyInfo")
                    ]
                        [ p []
                            [ span []
                                [ text "신체기록관리" ]
                            ]
                        ]
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