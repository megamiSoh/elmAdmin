module Page.Detail.MyAccount exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route exposing (..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Page.MyPage as My
import Json.Decode as Decode
import Json.Encode as Encode
import File as Files
import Task

type alias Model 
    = {
        session : Session
        , check : Bool
        , mydata : MyData
        , currentPage : String
        , nickname : String
        , sex : String
        , weight : Int
        , goal_weight : Int
        , height : Int
        , birthday : String
        , is_male: Bool
        , notmatchPwd : String
        , oldpwd : String
        , newpwd : String
        , pwd : String
        , deleteAuth : String
        , showbottomtoast : String  
        , profileImg : List Files.File
        , cannotChange : String
        , profileFileName : Maybe String
        , preview : List String
        }

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
type alias DataWrap = 
    { data : MyData }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
type alias MyData =
    { exercise : Int
    , share : Int
    , user : UserData }

type alias UserData =
    { id : Int
    , nickname : Maybe String
    , username : String
    , profile: Maybe String}

type alias FileData = 
    {data : File}

type alias File =
    { content_lenth : Int
    , content_type : String
    , extension : String
    , name : String
    , origin_name : String
    , path : String}


init session mobile
    = (
        { session = session
        , check = mobile
        , currentPage = ""
        , nickname = ""
        , sex = ""
        , deleteAuth = ""
        , cannotChange = ""
        , notmatchPwd = ""
        , oldpwd = ""
        , newpwd = ""
        , profileFileName = Nothing
        , pwd = ""
        , profileImg = []
        , weight = 0
        , goal_weight = 0
        , height = 0
        , preview = []
        , birthday = ""
        , is_male = False
        , showbottomtoast = ""
        , mydata = 
            { exercise = 0
            , share = 0
            , user = 
                { id = 0
                , nickname = Nothing
                , username = ""
                , profile = Nothing}
             }}
        ,Cmd.batch [
            Decoder.dataWRap My.DataWrap MyData UserData
             |> Api.get MyInfoData Endpoint.myInfo (Session.cred session) 
        ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)

type Msg 
    = BackBtn
    | GotSession Session
    | MyInfoData (Result Http.Error My.DataWrap)
    | ChangePage String
    | ChangeNick String
    | SuccessNickname (Result Http.Error Decoder.Success)
    | ChangeGo
    | KeyDown Int
    | AccountDelete
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | AllDeleteText
    | Sex String
    | BodyInfo String String
    | SaveBody
    | SaveComplete (Result Http.Error Decoder.Success)
    | BodyInfoComplete (Result Http.Error BodyData)
    | OldPwd String
    | Pwd String
    | NewPwd String
    | ChangePwd
    | PwdComplete (Result Http.Error Decoder.Success)
    | ChangeProfileImg Bool
    | ResetProfileImg
    | ResetComplete (Result Http.Error Decoder.Success)
    | ChangeComplete (Result Http.Error FileData)
    | ChangeProfileImage (List Files.File)
    | GotPreviews (List String)
    | ChangeProfile

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

saveEncode model =
    let
        body = 
            Encode.object   
                [ ("weight", Encode.int model.weight)
                , ("goal_weight", Encode.int model.goal_weight)
                , ("height", Encode.int model.height)
                , ("birthday", Encode.string model.birthday)
                , ("is_male", Encode.bool model.is_male)]
                |> Http.jsonBody    
    in
    Decoder.resultD
    |> Api.post Endpoint.bodyRecord (Session.cred model.session) SaveComplete body 
pwdEncode model session = 
    let
        list = 
            Encode.object
             [ ("password", Encode.string model.oldpwd)
             , ("new_password", Encode.string model.pwd)
             , ("new_password_confirmation", Encode.string model.newpwd)]   
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
    |> Api.post Endpoint.profileImg (Session.cred session) ChangeComplete body 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        ChangeProfile ->
            (model, profileEncode model.profileImg model.session)
            -- (model, Cmd.none)
        GotPreviews urls ->
            ({model | preview = urls}, Cmd.none)
        ChangeProfileImage file->
            let
                title =
                    List.head (file)
            in
            case title of
                Just a ->
                    ({model | profileImg = file, profileFileName = Just (Files.name a), currentPage = "image"}, Task.perform GotPreviews <| Task.sequence <|
                    List.map Files.toUrl (file)
                    )
            
                Nothing ->
                    (model, Cmd.none)
        ChangeComplete (Ok ok) ->
            ({model | currentPage = "", showbottomtoast = "showbottomToast  lastShowToast"}, Cmd.batch [
                Api.showToast (Encode.string "프로필이 변경되었습니다.")
                ,Decoder.dataWRap My.DataWrap MyData UserData
                    |> Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) 
            ])
        ChangeComplete (Err err) ->
            ({model | currentPage = "", showbottomtoast = "showbottomToast  lastShowToast"}, Api.showToast (Encode.string "프로필을 변경할 수 없습니다."))
        ResetComplete (Ok ok) ->
            ({model | showbottomtoast = "showbottomToast  lastShowToast"}, Cmd.batch [
                Api.showToast (Encode.string "기본이미지로 변경되었습니다.")
                , 
                Decoder.dataWRap DataWrap MyData UserData
                |> Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) 
            ])
        ResetComplete (Err err) ->
            ({model | showbottomtoast = "showbottomToast  lastShowToast"}, Api.showToast (Encode.string "기본 이미지를 변경할 수 없습니다."))
        ResetProfileImg ->
            ({model | showbottomtoast = "showbottomToast  lastShowToast"}, 
            Decoder.resultD
                |> Api.get ResetComplete Endpoint.resetprofileImg (Session.cred model.session))
        ChangeProfileImg bool ->
            if bool then
            ({model | showbottomtoast = "showbottomToast "}, Cmd.none)
            else
            ({model | showbottomtoast = "showbottomToast  lastShowToast"}, Cmd.none)
        GotSession session ->
            if model.deleteAuth == "pwd" then
            update ChangePwd {model | session = session}
            else if model.deleteAuth == "bodyInfo" then
            update SaveBody {model | session = session}
            else if model.deleteAuth == "accountDelete" then
            update AccountDelete {model | session =session}
            else if model.deleteAuth == "nick" then
            update ChangeGo {model | session =session}
            else
            ({model | session = session},
            Cmd.none
            )
        PwdComplete (Ok ok) ->
            let
                textEncode = Encode.string "변경되었습니다."
            in
            ({model | notmatchPwd = "", currentPage = ""}, Api.showToast textEncode)
        PwdComplete (Err err) ->
            let
                serverError = Api.decodeErrors err
            in
            if serverError == "401" then
            ({model | deleteAuth = "pwd"}, (Session.changeInterCeptor (Just serverError) model.session))
            else 
            ({model | notmatchPwd = "기존에 사용하시던 비밀번호를 다시 한번 확인 해 주세요."}, Cmd.none)
        OldPwd pwd ->
            ({model | oldpwd = pwd}, Cmd.none)
        Pwd pwd ->
            ({model | pwd = pwd}, Cmd.none)
        NewPwd pwd ->
            ({model | newpwd = pwd}, Cmd.none)
        ChangePwd ->
            if model.oldpwd == "" then
            ({model| notmatchPwd = "기존에 사용하시던 패스워드를 입력 해 주세요."}, Cmd.none)
            else if model.pwd /= model.newpwd then
            ({model| notmatchPwd = "비밀번호가 일치하지 않습니다."}, Cmd.none)
            else if String.length model.pwd < 5 then
            ({model | notmatchPwd = "비밀번호를 6자리 이상 입력 해 주세요."}, Cmd.none)
            else
            ({model | notmatchPwd = ""}, Cmd.batch[ pwdEncode model model.session ])
        BodyInfoComplete (Ok ok) ->
            ({model | currentPage = "body", birthday = ok.data.birthday, goal_weight = ok.data.goal_weight, height = ok.data.height, sex = if ok.data.is_male then "Male" else "Female", weight= ok.data.weight}, Cmd.none)
        BodyInfoComplete (Err err) ->
            let 
                serverErrors =
                    Api.decodeErrors err
            in
            if serverErrors == "BadBody" then
            ({model | currentPage = "body"}, Cmd.none)
            else if serverErrors == "401" then
            ({model | deleteAuth = "bodyinfo"}, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            ({model | currentPage = "body"}, Cmd.none)
        SaveComplete (Ok ok) ->
            let
                text = Encode.string "저장 되었습니다."
            in
            (model, Cmd.batch
            [ Api.showToast text
            , Api.historyUpdate (Encode.string "myAccount")
            -- , Route.pushUrl (Session.navKey model.session) Route.MyAccount
            ])
        SaveComplete (Err err) ->
            (model, Cmd.none) 
        SaveBody ->
            (model, saveEncode model)
        BodyInfo info what ->
            let
                toInt = String.toInt what
            in
            (
            case toInt of
                Just int ->
                    case info of
                        "weight" ->
                            {model | weight = int}
                        "goalWeight" ->
                            {model | goal_weight = int}            
                        "height" ->
                            {model | height = int}
                        "date" ->
                            {model | birthday = what}
                        _ ->
                            model
                Nothing ->
                    {model | birthday = what}
            , Cmd.none)
                    
        Sex sex ->
            if sex == "Male" then
            ({model | sex = sex, is_male = True} ,Cmd.none)
            else
            ({model | sex = sex, is_male = False}, Cmd.none)
        AllDeleteText ->
            ({model | nickname = ""}, Cmd.none)
        DeleteSuccess (Ok ok) ->
            (model,
            Route.replaceUrl (Session.navKey model.session) Route.Logout)
        DeleteSuccess (Err err) -> 
            let
                serverErrors = Api.decodeErrors err
            in
            
            ({model | deleteAuth = "accountDelete"},(Session.changeInterCeptor (Just serverErrors) model.session))
        AccountDelete ->
            let
                body = Encode.object [("is_leave", Encode.bool True)]
                    |> Http.jsonBody
            in
            
            (model, 
            Decoder.resultD
            |> Api.post Endpoint.accountDelete (Session.cred model.session) DeleteSuccess body   )
        KeyDown key ->
            if key == 13 then
                update ChangeGo model
            else
                (model, Cmd.none)
        SuccessNickname (Ok ok) ->
            ({model | currentPage = ""}, 
            Decoder.dataWRap My.DataWrap MyData UserData
           |>  Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) )
        SuccessNickname (Err err) ->
            let
                serverErrors = Api.decodeErrors err
            in
            
            ({model | deleteAuth = "nick"},(Session.changeInterCeptor (Just serverErrors) model.session))
        ChangeGo ->
            let
                list = 
                    "nickname="
                    ++ model.nickname
                        |> Http.stringBody "application/x-www-form-urlencoded"
            in
            (model, Cmd.batch 
            [ 
                Decoder.resultD
                |>Api.post Endpoint.changeNick (Session.cred model.session)  SuccessNickname list ])
        ChangeNick str ->
            ({model | nickname = str},Cmd.none)
        ChangePage str ->
            if str == "body" then
            (model, 
            Decoder.bodyInfo BodyData BodyInfoData
                |>Api.get BodyInfoComplete Endpoint.getBodyInfo (Session.cred model.session) )
            else
            ({model | currentPage = str,  showbottomtoast = "showbottomToast  lastShowToast"}, Cmd.none)
        MyInfoData (Ok ok) ->
            ({model | mydata = ok.data}, Cmd.none)
        MyInfoData (Err err) ->
            (model, Cmd.none)
        BackBtn ->
            ( model, 
            -- Route.pushUrl (Session.navKey model.session) Route.MyPage 
            Api.historyUpdate (Encode.string "myPage")
            )

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.currentPage == "nick" then
                div [] [
                    appHeaderConfirmDetailR "닉네임 변경" "myPageHeader whiteColor" (ChangePage "")  ChangeGo  "확인"
                    , nicknameContents model 
                ]
        else if  model.currentPage == "pwd" then
                div [] [
                        appHeaderConfirmDetailR "비밀번호 변경" "myPageHeader  whiteColor" (ChangePage "")  ChangePwd "변경"
                        , pwdContents model
                    ]
        else if model.currentPage == "account" then
                div [] [
                    appHeaderRDetailClick "계정관리" "myPageHeader  whiteColor" (ChangePage "") "fas fa-angle-left"
                    , accountContents model
                ]
        else if model.currentPage == "body" then
                div [] [
                    appHeaderConfirmDetailR "신체정보관리" "myPageHeader whiteColor" (ChangePage "")  SaveBody  "저장"
                    , bodyRecord model 
                ]
        else if model.currentPage == "image" then
            div [] [
                appHeaderConfirmDetailR "프로필 사진 변경" "myPageHeader whiteColor" (ChangePage "")  ChangeProfile  "저장"
                , div [] (List.map previewLayout model.preview )
            ]
        else
               div [] [
                    appHeaderRDetailClick "마이페이지" "myPageHeader whiteColor" BackBtn "fas fa-angle-left"
                    , contents model
                ]             
    
    }

previewLayout item = 
    div [] [
        img [src item] []
    ]
justData cases = 
    case cases of
        Just a ->
            a
    
        Nothing ->
            " 닉네임을 등록 해 주세요. "
nicknameContents model =    
    div [class "nicknameStyle"] [
        input [onKeyDown KeyDown, type_ "text",value model.nickname, onInput ChangeNick, placeholder "닉네임을 입력 해 주세요." ] []
        , span [class "allDeleteBtn", onClick AllDeleteText ] [
            i [ class "far fa-times-circle" ]
            []
        ]
    ]

accountContents model = 
    div [] [
        div [ class "container yf_container" ]
            [ div [ class "settingbox" ]
                [ text "계정기록"  
                --, a [ class "button is-large is-fullwidth settingmenu" ]
                --     [ text "기록초기화" ], text "계정연결" 
                ,  a [ class "button is-large is-fullwidth settingmenu", Route.href Route.Logout ]
                    [ text "로그아웃" ], text "계정관리" 
                , div [ class "button is-large is-fullwidth settingmenu", onClick AccountDelete ]
                    [ text "회원탈퇴" ]
                ]
            ]
    ]
contents model = 
        div [ class "container yf_container" ]
            [ div [ class "m_yf_mypage_setting" ]
                [ div [ class "m_profilebox" ]
                    [ div [onClick (ChangeProfileImg True)] [
                        case model.mydata.user.profile of
                            Just image ->
                                img [src image][]
                        
                            Nothing ->
                                img [src "../image/profile.png"] []
                        , div [class"m_photo_changebox"]
                         [
                            div [ class "button is-dark m_photo_change"]
                            [ text "프로필 사진 변경" ]
                        ]
                    ]
                    , p [class"m_account_id"]
                        [ text (justData model.mydata.user.nickname) ]
                    ]
                , div [ class "loginbox_info" ]
                    [ p [ class "m_logintext" ]
                        [ text "로그인ID" ]
                    , p [ class "login_id" ]
                        [ text model.mydata.user.username ]
                    ]
                , p [ class "yf_terms" ]
                    [ a [Route.href Route.Private]
                        [ text "개인정보 보호 및 약관확인" ]
                    ]
                , div [ class "settingbox" ]
                    [ div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "nick") ]
                        [ text "닉네임 변경" ]
                    , div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "pwd") ]
                        [ text "패스워드 변경" ]
                    , div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "body") ]
                        [ text "신체기록 관리" ]
                    , div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "account") ]
                        [ text "계정관리" ]
                    ]
                , p [ class "m_yf_version" ]
                    [ text "버전정보 v1.0" ]
                , div [class ("noShowToast " ++ model.showbottomtoast)] [
                    ul [] [
                        li [onClick ResetProfileImg] [
                            text "기본 이미지로 변경"
                        ]
                        , li [] [
                            label [] [
                            text "앨범에서 사진 선택"
                            , div [] [
                                input [ class "appFile", type_ "file", multiple False, id "thumbFile", accept "image/*" 
                            , on "change" (Decode.map ChangeProfileImage targetFiles)  
                            ]
                                []
                            ]
                            ]
                        ]
                        , li [class "toastCancel", onClick (ChangeProfileImg False)] [
                            text "취소"
                        ]
                    ]
                ]
                ]
            ]

bodyRecord model = 
   div [ class "settingbox" ]
    [ div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "성별" ]
        , p [ class "physicalbox" ]
            [ 
            label [ class "radio" ]
                [ input [ type_ "radio", name "question"
                , value model.sex
                , onClick (Sex "Male")
                         ]
                    []
                , i [ class (
                    if model.sex == "Male" then
                        "fas fa-mars man man_colorChange"
                    else
                        "fas fa-mars man"
                ) ]
                    [] 
                ]
                -- ,div [] [text "남자"]
            , label [ class "radio" ]
                [ input [ type_ "radio", name "question"
                , value model.sex
                , onClick (Sex "Female")
                 ]
                    []
                , i [ class (
                    if model.sex == "Female" then
                        "fas fa-venus waman woman_colorChange"
                    else
                        "fas fa-venus waman"
                ) ]
                    []
                -- ,div [] [text "여자" ]
                ]
            ]
        ]
    , div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "체중" ]
        , p [ class "physicalbox" ]
        [   input [ class "input yf_input_box", type_ "number" ,onInput (BodyInfo "weight"), value (
            if model.weight == 0 then
                ""
            else
            String.fromInt(model.weight)
        ) ]
            [], text "Kg"
            ]
        ]
    , div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "목표체중" ]
        , p [ class "physicalbox" ]
            [ input [ class "input yf_input_box", type_ "number" ,onInput (BodyInfo "goalWeight") , value (
            if model.weight == 0 then
                ""
            else
            String.fromInt(model.goal_weight)
        )]
                [], text "Cm" 
            ]
        ]
    , div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "신장" ]
        , p [ class "physicalbox" ]
            [ input [ class "input yf_input_box", type_ "number", onInput (BodyInfo "height"), value (
            if model.height == 0 then
                ""
            else
            String.fromInt(model.height)
        ) ]
                [], text "Kg" 
            ]
        ]
    , div [ class "physical_setting_birth" ]
        [ label [ class "label physical_title" ]
            [ text "생년월일" ]
        , p [ class "physicalbox2" ]
            [ input [ class "input yf_input_box2", type_ "date", onInput (BodyInfo "date"), value model.birthday ]
                []
            ]
        ]
    ]

pwdContents  model =
    div [class "m_pwd"] [
        div [class "notmatchPwd"] [text model.notmatchPwd]
        , input [ class "input myPage_yf_input", type_ "text", placeholder "기존의  패스워드를 입력 해 주세요." , onInput OldPwd]
        []
        , input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 입력 해 주세요." , onInput Pwd]
        []
        ,input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 한번 더 입력 해 주세요." , onInput NewPwd]
        []
        -- , div [ class "button mypage_nickbtn", onClick ChangePwd ]
        --     [ text "적용" ]
        ]