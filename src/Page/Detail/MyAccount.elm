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
import Page.MyPageMenu.MyPageInfoLayout exposing(..)

type alias Model 
    = {
        session : Session
        , check : Bool
        , mydata : MyData
        , currentPage : String
        , nickname : String
        , sex : String
        , weight : String
        , goal_weight : String
        , height : String
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
        , show : String
        , deleteOrNot : String
        , canNotUpdateField : String
        , showDetail : String
        , protain : Protain
        }

type alias BodyData = 
    {data : BodyInfoData }

type alias BodyInfoData =
    { birthday :String
    , body_no : Int
    , goal_weight :String
    , height : String
    , is_male : Bool
    , weight : String 
    , age : Int
    , protain : Protain
    }

type alias Protain = 
    { need : Maybe Int
    , recommend : Maybe Int}

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
        , sex = "Male"
        , deleteAuth = ""
        , cannotChange = ""
        , notmatchPwd = ""
        , oldpwd = ""
        , show = ""
        , deleteOrNot = ""
        , newpwd = ""
        , canNotUpdateField = ""
        , profileFileName = Nothing
        , pwd = ""
        , profileImg = []
        , weight = ""
        , goal_weight = ""
        , height = ""
        , preview = []
        , birthday = ""
        , is_male = True
        , showbottomtoast = ""
        , showDetail = ""
        , protain = 
            { need = Nothing
            , recommend = Nothing}
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
        , Decoder.bodyInfo BodyData BodyInfoData Protain
            |>Api.get BodyInfoComplete Endpoint.getBodyInfo (Session.cred session)
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
    | AccountDelete String
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
    | DeleteConfirm
    | ShowDetail String
    | NoOp

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

justFloat item = 
    let
        flaot = String.toFloat item
    in
    
    case flaot of
        Just ok ->
            ok
    
        Nothing ->
            0

saveEncode model =
    let
        body = 
            Encode.object   
                [ ("weight", Encode.float (justFloat model.weight))
                , ("goal_weight", Encode.float (justFloat model.goal_weight))
                , ("height", Encode.float (justFloat model.height))
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

updateFieldBodyInfo item model = 
    case String.toFloat item of
        Just ok ->
            if ok == 0 then
            ({model | canNotUpdateField = "유효하지 않은 입력 값 입니다."}, Cmd.none)
            else
            ({model | showDetail = "", canNotUpdateField = ""}, Cmd.none)
        Nothing ->
            ({model | canNotUpdateField = "유효하지 않은 입력 값 입니다."}, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of     
        NoOp -> 
            (model ,Cmd.none)
        ShowDetail detail ->
            case detail of
                "weightInput" ->   
                    updateFieldBodyInfo model.weight model  
                "goalWeightInput" ->
                    updateFieldBodyInfo model.goal_weight model  
                "heightInput" ->
                    updateFieldBodyInfo model.height model  
                "birthInput" ->
                    if model.birthday == "" then
                    ({model | canNotUpdateField = "유효하지 않은 입력 값 입니다."}, Cmd.none)
                    else
                    ({model | showDetail = "", canNotUpdateField = ""}, Cmd.none)
                _ ->
                    ({model | showDetail = detail, canNotUpdateField = ""}, Cmd.none)

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
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            ({model | currentPage = "", showbottomtoast = "showbottomToast  lastShowToast"}, Api.showToast (Encode.string "프로필을 변경할 수 없습니다."))
        ResetComplete (Ok ok) ->
            ({model | showbottomtoast = "showbottomToast  lastShowToast"}, Cmd.batch [
                Api.showToast (Encode.string "기본이미지로 변경되었습니다.")
                , 
                Decoder.dataWRap DataWrap MyData UserData
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
            update (AccountDelete model.deleteOrNot) {model | session =session}
            else if model.deleteAuth == "nick" then
            update ChangeGo {model | session =session}
            else
            ({model | session = session},
            Cmd.none)
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
            ({model | birthday = ok.data.birthday, goal_weight = ok.data.goal_weight, height = ok.data.height, sex = if ok.data.is_male then "Male" else "Female", weight = ok.data.weight, protain = ok.data.protain }, Cmd.none)
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
            ({model | currentPage = "body"}, Cmd.batch
            [ Api.showToast text
            -- , -- Api.historyUpdate (Encode.string "myAccount")
            , Route.pushUrl (Session.navKey model.session) Route.MyAccount
            ])
        SaveComplete (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
        SaveBody ->
            if model.weight == "0" || model.goal_weight == "0" || model.height == "0" || model.birthday == "" || model.weight == "" || model.goal_weight == "" || model.height == "" then
            ({model | canNotUpdateField = "항목을 정확히 입력 해 주세요."}, Cmd.none)
            else
            (model, saveEncode model)
        BodyInfo info what ->
            let
                split = String.split "." what
                tail = List.drop 1 split
                len = List.map (\x ->
                        String.slice 0 2 x
                    ) tail
                head = List.take 1 split
                result = String.join "." (head ++ len)
                -- toInt = String.toFloat result
            in
            if what == "" then
            (case info of
                "weight" ->
                    {model | weight = ""}
                "goalWeight" ->
                    {model | goal_weight = ""}            
                "height" ->
                    {model | height = ""}
                "date" ->
                    {model | birthday = what}
                _ ->
                    model
                , Cmd.none )
            else
                case String.toFloat what of
                    Just ok ->
                        (case info of
                        "weight" ->
                            {model | weight = result}
                        "goalWeight" ->
                            {model | goal_weight = result}            
                        "height" ->
                            {model | height = result}
                        "date" ->
                            {model | birthday = what}
                        _ ->
                            model
                            
                        , Cmd.none)
                
                    Nothing ->
                        ({model | birthday = what}, Cmd.none)
            
                    
        Sex sex ->
            if sex == "male" then
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
            if serverErrors == "401" then
            ({model | deleteAuth = "accountDelete"},(Session.changeInterCeptor (Just serverErrors) model.session))
            else 
            (model , Api.showToast (Encode.string "삭제 할수 없습니다."))
        AccountDelete str->
            let
                body = Encode.object [("is_leave", Encode.bool True)]
                    |> Http.jsonBody
            in            
            if str == "go" then
            ({model | deleteOrNot = str}, 
            Decoder.resultD
            |> Api.post Endpoint.accountDelete (Session.cred model.session) DeleteSuccess body )
            else 
            ({model | show = "", deleteOrNot = str}, Cmd.none)
        DeleteConfirm ->
            ({model | show = "logoutShow"}, Cmd.none)
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
            if serverErrors == "401" then
            ({model | deleteAuth = "nick"},(Session.changeInterCeptor (Just serverErrors) model.session))
            else 
            (model, Api.showToast (Encode.string "변경 할 수 없습니다."))
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
            ({model | currentPage = "body" }, 
            Cmd.batch [
                Decoder.bodyInfo BodyData BodyInfoData Protain
                |>Api.get BodyInfoComplete Endpoint.getBodyInfo (Session.cred model.session)
                , scrollToTop NoOp
            ] )
            else if str =="nick" then
            ({model | currentPage = str}
            , Cmd.batch[Api.scrollControl ()
            , scrollToTop NoOp])
            else
            ({model | currentPage = str}, Cmd.batch [scrollToTop NoOp
            ])
        MyInfoData (Ok ok) ->
            ({model | mydata = ok.data}, Cmd.none)
        MyInfoData (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
        BackBtn ->
            ( model, 
            Route.pushUrl (Session.navKey model.session) Route.MyPage 
            -- -- Api.historyUpdate (Encode.string "myPage")
            )

view : Model -> {title : String , content : Html Msg}
view model =
    -- case model.currentPage of
    --     "nick" ->
            { title = "닉네임 변경"
            , content =
                div [style "min-height" "750px"] [
                    -- appHeaderConfirmDetailR "닉네임 변경" "myPageHeader whiteColor" (ChangePage "")  ChangeGo  "확인"
                    div [] [
                         contents model
                        , nicknameContents model
                        , pwdContents model
                        , accountContents model
                        , bodyRecord model 
                        , div [] (List.map previewLayout model.preview )
                    ] 
                ]
            }
        -- "pwd" ->
        --     { title = "비밀번호 변경"
        --     , content = 
        --         div [] [
        --                 appHeaderConfirmDetailR "비밀번호 변경" "myPageHeader  whiteColor" (ChangePage "")  ChangePwd "변경"
        --                 , pwdContents model
        --             ]
        --     }
        -- "account" ->
        --     { title = "계정관리"
        --     , content = 
        --         div [] [
        --             appHeaderRDetailClick "계정관리" "myPageHeader  whiteColor" (ChangePage "") "fas fa-angle-left"
        --             , accountContents model
        --         ]
        --     }
        -- "body" ->
        --     { title = "신체정보관리"
        --     , content = 
        --         div [id "mypage_container"] [
        --             bodyRecord model 
        --         ]
        --     }
        
        -- "image" ->
        --     { title =" 프로필 사진"
        --     , content = 
        --         div [] [
        --         appHeaderConfirmDetailR "프로필 사진 변경" "myPageHeader whiteColor" (ChangePage "")  ChangeProfile  "저장"
        --         , div [] (List.map previewLayout model.preview )
        --         ]
        --     }
        -- _ ->
        --     { title ="마이페이지"
        --     , content = 
        --         div [] [
        --             appHeaderRDetailClick "마이페이지" "myPageHeader whiteColor" BackBtn "fas fa-angle-left"
        --             , contents model
        --         ]
        --     }

    

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
    div [ class ("control has-icons-right myaccountStyle "  ++ (if model.currentPage == "nick" then model.currentPage else ""))] [
        ul [class "accountHeader"] 
                [ li[onClick (ChangePage "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "닉네임 변경"]
                , li[onClick ChangeGo][
                   text "확인"
                ]
                ],
        div [style "position" "relative"] [
            input [onKeyDown KeyDown, type_ "text",value model.nickname, onInput ChangeNick, placeholder "닉네임을 입력 해 주세요.", maxlength 10 , class "input nicknameInput", id (if model.currentPage == "nick" then "noScrInput" else "")] []
        , span [class "allDeleteBtn icon is-right", onClick AllDeleteText ] [
            i [ class "far fa-times-circle" ]
            []
        ]
        ]
    ]

accountContents model = 
        div [ class ("container yf_container myaccountStyle "  ++ (if model.currentPage == "account" then model.currentPage else ""))]
            [ ul [class "accountHeader"] 
                [ li[onClick (ChangePage "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "계정관리"]
                , li[][
                   text "확인"
                ]
                ],
                div [ class "settingbox" ]
                [ text "계정기록"  
                --, a [ class "button is-large is-fullwidth settingmenu" ]
                --     [ text "기록초기화" ], text "계정연결" 
                ,  a [ class "button is-large is-fullwidth settingmenu", Route.href Route.Logout ]
                    [ text "로그아웃" ], text "계정관리" 
                , div [ class "button is-large is-fullwidth settingmenu", onClick DeleteConfirm ]
                    [ text "회원탈퇴" ]
                , mremovelayer model.show AccountDelete
                ]
            ]
contents model = 
        div [ class ("container yf_container account_container "++ (if model.currentPage /= "" then "fadeContainer" else ""))]
            [ 
                 appHeaderRDetailClick "마이페이지" "myPageHeader whiteColor" BackBtn "fas fa-angle-left"
                ,div [ class "m_yf_mypage_setting" ]
                [ div [ ]
                    [ div [onClick (ChangeProfileImg True), class "m_profilebox"] [
                        case model.mydata.user.profile of
                            Just image ->
                                img [src image][]
                        
                            Nothing ->
                                img [src "../image/profile.png"] []
                        , div [class"m_photo_changebox"]
                         [
                            div [class "protainStyle"] [
                                 case model.protain.need of
                                    Just ok ->
                                        div [style "padding" ".5rem"] [
                                        span [style "font-weight" "bold"][text ( "단백질 평균필요량 (per day) : ")]
                                        , span [] [text (String.fromInt ok ++ "g")]
                                        , div [][
                                            span [style "font-weight" "bold"] [text ( "단백질 권장섭취량 (per day) : ")]
                                            , span [] [text ((case model.protain.recommend of
                                                Just recom ->
                                                    String.fromInt recom
                                            
                                                Nothing ->
                                                    ""
                                            ) ++ "g")]
                                            ]
                                        ]
                                    Nothing ->
                                        text "신체 정보를 기록 해 주세요."
                            ]
                            , div [ class "button is-dark m_photo_change"]
                            [ text "프로필 사진 변경" ]
                        ]
                    ]

                    ]
                , div [ class "loginbox_info" ]
                    [ p [ class "m_logintext" ]
                        [ text "닉네임" ]
                    , p [ class "login_id" ]
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

bodysex model = 
   div [ class "editTop showAccount" ]
    [ 
        -- appHeaderConfirmDetailR "성별" "myPageHeader whiteColor" (ShowDetail "")  (ShowDetail "")  "확인"
        -- , 
        div [ class "editData" ]
        [  ul [class "accountHeader"] 
                [ li[onClick (ShowDetail "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text  "성별"]
                , li[onClick (ShowDetail "")][
                   text "확인"
                ]
                ], p [ class "editdataBoxsex" ]
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
    ]
bodyweight model = 
    div [class "editTop showAccount"] [
        div [  class "editData"]
        [ ul [class "accountHeader"] 
                [ li[onClick (ShowDetail "weightInput")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "체중"]
                , li[onClick (ShowDetail "weightInput")][
                   text "확인"
                ]
                ],
        p [ class "editdataBox" ]
        [   div [] [
            input [ class "input yf_input_box" ,onInput (BodyInfo "weight"), value model.weight, type_ "number", id (if model.showDetail /= "" then "noScrInput" else "" ) ]
            [], text "Kg"
            ]
             , div [class "wrongValueWarn"] [text model.canNotUpdateField]
            ]
        ]
    ]


goalweight model = 
    div [class "editTop showAccount"] [
        --  appHeaderConfirmDetailR "목표체중" "myPageHeader whiteColor" (ShowDetail "goalWeightInput")  (ShowDetail "goalWeightInput")  "확인"
        -- , 
        div [  class "editData"]
        [ ul [class "accountHeader"] 
                [ li[onClick (ShowDetail "goalWeightInput")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "목표체중"]
                , li[onClick (ShowDetail "goalWeightInput")][
                   text "확인"
                ]
                ],
            p [ class "editdataBox" ]
        [   div [] [
            input [ class "input yf_input_box" ,onInput (BodyInfo "goalWeight") , value model.goal_weight, type_ "number", id (if model.showDetail /= "" then "noScrInput" else "" )]
                [], text "Kg"
        ]
        , div [class "wrongValueWarn"] [text model.canNotUpdateField]
            ]
        ]
    ]

height model = 
    div [class "editTop  showAccount"] [
        --  appHeaderConfirmDetailR "신장" "myPageHeader whiteColor" (ShowDetail "heightInput")  (ShowDetail "heightInput")  "확인"
        -- , 
        div [  class "editData"]
        [ 
            ul [class "accountHeader"] 
                [ li[onClick (ShowDetail "heightInput")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "신장"]
                , li[onClick (ShowDetail "heightInput")][
                   text "확인"
                ]
                ]
            , p [ class "editdataBox" ]
        [   div [][
            input [ class "input yf_input_box", onInput (BodyInfo "height"), value model.height , type_ "number", id (if model.showDetail /= "" then "noScrInput" else "" )]
                [], text "Cm"
        ] 
            , div [class "wrongValueWarn"] [text model.canNotUpdateField]
            ]
        ]
    ]

birth model = 
    div [class "editTop showAccount"] [
        
        --  appHeaderConfirmDetailR "생년월일" "myPageHeader whiteColor" (ShowDetail "birthInput")  (ShowDetail "birthInput")  "확인"
        -- ,
         div [  class "editData"]
        [ ul [class "accountHeader"] 
                [ li[onClick (ShowDetail "birthInput")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "생년월일"]
                , li[onClick (ShowDetail "birthInput")][
                   text "확인"
                ]
                ],
           p [ class "editdataBox" ]
        [  div [] [
            input [ class "input yf_input_box2", type_ "date", onInput (BodyInfo "date"), value model.birthday , style "min-width" "200px", id (if model.showDetail /= "" then "noScrInput" else "" )]
                []
        ]
        , div [class "wrongValueWarn"] [text model.canNotUpdateField]
        ]
    ]
    ]


bodyRecord model = 
   div [ class ("settingbox myaccountStyle " ++ (if model.currentPage == "body" then model.currentPage else "")) ]
    [ 
        ul [class "accountHeader"] 
                [ li[onClick (ChangePage "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "신체정보"]
                , li[onClick SaveBody][
                   text (if model.weight == "" then "등록" else "수정")
                ]
                ],
        -- appHeaderConfirmDetailR "신체정보" "myPageHeader whiteColor"  (ChangePage "") SaveBody  (if model.weight == "" then "등록" else "수정")
        -- , 
        div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "성별" ]
        , p [ class "physicalbox", onClick (ShowDetail "sex")]
            [ 
            label [ class "radio" ]
                [ input [ type_ "radio", name "question"
                , value model.sex
                -- , onClick (Sex "Male")
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
                -- , onClick (Sex "Female")
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
        , p [ class "physicalbox bodyinput", onClick (ShowDetail "weight") ]
        [   div [ class "input yf_input_box " ]
            [text model.weight], text "Kg"
            ]
        ]
    , div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "목표 체중" ]
        , p [ class "physicalbox bodyinput" ]
            [ div [ class "input yf_input_box", onClick (ShowDetail "goalweight")]
                [text model.goal_weight], text "Kg" 
            ]
        ]
    , div [ class "physical_setting" ]
        [ label [ class "label physical_title" ]
            [ text "신장" ]
        , p [ class "physicalbox bodyinput", onClick (ShowDetail "height") ]
            [ div [ class "input yf_input_box" ]
                [text model.height ], text "Cm" 
            ]
        ]
    , div [ class "physical_setting_birth" ]
        [ label [ class "label physical_title" ]
            [ text "생년월일" ]
        , p [ class "physicalbox2 bodyinput", onClick (ShowDetail "birth") ]
            [ div [ class "input yf_input_box2" ]
                [text model.birthday]
            ]
        ],
        case model.showDetail of
            "sex" ->
                bodysex model   
            "weight" ->
                bodyweight model
            "goalweight" ->
                goalweight model
            "height" ->
                height model
            "birth" ->
                birth model
            _ ->
                div [class "editTop "] []
    ]


pwdContents  model =
    div [class ("m_pwd myaccountStyle " ++ (if model.currentPage == "pwd" then model.currentPage else ""))] [
        ul [class "accountHeader"] 
                [ li[onClick (ChangePage "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "패스워드 변경"]
                , li[][
                   text "확인"
                ]
                ],
        div [class "notmatchPwd"] [text model.notmatchPwd]
        , input [ class "input myPage_yf_input", type_ "text", placeholder "기존의  패스워드를 입력 해 주세요." , onInput OldPwd, id (if model.currentPage == "pwd" then "noScrInput" else "")]
        []
        , input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 입력 해 주세요." , onInput Pwd]
        []
        ,input [ class "input myPage_yf_input", type_ "text", placeholder "변경할 패스워드를 한번 더 입력 해 주세요." , onInput NewPwd]
        []
        -- , div [ class "button mypage_nickbtn", onClick ChangePwd ]
        --     [ text "적용" ]
        ]


