module Page.Signup exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Route exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Page.MyPageMenu.MyPageInfoLayout exposing(..)
import Page.Common exposing(..)
import Api as Api exposing (Cred)
import Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
type alias Model = 
    { session : Session
    , checkDevice : String
    , matchStyle : String
    , password : String
    , borderMatch: String
    , errmsg : String
    , pwdmsg : String
    , pwdborder : String
    , pwdstyle : String
    , mail : String
    , repwd : String
    , reborder: String
    , restyle : String
    , repassword : String
    , err : String
    , check : Bool
    , authNo : String
    , emailComplate : String
    }
type alias Token =  {
    token : String
    }

type alias AuthSendData = {
    data : String
    }
type alias CheckOverlapUserName = 
    { data : Bool}

datalist model= 
    Encode.object
        [ ("username", Encode.string model.mail)
        , ("auth_no", Encode.string model.authNo)
        , ("password" , Encode.string model.password)
        , ("password_confirmation", Encode.string model.repassword)]
        
signupEncoder model session = 
    let
        wrap = 
            Encode.object
                [("user", datalist model)]
        -- datalist = 
        --     Encode.object
        --         [ ("username", Encode.string model.mail)
        --         , ("password" , Encode.string model.password)
        --         , ("password_confirmation", Encode.string model.repwd)]
        body = 
            wrap
                |> Http.jsonBody
    in
    (Decoder.tokenDecoder Api.Cred )
    |> Api.post Endpoint.signup (Session.cred session) SuccessData body 

overlapIdEncoder username session = 
    let
        body =
            Encode.object 
                [ ("username", Encode.string username)]
                    |> Http.jsonBody
    in
    (Decoder.checkOverlapmail CheckOverlapUserName)
    |> Api.post Endpoint.checkoverlapId (Session.cred session) CanUseUsername body 
    

emailAuthEncode email session =
    let
        body = Encode.object [("username",Encode.string email)] 
            |> Http.jsonBody
    in
    (Decoder.authMail AuthSendData)
    |> Api.post Endpoint.emailAuth (Session.cred session) AuthComplete body 
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , matchStyle = ""
        , password = ""
        , borderMatch = ""
        , errmsg =""
        , pwdmsg = ""
        , pwdborder = ""
        , pwdstyle = ""
        , mail = ""
        , repwd = ""
        , authNo = ""
        , reborder= ""
        , restyle = ""
        , repassword = ""
        , err = ""
        , emailComplate = ""
        , check = mobile
        }
        , Cmd.none
    )

type Msg 
    = NoOp
    | CheckDevice Encode.Value
    | EmailCheck String
    | PasswordCheck String
    | Submit
    | RePasswordCheck String
    | SuccessData (Result Http.Error Cred )
    | GotSession Session
    | KeyDown Int
    | EmailAuth 
    | AuthComplete (Result Http.Error AuthSendData)
    | EmailAuthInput String
    | CanUseUsername (Result Http.Error CheckOverlapUserName)
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CanUseUsername (Ok ok) ->
            if ok.data then
            ({model | err = ""}, emailAuthEncode model.mail model.session)
            else 
            ({model | err = "이미 사용 중입니다."}, Cmd.none)
        CanUseUsername (Err err) ->
            (model, Cmd.none)
        EmailAuthInput str ->
            if model.authNo == "" then
            ({model | authNo = str} , Cmd.none)
            else
            ({model | authNo = str, emailComplate = ""} , Cmd.none)
        AuthComplete (Ok ok) ->
            ({model | emailComplate = "인증번호가 발송 되었습니다."}, Api.showToast (Encode.string "인증번호가 발송 되었습니다."))
        AuthComplete (Err err) ->
            ({model | emailComplate = ""}, Cmd.none)
        EmailAuth ->
            if model.mail == "" then
            ({model | err = "이메일 주소를 입력 해 주세요."}, Cmd.none)
            else
            (model, 
            overlapIdEncoder model.mail model.session
            )
        KeyDown key ->
            if key == 13 then
                update Submit model
            else
                (model, Cmd.none)
        GotSession session ->
            ( { model | session = session }
            , 
            Cmd.batch[Api.refreshFetchData (),  Route.replaceUrl (Session.navKey model.session) Route.SC]
            )
        SuccessData (Ok ok) -> 
            (model,Cmd.batch[Api.storeCredWith (ok)])
        SuccessData (Err err) -> 
            let
                serverErrors =
                    Api.decodeErrors err
            in
                if serverErrors == "badbody" then
                    ( { model | err = "회원가입을 진행 할 수 없습니다." }
                        , Cmd.none
                        )
                else
                    ( { model | err = serverErrors }
                    , Cmd.none
                    )
        Submit ->
            if model.authNo == "" then
                ({model | err = "이메일 인증 먼저 진행 해 주세요."}, Cmd.none)
            else if String.length model.password < 6 then
                ({model | pwdstyle = "noMatch", pwdborder = "noMatchBorder", pwdmsg = "비밀번호는 7자 이상 입력 해 주세요.", matchStyle = "", borderMatch = "", errmsg = ""}, Cmd.none)
            else if String.length model.password > 3 then
                if model.password  /= model.repassword then
                    ({model | restyle = "noMatch", reborder = "noMatchBorder", repwd = "두 개의 비밀번호가 일치하지 않습니다..", matchStyle = "", borderMatch = "", errmsg = "", pwdstyle = "", pwdborder = "", pwdmsg = ""}, Cmd.none)
                else 
                    ({model | restyle = "", reborder = "", repwd = "", matchStyle = "", borderMatch = "", errmsg = "", pwdstyle = "", pwdborder = "", pwdmsg = ""}, signupEncoder model model.session)
            else 
                (model, signupEncoder model model.session)
        RePasswordCheck repassword ->
            ({model | repassword = repassword}, Cmd.none)
        PasswordCheck pwd->
            ({model | password =pwd}, Cmd.none)
            
        EmailCheck mail ->
            -- if (mail |> String.split("@") |> List.length) /=2 then
            --     ({model | matchStyle = "noMatch", borderMatch = "noMatchBorder", errmsg = "유효한 이메일 주소를 입력 해 주세요."}, Cmd.none)
            -- else 
                ({model | mail = mail}, Cmd.none)
                
        NoOp ->
            ( model, Cmd.none )

        CheckDevice str ->
           let 
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ -> 
                        ({model | checkDevice = ""}, Cmd.none)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitSignr"
    , content = 
        if model.check then
           div [] [
                appHeadermypage "회원가입" "myPageHeader" ,
                mobileLayout model
            ]
        else
            div [] [
                pcLayout model
            ]
            
    }
pcLayout model= 
    div [ class "yourfitExercise_yf_workoutcontainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                   div [] [commonHeader2 "/image/icon_signup.png" "회원가입",
 
            div [ class "signup_yf_box" ]
                [ div[]
                 [img [src "image/signupimage.png", alt "signupimage" ]
                [ ]
                 ],
                
                    h1 [ class "signup_yf_h1" ]
                    [ text "유어핏에서 사용할 이메일을 입력해주세요" ]
                , div [class"sign_text"] [text model.err]
                , div [ class "signup_yf_box2" ]
                    [ p [ class "control has-icons-left signup_yf_inputbox" ]
                        [ input [ class ("input signup_yf_input" ++ model.borderMatch ), type_ "email", placeholder "이메일을 입력해주세요", onInput EmailCheck ]
                            []
                            , div [class "button is-link emailButton", onClick EmailAuth] [text "인증번호 보내기"]
                        , span [ class "icon is-small is-left " ]
                        [ i [ class ("fas fa-envelope a"++ model.matchStyle) ]
                                []
                        ]
                        ]
                        , div [class "emailAuth"][
                            
                            input [ class "input signup_yf_inputbox", type_ "number", placeholder "인증번호를 입력해 주세요.", onInput EmailAuthInput] []
                          ]
                        , div [class "completeAuthEmail"] [text model.emailComplate]                        

                    , p [] [text model.errmsg]
                    , p [ class "control has-icons-left signup_yf_inputbox" ]
                        [ input [ class ("input " ++ model.pwdborder ), type_ "password", placeholder "비밀번호를 입력해주세요", onInput PasswordCheck ]
                            []
                        , span [ class "icon is-small is-left "  ]
                            [ i [ class ("fas fa-lock "++  model.pwdstyle) ]
                                []
                            ]
                        ]
                    , p [] [text model.pwdmsg]
                    , p [ class "control has-icons-left signup_yf_inputbox" ]
                        [ input [ class ("input " ++ model.reborder ), type_ "password", placeholder "비밀번호를 한번 더 입력해주세요", onInput RePasswordCheck ]
                            []
                        , span [ class "icon is-small is-left "  ]
                            [ i [ class ("fas fa-lock "++  model.restyle) ]
                                []
                            ]
                        ]
                    , p [] [text model.repwd]
                    , p [ class "signup_yf_terms" ]
                        [ a [Route.href Route.Private ]
                            [ text "개인정보 보호 및 약관확인" ]
                        ]
                    , p []
                        [ text "개인정보 보호 및 이용약관을 확인해주세요" ]
                    ]
                ]
            ,div[ class "button is-dark signup_yf_darkbut", onClick Submit ]
                [ text "약관동의 및 회원가입" ]
            ]
                    ]
                ]
            ]

mobileLayout model = 
    div [][
    div [ class "topbox" ]
        [ div [ class "backbtn" ]
            []
        , div [ class "nextbtn" ]
            []
        ]
        ,div [ class "m_signup_yf_box" ]
            
        
            [ h1 [ class "m_signup_yf_h1" ]
                [ text "유어핏에서 사용할 이메일을 입력해주세요" ]
            , div [] [text model.err]
            , div [ class "m_signup_yf_box2" ]
                [ p [ class "control has-icons-left m_signup_yf_inputbox" ]
                 [ input [ class ("input " ++ model.borderMatch ), type_ "email", placeholder "이메일을 입력해주세요", onInput EmailCheck ]
                            []
                         , div [class "button is-link emailButton m_signup_yf_inputbox", onClick EmailAuth] [text "인증번호 보내기"]
                        , span [ class "icon is-small is-left" ]
                            [ i [ class "fas fa-envelope sign_icon" ]
                                []
                            ]
                        ]
                    , p [ class "control has-icons-left m_signup_yf_inputbox" ]
                        [ input [ class "input signup_yf_inputbox", type_ "number", placeholder "인증번호를 입력해 주세요.", onInput EmailAuthInput ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ class "fas fa-envelope sign_icon" ]
                                []
                            ]
                        , div [class "completeAuthEmail"] [text model.emailComplate]  
                        ]
                    , p [] [text model.errmsg]
                    , p [ class "control has-icons-left m_signup_yf_inputbox" ]
                        [ input [onKeyDown KeyDown, class ("input " ++ model.pwdborder ), type_ "password", placeholder "비밀번호를 입력해주세요", onInput PasswordCheck ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ class ("fas fa-lock "++  model.pwdstyle) ]
                                []
                            ]
                        ]
                     , p [ class "control has-icons-left m_signup_yf_inputbox" ]
                        [ input [ onKeyDown KeyDown, class ("input " ++ model.reborder ), type_ "password", placeholder "비밀번호를 한번 더 입력해주세요", onInput RePasswordCheck ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ class ("fas fa-lock "++  model.restyle) ]
                                []
                            ]
                        ]
                , p [ class "m_signup_yf_terms" ]
                    [ a [Route.href Route.Private]
                        [ text "개인정보 보호 및 약관확인" ]
                    ]
                , p []
                    [ text "개인정보 보호 및 이용약관을 확인해주세요" ]
                ]
            ]
        ,div [ class "button is-dark m_signup_yf_darkbut", onClick Submit ]
            [ text "약관동의 및 회원가입" ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Session.changes GotSession (Session.navKey model.session)]