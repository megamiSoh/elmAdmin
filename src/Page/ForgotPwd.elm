module Page.ForgotPwd exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Json.Encode as Encode
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Api as Api
import Route as Route
import Page.Common exposing(..)

type alias Model = 
    { session : Session
    , check : Bool
    , sendMail : String
    , nextStep : String
    , authNumber : String
    , cannotsend : String
    , yfEmail : String
    }

init : Session -> Bool -> (Model, Cmd Msg)
init session mobile = 
    ({session = session
    , check = mobile
    , sendMail = ""
    , nextStep = ""
    , authNumber = "" 
    , cannotsend = ""
    , yfEmail = ""}
        , Cmd.none
    )
type alias Data =
    {data : String}

mailEncoder email session= 
    let
        body =
            Encode.object [("username", Encode.string email)]
            |> Http.jsonBody
    in
    (Decoder.authMail Data)
    |>Api.post Endpoint.emailAuth (Session.cred session) MailSendComplete body 
    

temporaryEncoder model session =
    let
        body =
            Encode.object 
                [ ("username", Encode.string model.yfEmail)
                , ("auth_no", Encode.string model.authNumber)]
                    |> Http.jsonBody
    in
    (Decoder.resultD)
    |> Api.post Endpoint.temporaryPwd (Session.cred session) SendPwd body 
    

type Msg 
    = NoOp
    | SendMail String
    | MailSendComplete (Result Http.Error Data)
    | GoSendMail
    | AuthInput String
    | FindPwd String String
    | TemporaryPwd
    | SendPwd (Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendPwd ( Ok ok ) ->
            (model, Cmd.batch[
                Api.historyUpdate (Encode.string "login")
                -- Route.pushUrl (Session.navKey model.session) Route.Login, 
                , Api.showToast (Encode.string "임시비밀번호가 발송되었습니다")
                ])
        SendPwd ( Err err ) ->
             ({model |cannotsend = "이메일계정 또는 인증번호를 다시 확인 해 주세요."}, Cmd.none)
        TemporaryPwd ->
            if model.yfEmail == "" then
                ({model | cannotsend = "유어핏 계정을 입력 해 주세요."}, Cmd.none)
            else if model.authNumber == "" then
                ({model | cannotsend = "인증번호를 입력 해 주세요."}, 
                Cmd.none)
            else
                ({model | cannotsend = ""}, Cmd.batch[temporaryEncoder model model.session])
        FindPwd category str ->
            case category of
                "mail" ->
                    ({model | yfEmail = str}, Cmd.none)
            
                "auth" ->
                    ({model | authNumber = str}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        GoSendMail ->
            if model.sendMail == "" then
            ({model | cannotsend = "이메일 주소를 입력 해 주세요."}, Cmd.none)
            else
            ({model | cannotsend = ""}, mailEncoder model.sendMail model.session)
        NoOp ->
            ( model, Cmd.none )
        SendMail mail ->
            ({model | sendMail = mail}, Cmd.none)
        MailSendComplete (Ok ok) ->
            ({model | nextStep = "next"}, Cmd.none)
        MailSendComplete (Err err) ->
            (model, Cmd.none)
        AuthInput str ->
            ({model | authNumber = str}, Cmd.none)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.check then 
      div [] [ 
                if model.nextStep == "" then 
                div[][appHeaderRDetail  "비밀번호 찾기" "myPageHeader" Route.Home "fas fa-angle-left"
                   ,layout model]
                
                else
                div[][appHeaderRDetail  "비밀번호 찾기" "myPageHeader" Route.Home "fas fa-angle-left"
                   ,appsecondStep model]

               
            ]
        else 
          div [] [
                if model.nextStep == "" then
                forgotpwdstep1 model
                else
                secondStep  model
            ]
    }

forgotpwdstep1 model =
   div [ class "containerwrap" ]
    [ div [ class "container is-fullhd" ]
        [ div [ class "notification yf_workout" ]
            [ div [ class "titlewrap" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-full pagename" ]
                        [ div [ class "yficon" ]
                            [ img [src "../image/icon_signup.png", alt "icon" ]
                                []
                            ]
                        , div [ class "yftext" ]
                            [ strong []
                                [ text "비밀번호찾기" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "yf_box serpwdbox" ]
                [ img [ src "../image/lockimage.png" ]
                    []
                , div [ class "yf_textbox" ]
                    [ div [ class "yf_password" ]
                        [ p [ class "yf_passbox" ]
                            [ 
                                if model.cannotsend == "" then
                                div [] [
                                    text "유어핏에서 아이디로 사용중인 이메일 주소를 입력하시면 인증번호가 발송됩니다." 
                                    , br [][]
                                    , text "인증번호를 통해 기존의 비밀번호를 변경하시기 바랍니다." 
                                ]
                                else 
                                div [class "colorRed"] [text model.cannotsend]
                            ]
                        , p [ class "control has-icons-left yf_mail_send" ]
                            [ input [ class "input mail_send", type_ "email", placeholder "이메일을 입력해주세요", onInput SendMail, value model.sendMail ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-envelope" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "button is-link yf_sendbtn", onClick GoSendMail ]
                        [ text "이메일 인증번호 받기" ]
                    ]
                ]
            ]
        ]
    ]

secondStep model = 
   div [ class "containerwrap" ]
    [ div [ class "container is-fullhd" ]
        [ div [ class "notification yf_workout" ]
            [ div [ class "titlewrap" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-full pagename" ]
                        [ div [ class "yficon" ]
                            [ img [ src "../image/icon_signup.png", alt "icon" ]
                                []
                            ]
                        , div [ class "yftext" ]
                            [ strong []
                                [ text "비밀번호찾기" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "yf_box serpwdbox" ]
                [ img [ src "../image/lockimage.png" ]
                    []
                , div [ class "yf_textbox" ]
                    [ div [ class "yf_password" ]
                        [ p [ class "yf_passbox" ]
                            [ 
                                if model.cannotsend == "" then
                                    div [][
                                        text "입력하신 이메일로 인증번호를 발송했습니다." 
                                        , br [] [] 
                                        , text "인증번호와 이메일 계정을 입력하시면 임시 비밀번호가 발송됩니다."
                                    ] 
                                else
                                    div [class "colorRed"] [text model.cannotsend]
                            ]
                        , p [ class "control has-icons-left yf_mail_send" ]
                            [ 
                                input [class "input mail_send", type_ "email", placeholder "유어핏 계정을 입력해 주세요.", onInput (FindPwd "mail"), value model.yfEmail ][]
                                , input [ class "input mail_send", type_ "text", placeholder "인증번호를 입력해주세요", onInput (FindPwd "auth") , value model.authNumber ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-align-left" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "button is-info yf_sendbtn", href "../index.html" , onClick TemporaryPwd]
                        [ text "인증번호 입력하기" ]
                    ]
                ]
            ]
        ]
    ]

layout model = 
 div [ class "m_findpassbox" ]
                [ img [ src "../image/lockimage.png" ]
                    []
                , div [ class "" ]
                    [ div [ class "" ]
                        [ p [ class "m_yf_passbox" ]
                            [ 
                                if model.cannotsend == "" then
                                div [] [
                                    text "유어핏에서 아이디로 사용중인 이메일 주소를 입력하시면 인증번호가 발송됩니다." 
                                    , br [][]
                                    , text "인증번호를 통해 기존의 비밀번호를 변경하시기 바랍니다." 
                                ]
                                else 
                                div [class "colorRed"] [text model.cannotsend]
                            ]
                        , p [ class "control has-icons-left yf_m_mail_send" ]
                            [ input [ class "input mail_send", type_ "email", placeholder "이메일을 입력해주세요", onInput SendMail, value model.sendMail ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-envelope" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "button is-link yf_sendbtn", onClick GoSendMail ]
                        [ text "이메일 인증번호 받기" ]
                    ]
                ]


appsecondStep model = 
 div [ class "m_findpassbox" ]
                [ img [ src "../image/lockimage.png" ]
                    []
                , div [ class "" ]
                    [ div [ class "" ]
                        [ p [ class "m_yf_passbox" ]
                            [ 
                                if model.cannotsend == "" then
                                div [] [
                                      text "입력하신 이메일로 인증번호를 발송했습니다." 
                                    , br [][]
                                    ,  text "인증번호와 이메일 계정을 입력하시면 임시 비밀번호가 발송됩니다."
                                ]
                                else 
                                div [class "colorRed"] [text model.cannotsend]
                            ]
                        , p [ class "control has-icons-left yf_m_mail_send" ]
                            [ 
                                input [class "input mail_send2", type_ "email", placeholder "유어핏 계정을 입력해 주세요.", onInput (FindPwd "mail"), value model.yfEmail ][]
                                , input [ class "input mail_send", type_ "text", placeholder "인증번호를 입력해주세요", onInput (FindPwd "auth") , value model.authNumber ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-align-left" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "button is-info yf_sendbtn", href "../index.html" , onClick TemporaryPwd]
                        [ text "인증번호 입력하기" ]
                    ]
                ]
