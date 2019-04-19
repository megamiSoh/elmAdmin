module Page.SetPwd exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Json.Encode as E
import Api.Endpoint as Endpoint
import Api as Api
import Api.Decoder as Decoder
import Http as Http
import Route as Route

type alias Model  = 
    { session : Session
    , check : Bool
    , oldPwd : String
    , rePwd : String
    , pwd : String
    , notMatchPwd : String
    }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    ({ session = session
    , check = mobile
    , oldPwd = ""
    , rePwd = ""
    , pwd = ""
    , notMatchPwd = ""}
    , Cmd.none
    )

type Msg 
    = NoOp
    | OldPwd String
    | RePwdInput String
    | PwdInput String
    | ChangePwd
    | PwdComplete (Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
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
        PwdComplete (Ok ok) ->
            let
                textEncode = E.string "변경되었습니다."
            in
            (model, Cmd.batch[Api.showToast textEncode, 
            Route.pushUrl (Session.navKey model.session) Route.MyPage
            -- Api.historyUpdate (E.string "myPage")
            ])
        PwdComplete (Err err) ->
            let
                serverError = Api.decodeErrors err
            in
            if serverError == "401" then
            (model, (Session.changeInterCeptor (Just serverError) model.session))
            else 
            ({model | notMatchPwd = "기존에 사용하시던 비밀번호를 다시 한번 확인 해 주세요."}, Cmd.none)


view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = div [] [
        setPwdSetting model
    ]}


setPwdSetting model = 
    div [ class "containerwrap" ]
    [ div [ class "container is-fullhd" ]
        [ div [ class "notification yf_workout" ]
            [ div [ class "titlewrap" ]
                [ div [ class "columns" ]
                    [ div [ class "column is-full pagename" ]
                        [ div [ class "yficon" ]
                            [ img [  src "../image/icon_signup.png", alt "icon" ]
                                []
                            ]
                        , div [ class "yftext" ]
                            [ strong []
                                [ text "비밀번호 재설정" ]
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
                            [ if model.notMatchPwd == "" then
                                text "유어핏에서 사용할 새로운 비빌번호를 설정해주세요."
                            else
                                div [class "colorRed"] [
                                    text model.notMatchPwd
                                ]
                             ]
                        , p [ class "control has-icons-left yf_mail_send" ]
                            [ input [ class "input mail_send", type_ "password", placeholder "기존의 비밀번호를 입력해주세요", onInput OldPwd ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-key" ]
                                    []
                                ]
                            ]
                        , p [ class "control has-icons-left yf_mail_send2" ]
                            [ input [ class "input mail_send", type_ "password", placeholder "새 비밀번호를 입력해주세요", onInput PwdInput ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-key" ]
                                    []
                                ]
                            ]
                        , p [ class "control has-icons-left yf_mail_send2" ]
                            [ input [ class "input mail_send", type_ "password", placeholder "새 비밀번호를 재입력해주세요", onInput RePwdInput ]
                                []
                            , span [ class "icon is-small is-left" ]
                                [ i [ class "fas fa-key" ]
                                    []
                                ]
                            ]
                        ]
                    , div [ class "button is-dark yf_sendbtn", onClick ChangePwd ]
                        [ text "새 비밀번호 수정하기" ]
                    ]
                ]
            ]
        ]
    ]