module Page.Login exposing (..)

import Api as Api exposing (Cred)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, value, type_, src, value, alt)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import Route exposing (Route)
import Session exposing (Session)
import Api.Endpoint as Endpoint
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Page.Common exposing(..)

type alias Model =
    { session : Session
    , problems : List Problem
    , err: String
    , form : Form
    , token : String
    , tstr : Str
    , check : Bool
    , loginState : String
    }

type alias LoginState = 
    {error : String}


type Problem
    = InvalidEntry ValidatedField String


type alias Form =
    { email : String
    , password : String
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
    ( { session = session
    , check = mobile
    , loginState = ""
      , problems = []
      , err =""
      , token =""
      , form =
            { email = ""
            , password = ""
            }
      , tstr = 
            { token = ""}
      }
    , Cmd.none
    )



-- VIEW






type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error Cred)
    | GotSession Session
    | KeyDown Int

type alias Str =  {
    token : String
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            if key == 13 then
                update SubmittedForm model
            else
                (model, Cmd.none)
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , login validForm
                    )

                Err problems ->
                    
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let 
                serverErrors =
                    Api.decodeErrors error
            in
            if serverErrors == "badbody" then
                ( { model | err = "이메일 주소 또는 비밀번호를 다시 확인 해 주세요." }
            , Cmd.none
            )
            else
            ( { model | err = serverErrors }
            , Cmd.none
            )

        CompletedLogin (Ok viewer) ->
            ( model, Cmd.batch[Api.storeCredWith (viewer)] )



        GotSession session ->
            ( { model | session = session }
            , 
            Cmd.batch[Api.refreshFetchData (),  Route.replaceUrl (Session.navKey model.session) Route.Home]
            )

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )





    

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Session.changes GotSession (Session.navKey model.session)]


-- FORM



type TrimmedForm
    = Trimmed Form


type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "올바른 이메일 주소를 입력 해 주세요." ]

                else if (form.email |> String.split ("@") |> List.length) /= 2  then
                    ["올바른 이메일 주소를 입력 해 주세요."]
                else
                    []
            Password ->
                if String.isEmpty form.password then
                    [ "비밀번호를 입력 해 주세요." ]

                else
                    []



trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


-- login : TrimmedForm -> Http.Request Viewer
login (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "username", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        body =
            user
                |> Http.jsonBody
    in
    Api.login body CompletedLogin tokenDecoder 

tokenDecoder =
    Decode.succeed Api.Cred
        |> required "token" Decode.string

-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)


view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
    { title = "로그인"
    , content = 
        div [] [
                    appHeaderRDetail  "로그인" "myPageHeader" Route.Home "fas fa-angle-left",
                    layout model ]
    }
    else
    { title = "로그인"
    , content = 
        div [] [
                layout model
        ]   
    }


layout model = 
    div [ class "loginwrap" ]
    [ div [ class "yf_contents" ]
        [ div [ class "login_logo" ]
            [ img [ src "../image/login_logo.png", alt "logo" ]
                []
            ]
        , div [class "login_text1"] [text model.err]
        , div [ class "yf_login" ]
            [ p [ class "control has-icons-left" ]
                [ input [ class "input yf_loginput", type_ "email", placeholder "이메일을 입력해주세요", onInput EnteredEmail ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-envelope" ]
                        []
                    ]
                ]
            ]
            , div [class "login_text2"] (List.map (\x ->
                let
                    errorMessage =
                        case x of
                            InvalidEntry _ str ->
                                str
                in
                if errorMessage == "올바른 이메일 주소를 입력 해 주세요." then
                div [] [ text errorMessage ]
                else 
                div [] []
            ) model.problems)
        , div [ class "yf_login" ]
            [ p [ class "control has-icons-left" ]
                [ input [ class "input yf_loginput", type_ "password", placeholder "비밀번호를 입력해주세요", onInput EnteredPassword ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-lock" ]
                        []
                    ]
                ]
            ]
        , div [] (List.map (\x ->
                let
                    errorMessage =
                        case x of
                            InvalidEntry _ str ->
                                str
                in
                if errorMessage == "비밀번호를 입력 해 주세요." then
                div [class "login_text2"] [ text errorMessage ]
                else 
                div [] []
            ) model.problems)
        , div [ class "yfloginbox" ]
            [ ul [ class "yf_login_ul" ]
                [ li [ class "yf_login_li" ]
                    [ div [ class "button emailloginbtn", onClick SubmittedForm ]
                        [ text "로그인하기" ]
                    ]
                , li [class "yf_login_li"]
                    [ a [ class "button emailsignup", Route.href Route.Signup]
                        [ text "이메일로 가입하기" ]
                    ]
                 , li [class "yf_login_li"]
                    [ a [ class "button emailsignup", Route.href Route.FPwd]
                        [ text "비밀번호 찾기" ]
                    ]
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
        , div [ class "m_login_logo" ]
            [ img [ src "../image/login_logo.png", alt "logo" ]
                []
            ]
                , div [] [text model.err]
                , p [ class "control has-icons-left m_login_yf_inputbox" ]
                 [ input [onKeyDown KeyDown, class "input m_sign_input", type_ "email", placeholder "이메일을 입력해주세요" , onInput EnteredEmail  ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ class "fas fa-envelope sign_icon" ]
                                []
                            ]
                        ]
                     , div [] 
                     (List.map (\x ->
                            let
                                errorMessage =
                                    case x of
                                        InvalidEntry _ str ->
                                            str
                            in
                            if errorMessage == "올바른 이메일 주소를 입력 해 주세요." then
                                div [] [ text errorMessage ]
                            else 
                                div [] []
                        ) model.problems)
                    , p [ class "control has-icons-left m_login_yf_inputbox" ]
                        [ input [onKeyDown KeyDown, class "input m_sign_input", type_ "password", placeholder "비밀번호를 입력해주세요", onInput EnteredPassword ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ class "fas fa-lock sign_icon" ]
                                []
                            ]
                        ]
                         , div [] 
                         (List.map (\x ->
                            let
                                errorMessage =
                                    case x of
                                        InvalidEntry _ str ->
                                            str
                            in
                            if errorMessage == "비밀번호를 입력 해 주세요." then
                                div [] [ text errorMessage ]
                            else 
                                div [] []
                        ) model.problems)
                            , div [ class "m_yfloginbox" ]
            [ ul [ class "m_yf_login_ul" ]
                [ li [ class "yf_login_li" ]
                    [ 
                        input [ type_ "submit",  class "button m_emailloginbtn" , onClick SubmittedForm, value "로그인하기"] [ text "로그인하기" ]
                        -- , div [ class "button m_emailloginbtn", onClick SubmittedForm ]
                        -- [ text "로그인하기" ]
                    ]
                , li [class "yf_login_li"]
                    [ a [ class "button m_emailsignup", Route.href Route.Signup]
                        [ text "이메일로 가입하기" ]
                    ]
                 , li [class "yf_login_li"]
                    [ a [ class "button m_emailsignup", Route.href Route.Signup]
                        [ text "비밀번호 찾기" ]
                    ]
                ]
            ]

                
            ]

    

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     P.check CheckDevice