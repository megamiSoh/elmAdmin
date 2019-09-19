module Page.EmptyPage exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Route exposing (..)

type alias Model 
    = {
        session : Session
        , check : Bool
    }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , check = mobile }
        , Cmd.none
    )

type Msg 
    = NoOp

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

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
        { title = "empty"
        , content = 
            div [] [
                    app
            ]
        }
    else
    { title = "empty"
    , content = 
        div [] [
            web
        ]
    }

app : Html Msg
app = 
    div [] [
            p [] [text "로그인 후 이용 가능 한 메뉴 입니다."]
            , a [ Route.href Route.Login ] [text "로그인 및 회원 가입 페이지로 이동"]
            , a [ Route.href Route.Home ] [text "홈으로 이동"]
            ]

web : Html Msg
web =
     div [] [
            p [] [text "로그인 후 이용 가능 한 메뉴 입니다."]
            , a [ Route.href Route.Login ] [text "로그인 및 회원 가입 페이지로 이동"]
            , a [ Route.href Route.Home ] [text "홈으로 이동"]
            ]