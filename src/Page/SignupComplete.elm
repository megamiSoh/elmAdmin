module Page.SignupComplete exposing(..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Api as Api
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
    {
    
    title = "YourFitExer"
    , content = div [class "noResult"] [
        div [] [

             img [ src "image/sign_image.png", alt "sign_image" ][]
            --  text "회원가입이 완료 되었습니다."
        ]
        ,
        div [] [
             a [ class "button is-primary signupComplete_btn", Route.href Route.Home ] [text "홈으로 이동하기"]
        ]
    ]}
