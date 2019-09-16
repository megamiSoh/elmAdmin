module Page.GateProgress exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Api as Api

type alias Model 
    = {
        session : Session
        , check : Bool
    }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , check = mobile}
        , Api.mobilePaymentCheck ()
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
    { title = ""
    , content =
        div [class "bar_container"][
            div [class "bar"][]
        ]
    }
