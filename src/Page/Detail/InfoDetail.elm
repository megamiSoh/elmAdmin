module Page.Detail.InfoDetail exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route exposing(..)
import Port as P
import Json.Decode as Decode
import Json.Encode as E
import Api as Api
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Http as Http
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
import Page.InfoEditor exposing (..)


defaultOptions =
    { softAsHardLineBreak = False
    , rawHtml = ParseUnsafe
    }

type alias Model 
    = {
        session : Session
        , textarea : String
        , checkDevice: String
        , check : Bool
        , showToC : Bool
        , data : DetailData
        , onDemandText : String
        , options : Markdown.Config.Options
        , selectedPreviewTab : PreviewTab

    }

type alias Data =
    { data : DetailData }
type alias DetailData = 
    { content : String
    , id : Int
    , title : String }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = ""
        , check = mobile
        , textarea = ""
        , showToC = False
        , onDemandText = ""
        , options = defaultOptions
        , selectedPreviewTab = RealTime
        , data = 
            { content = ""
            , id = 0
            , title = "" }
    }
        , Api.getId ()
    )

type EditorTab
    = Editor


type PreviewTab
    = RealTime

type Msg 
    = CheckDevice E.Value
    | GetDetail (Result Http.Error Data)

subscriptions : Model -> Sub Msg
subscriptions model =
    Api.receiveId CheckDevice

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDetail (Ok ok)->
            ({model | data = ok.data, textarea = ok.data.content}, Cmd.none)
        GetDetail (Err err)->
            (model, Cmd.none)
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model | checkDevice = string}, Api.get GetDetail (Endpoint.detailInfo string) (Session.cred model.session) (Decoder.detailInfo Data DetailData))
                
                    Err _ ->
                        ({model | checkDevice = ""}, Cmd.none)
            

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
    if model.check then
    app model.data model
    else
    web model.data model
    }

web data model= 
    div [class "container"] [
        commonJustHeader "/image/icon_notice.png" "공지사항" ,
        contentsBox data model,
        backBox
    ]
app data model =
    div [class "container"] [
        appHeaderRDetail (
            if String.length( data.title ) > 10 then
                (String.dropRight (String.length data.title - 9) data.title) ++ "..."
            else 
            data.title
        ) "myPageHeader colorWhite" Route.Info "fas fa-times",
        appContentsBox data model
    ]
contentsBox item model = 
    div [ class "info_mediabox" ]
        [ div [ class "infoDetail_titlebox" ]
            [ div [ class "infoDetail_title" ]
                [ text item.title  ]
            -- , div [ class "infoDetail_yf_date" ]
            --     [ text item.createDate ]
            ]
        , div [ class "infoDetail_textbox" ]
            [ 
                markdownView model
            ]
        ]

appContentsBox item model= 
    div [ class "mediabox" ]
        [ div [ class "titlebox" ]
            [ 
                --  div [ class "m_infoDetail_yf_date" ]
                -- [ text item.createDate ]
            ]
        , div [ class "m_infoDetail_textbox" ]
            [ 
                markdownView model
            ]
        ]

backBox = 
    div [ class "make_yf_butbox" ]
        [ a [ class "button infoDetail_yf_back", Route.href Route.Info]
            [ text "뒤로" ]
        ]

