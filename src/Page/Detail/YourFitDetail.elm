module Page.Detail.YourFitDetail exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(Session)
import Html exposing (..)
import Page.Common exposing(..)
import Route exposing(..)
import Port as P
import Json.Decode as Decode
import Api.Endpoint as Endpoint
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Json.Encode as Encode
import Json.Decode as Decode exposing(..)
import Html.Lazy exposing (lazy, lazy2)
type alias Model =
    { session : Session
    , checkDevice : String
    , listData : DetailData
    , check : Bool
    , loading : Bool
    }
-- Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
type alias GetData = 
    { data : DetailData }

type alias DetailData =    
    { difficulty_name : Maybe String
    , duration : String
    , exercise_items : List DetailDataItem
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List Pairing
    , title : String}

type alias DetailDataItem = 
    { exercise_id : Int
    , is_rest : Bool
    , sort : Int
    , title : String
    , value : Int}

type alias Pairing = 
    { file : String
    , image : String
    , title : String}

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , loading = True
        , listData = 
            { difficulty_name = Nothing
            , duration = ""
            , exercise_items = []
            , exercise_part_name = Nothing
            , id = 0
            , inserted_at = ""
            , pairing = []
            , title = ""}
        }
        , Api.getId ()
    )
subscriptions :Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ P.check CheckDevice
        , Api.receiveId ReceiveId
        , Api.videoSuccess Loading
        ]
type Msg 
    = BackPage
    | CheckDevice Encode.Value 
    | ReceiveId Encode.Value
    | GetListData (Result Http.Error GetData)
    | Loading Encode.Value
    | GoVideo
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoVideo ->
            let
                videoList = 
                    Encode.object 
                        [("pairing", (Encode.list videoEncode) model.listData.pairing) ]

                videoEncode p=
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)
                        ]
            in
            (model, Api.videoData videoList)
        GetListData (Ok ok) -> 
           
            update GoVideo {model | listData = ok.data}
        GetListData (Err err) -> 
            (model, Cmd.none)
        Loading success ->
            let
                d = Decode.decodeValue Decode.string success
            in
                case d of
                    Ok item ->
                        ({model | loading = False},Cmd.none)
                        -- (model, Cmd.none)
                
                    Err _->
                         ({model | loading = False},Cmd.none)
        ReceiveId id ->
            let
                d = Decode.decodeValue Decode.string id
            in
            case d of
                Ok item ->
                    (model, Api.get GetListData (Endpoint.yfDetailDetail item ) (Session.cred model.session) (Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing))
            
                Err _ ->
                    (model, Cmd.none)
            
        BackPage ->
            ( model, 
            Route.backUrl (Session.navKey model.session) 1)
        CheckDevice str ->
           let 
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ -> 
                        ({model | checkDevice = "pc"}, Cmd.none)


view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
            if model.check then
                
                app model
            else
                web BackPage model
        
    }
app model= 
        div [ class "container" ]
                [
                   appHeaderBDetail model.listData.title  "yourfitHeader" BackPage
                   ,lazy2 appcontentsItem model.listData model.loading
                ]

web msg model= 
    div [ class "container" ]
                [
                    commonHeader "image/icon_workout.png" "유어핏운동"
                    ,
                    contentsBody model.listData model.loading
                    ,goBtnBox msg
                ]
contentsBody item model=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model
               
            ]
        
        ]

contentsItem item loading=
            div [ class "tile is-parent is-vertical" ]
            [lazy2 div [ class "yf_notification" ]
                [ p [ class "title" ]
                    [ 
                         div [ id "myElement" ] [
                            ]
                    ]
                ], 

            if loading then 
            spinner
            else
            div [ class "yf_subnav" ]
                [ div [ class "yf_time" ]
                    [ span []
                        [ i [ class "fas fa-clock" ]
                            []
                        ], text item.duration
                    ]
                , div [ class "yf_part" ]
                    [ text ((justok item.exercise_part_name) ++ " - " ++  (justok item.difficulty_name)) ]
                , div [ class "yf_scrapt" ]
                    [ span []
                        [ i [ class "far fa-bookmark" ]
                            []
                        ], text "스크랩" 
                    ]
                ]
            , 
            if loading then 
            div [] []
            else
            div [ class "yf_text" ]
               (List.indexedMap description item.exercise_items)
            ]


justok casees = 
    case casees of
        Just a ->
            a
    
        Nothing ->
            "-"

appcontentsItem item loading= 
            div [ ]
            [ div []
                [ p [ class "m_yf_container" ]
                    [ 
                        div [ id "myElement" ] [
                            ]
                    ]
                ]
            , 
            if loading then 
            spinner
            else
            div [ class "m_yf_work_textbox" ]
                [ div [ class "m_yf_work_time" ]
                    [ span []
                        [ i [ class "fas fa-clock m_yf_timeicon" ]
                            []
                        ], text item.duration
                    ]
                , div [ class "m_yf_work_text" ]
                    [ text ((justok item.exercise_part_name) ++ " - " ++  (justok item.difficulty_name)) ]
                , div [ class "m_yf_scrapt" ]
                    [ span []
                        [ i [ class "far fa-bookmark" ]
                            []
                        ]
                    ]
                ]
            , 
            if loading then 
            div [][]
            else
            div [ class "m_work_script" ]
                (List.indexedMap description item.exercise_items)
            ]

description idx item = 
    ul [] [
        li [] [text ((String.fromInt(idx + 1)) ++ " . " ++  item.title)]
        -- li [] [text]
    ]
    
goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]

