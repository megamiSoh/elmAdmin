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
import Html.Lazy exposing (lazy, lazy2, lazy4)

type alias Model =
    { session : Session
    , scrap : Bool
    , checkDevice : String
    , listData : DetailData
    , check : Bool
    , loading : Bool
    , need2login : Bool
    , videoId : String
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
    , title : String
    , nickname : Maybe String
    , thumbnail : String}

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
        , scrap = False
        , loading = True
        , need2login = False
        , videoId = ""
        , listData = 
            { difficulty_name = Nothing
            , duration = ""
            , exercise_items = []
            , exercise_part_name = Nothing
            , id = 0
            , inserted_at = ""
            , pairing = []
            , title = ""
            , nickname = Nothing
            , thumbnail = ""}
        }
        , Api.getId ()
    )
subscriptions :Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
        --     P.check CheckDevice
        -- , 
        Api.receiveId ReceiveId
        , Api.videoSuccess Loading
        ]
type Msg 
    = BackPage
    -- | CheckDevice Encode.Value 
    | ReceiveId Encode.Value
    | GetListData (Result Http.Error GetData)
    | Loading Encode.Value
    | GoVideo (List Pairing)
    | Scrap
    | ScrapComplete (Result Http.Error Decoder.Success)
    | BackDetail
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackDetail ->
            ({model | need2login = False}, Api.get GetListData (Endpoint.yfDetailDetail model.videoId ) (Session.cred model.session) (Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing))
        ScrapComplete (Ok ok) ->
            let
                text = Encode.string "스크랩 되었습니다."
            in
            
            ({model | scrap = not model.scrap}, Api.showToast text )
        ScrapComplete (Err err) ->
            let
                error = Api.decodeErrors err
                cannotScrap = Encode.string "이미 스크랩 되었습니다."
            in
            if error == "401" then
                ({model | need2login = True}, Cmd.none )
            else
                (model, Api.showToast cannotScrap)
        Scrap ->
            (model, Api.get ScrapComplete (Endpoint.scrap model.videoId)(Session.cred model.session) Decoder.resultD)
        GoVideo pairing->
            let
                videoList = 
                    Encode.object 
                        [("pairing", (Encode.list videoEncode) pairing) ]

                videoEncode p=
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)
                        ]
            in
            (model, Api.videoData videoList)
        GetListData (Ok ok) -> 
             ({model | listData = ok.data, scrap = False, loading = False}, Cmd.none)
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
                    ({model | videoId = item}, Api.get GetListData (Endpoint.yfDetailDetail item ) (Session.cred model.session) (Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing))
            
                Err _ ->
                    (model, Cmd.none)
            
        BackPage ->
            ( model, 
            Route.backUrl (Session.navKey model.session) 1)
        -- CheckDevice str ->
        --    let 
        --         result =
        --             Decode.decodeValue Decode.string str
        --     in
        --         case result of
        --             Ok string ->
        --                 ({model| checkDevice = string}, Cmd.none)
        --             Err _ -> 
        --                 ({model | checkDevice = "pc"}, Cmd.none)


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
                   appHeaderRDetailClick model.listData.title  "yourfitHeader" BackPage "fas fa-times"
                   ,
                    if model.loading then
                    div [class "spinnerBack"] [
                        spinner
                        ]
                    else 
                    div [] []
                     , if model.need2login then
                        need2loginAppDetail BackDetail
                    else
                    appcontentsItem model.listData model.loading Scrap model
                ]

web msg model= 
    div [ class "container" ]
                [
                    commonHeader "image/icon_workout.png" "유어핏운동"
                    , if model.need2login then
                        need2loginAppDetail BackDetail
                    else
                    contentsBody model.listData model.loading Scrap model.scrap GoVideo
                    ,goBtnBox msg
                ]
contentsBody item model scrap modelscrap goVideo=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model scrap modelscrap goVideo
               
            ]
        
        ]

contentsItem item loading scrap modelscrap govideo=
            div [ class "tile is-parent is-vertical" ]
            [lazy2 div [ class "yf_notification" ]
                [ p [ class "title" ]
                    [ 
                         div [] [
                             img [ src item.thumbnail , onClick (govideo item.pairing)] []
                            , div [ id "myElement" ] [
                            ]
                         ]
                    ]
                ], 
            div [ class "yf_subnav" ]
                [ div [ class "yf_time" ]
                    [ span []
                        [ i [ class "fas fa-clock" ]
                            []
                        ], text item.duration
                    ]
                , div [ class "yf_part" ]
                    [ text ((justok item.exercise_part_name) ++ "  " ++  (justok item.difficulty_name)) ]
                , div [ class "yf_scrapt", onClick scrap ]
                    [ span []
                        [ i [ class (
                            if modelscrap then
                            "fas fa-bookmark"
                            else
                            "far fa-bookmark"
                        ) ]
                            []
                        ], text "스크랩" 
                    ]
                ]
            , 
            -- if loading then 
            -- div [] []
            -- else
            div [ class "yf_text" ]
               (List.indexedMap description item.exercise_items)
            ]


justok casees = 
    case casees of
        Just a ->
            a
    
        Nothing ->
            "  "

appcontentsItem item loading scrap modelscrap= 
            div [ ]
            [ div []
                [ p [ class "m_yf_container" ]
                    [ 
                        img [src item.thumbnail, onClick (GoVideo item.pairing)] []
                        ,div [ id "myElement" ] [
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
                    [ text ((justok item.exercise_part_name) ++ "  " ++  (justok item.difficulty_name)) ]
                , div [ class "m_yf_scrapt", onClick scrap ]
                    [ span []
                        [ i [ class  (
                            if modelscrap.scrap then
                            "fas fa-bookmark"
                            else
                            "far fa-bookmark"
                        ) ]
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
            if item.is_rest then    
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "세트")]
            else 
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "분")] 
                    
        -- li [] [text]
    ]
    
goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]

