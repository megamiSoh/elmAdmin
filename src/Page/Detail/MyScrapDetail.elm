module Page.Detail.MyScrapDetail exposing (..)

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
import Page.Detail.YourFitDetail as YfD
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
    , zindex : String
    , videoId : CodeId
    }
-- Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
type alias GetData = 
    { data : DetailData }

type alias CodeId = 
    { code : String
    , id : String }

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
    , thumbnail : String
    , description : Maybe String}

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
        , zindex = ""
        , videoId = 
            { code = ""
            , id = ""}
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
            , thumbnail = ""
            , description = Nothing}
        }
        , Api.getId ()
    )
subscriptions :Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ 
        Api.receiveId ReceiveId
        , Api.videoSuccess Loading
        , Session.changes GotSession (Session.navKey model.session)
        , Api.videoWatchComplete VideoEnd
        ]
type Msg 
    = BackPage
    -- | CheckDevice Encode.Value 
    | ReceiveId Encode.Value
    | GetListData (Result Http.Error GetData)
    | Loading Encode.Value
    | GoVideo
    | GotSession Session
    -- | Scrap Int
    -- | ScrapComplete (Result Http.Error Decoder.Success)
    | BackDetail
    | VideoCall (List Pairing)
    | VideoEnd Encode.Value
    | VideoRecordComplete (Result Http.Error Decoder.Success)
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VideoRecordComplete (Ok ok) ->
            (model, Cmd.none)
        VideoRecordComplete (Err err) ->
            (model, Cmd.none)
        VideoEnd complete ->
            let
                decodestr = Decode.decodeValue Decode.string complete
            in
                case decodestr of
                    Ok ok ->
                        (model, Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId.id)  (Session.cred model.session) Decoder.resultD)
                
                    Err err ->
                        (model, Cmd.none)
        VideoCall pairing ->
            let
                pEncode p = 
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)]
                pList = 
                    Encode.list pEncode pairing

            in
            
            ({model | zindex = "zindex"}, Api.videoData pList)
        GotSession session ->
            ({model | session = session},
            Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
            |> Api.get GetListData (Endpoint.scrapDetail model.videoId.code model.videoId.id ) (Session.cred session) 
            )
        BackDetail ->
            ({model | need2login = False}, 
            Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
             |> Api.get GetListData (Endpoint.scrapDetail model.videoId.code model.videoId.id ) (Session.cred model.session) 
            )

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
             (model, Cmd.none)
        GetListData (Ok ok) -> 
            update GoVideo {model | listData = ok.data, scrap = False, loading = False}
        GetListData (Err err) -> 
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
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
                d = 
                    Decode.decodeValue (Decoder.codeId CodeId) id
            in
            case d of
                Ok item ->
                    ({model | videoId = item}, 
                    Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
                    |>Api.get GetListData (Endpoint.scrapDetail item.code item.id ) (Session.cred model.session) 
                    )
            
                Err _ ->
                    (model, Cmd.none)
            
        BackPage ->
            ( model, 
            Route.pushUrl (Session.navKey model.session) Route.MyScrap
            -- -- Api.historyUpdate (Encode.string "myScrap")
            )

view : Model -> {title : String , content : Html Msg}
view model =
    -- if model.check then
    --     if model.loading then
    --         { title = "내 스크랩"
    --         , content = 
    --             div [] [
    --                     div [class "spinnerBack"] [spinner]
    --             ]
            
    --         }
    --     else
    --         { title = "내 스크랩"
    --         , content = 
    --             div [] [app model
    --             ]
            
    --         }
    -- else
        { title = "내 스크랩"
        , content = 
            div [] [
                    web BackPage model
            ]
        
        }
app model back videoCall= 
        div [ class "container" ]
                [
                   appHeaderRDetailClick model.listData.title  "myPageHeader whiteColor" back "fas fa-times"
                   , div [] [
                        --  if model.need2login then
                        -- need2loginAppDetail BackDetail
                        -- else
                        appcontentsItem model.listData model.zindex videoCall
                     ]
                ]

web msg model= 
    div [ class "container" ]
                [
                    -- appHeaderRDetailClick  model.listData.title  "yourfitHeader" BackPage "fas fa-times"  
                    -- ,
                    contentsBody model.listData model.loading model.zindex
                    ,goBtnBox msg
                ]
contentsBody item loading zindex=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item loading zindex
               
            ]
        
        ]

contentsItem item loading zindex =
            div [ class "tile is-parent is-vertical" ]
            [lazy2 div [ class "yf_notification" ]
                [ p [ class "video_title" ]
                    [ 
                    --    div [class ("imagethumb " ++ zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") , onClick (GoVideo item.pairing)] [] ,
                    --          videoCall


                         div [ class ("imagethumb " ++ zindex),style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") , onClick (VideoCall item.pairing) ][]
                        -- img [class zindex, src item.thumbnail, onClick (VideoCall item.pairing)] []
                        , div [ id "myElement", style "height" (if String.isEmpty zindex then "0px" else "auto") ] [
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
                    [ text ((justok item.exercise_part_name) ++ " - " ++  (justok item.difficulty_name)) ]
                ]
            , pre [class"yf_explanation descriptionBackground"] [text (justok item.description)]
            , div [ class "yf_text" ]
               (List.indexedMap description item.exercise_items)
            ]


justok casees = 
    case casees of
        Just a ->
            a
    
        Nothing ->
            "-"

appcontentsItem item zindex videoCall = 
            div []
            [ div []
                [ p [ class "m_yf_container" ]
                    [ div [ class ("appimagethumb " ++ zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") "), onClick (videoCall item.pairing) ][],
                         div [ id "myElement" ] [
                            ]
                    ]
                ]
            , 
            div [ class "m_yf_work_textbox" ]
                [ div [ class "m_yf_work_time" ]
                    [ span []
                        [ i [ class "fas fa-clock m_yf_timeicon" ]
                            []
                        ], text item.duration
                    ]
                , div [ class "m_yf_work_text" ]
                    [ text ((justok item.exercise_part_name) ++ " - " ++  (justok item.difficulty_name)) ]
                ]
            , pre [class"wordBreak descriptionBackground"][text (justok item.description)]
            , div [ class "m_work_script" ]
                (List.indexedMap YfD.description item.exercise_items)
            ]

description idx item = 
    ul [] [
            if item.is_rest then    
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "분")]
            else 
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "세트")] 
    ]
    
goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]

