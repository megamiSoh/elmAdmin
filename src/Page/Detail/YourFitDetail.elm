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
    , zindex : String
    , video : String
    , errType : String
    }

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
    , thumbnail : String
    , description: Maybe String}

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

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , scrap = False
        , loading = True
        , need2login = False
        , videoId = ""
        , zindex = ""
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
        , video = ""
        , errType = ""
        }
        , Cmd.batch [
            Api.getId ()
            , mydata session
        ]
    )
subscriptions :Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Api.receiveId ReceiveId
        , Session.changes GotSession (Session.navKey model.session)
        , Api.videoSuccess Loading
        , Api.videoWatchComplete VideoEnd
        ]

mydata : Session -> Cmd Msg
mydata session = 
    Decoder.sessionCheckMydata
        |> Api.get MyInfoData Endpoint.myInfo (Session.cred session)

type Msg 
    = BackPage
    | ReceiveId Encode.Value
    | GetListData (Result Http.Error GetData)
    | Loading Encode.Value
    | GoVideo (List Pairing)
    | Scrap
    | ScrapComplete (Result Http.Error Decoder.Success)
    | BackDetail
    | GotSession Session
    | MyInfoData (Result Http.Error Decoder.DataWrap)
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
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "record"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        VideoEnd complete ->
            let
                decodestr = Decode.decodeValue Decode.string complete
            in
                case decodestr of
                    Ok ok ->
                        (model, Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred model.session) Decoder.resultD)
                
                    Err err ->
                        (model, Cmd.none)
        MyInfoData (Ok ok) ->
            (model, Cmd.none) 
        MyInfoData (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "myInfo"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "record" ->
                    Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred session) Decoder.resultD
                "myInfo" -> 
                    mydata session
                "scrap" ->
                    Decoder.resultD
                    |> Api.get ScrapComplete (Endpoint.scrap model.videoId)(Session.cred session)
                "getList" ->
                    Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
                        |>Api.get GetListData (Endpoint.yfDetailDetail model.videoId ) (Session.cred session)
                _ ->
                    Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
                        |>Api.get GetListData (Endpoint.yfDetailDetail model.videoId ) (Session.cred session)
            )
        BackDetail ->
            ({model | need2login = False}, 
            (Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing)
            |> Api.get GetListData (Endpoint.yfDetailDetail model.videoId ) (Session.cred model.session) 
            )
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
                ({model | need2login = True, errType = "scrap"}, (Session.changeInterCeptor(Just error)model.session))
            else
                (model, Api.showToast cannotScrap)
        Scrap ->
            (model, 
            Decoder.resultD
            |> Api.get ScrapComplete (Endpoint.scrap model.videoId)(Session.cred model.session) )
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
             ({model | zindex = "zindex"}, Api.videoData videoList)
        GetListData (Ok ok) -> 
             ({model | listData = ok.data, scrap = False, loading = False}, Cmd.none)
        GetListData (Err err) -> 
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "getList" }, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        Loading success ->
            let
                d = Decode.decodeValue Decode.string success
            in
                case d of
                    Ok item ->
                        ({model | loading = False},Cmd.none)
                
                    Err _->
                         ({model | loading = False},Cmd.none)
        ReceiveId id ->
            let 
                d = Decode.decodeValue Decode.string id
            in
            case d of
                Ok item ->
                    ({model | videoId = item}, 
                    (Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing)
                    |>Api.get GetListData (Endpoint.yfDetailDetail item ) (Session.cred model.session) 
                    )
            
                Err _ ->
                    (model, Cmd.none)
            
        BackPage ->
            ( model, 
            Route.backUrl (Session.navKey model.session) 1)

view : Model -> {title : String , content : Html Msg}
view model =
        { title = "유어핏 운동"
        , content = 
            div [] [
                    web BackPage model
            ]   
        }


app model backpage scrap goVideo= 
        div [class ("container myaccountStyle " ++ if model.detailShow then "account yfdetailShow" else "")]
                [
                   appHeaderRDetailClick model.listData.title  "yourfitHeader" backpage "fas fa-times"
                   ,
                    appcontentsItem model.listData model.loading scrap model model.zindex goVideo
                    
                ]

web msg model= 
    div [ class "container" ]
                [
                    commonHeader2 "image/icon_workout.png" "유어핏운동"
                    , div [] [
                        if model.need2login then
                        need2loginAppDetail BackDetail
                        else
                        contentsBody model.listData model.loading Scrap model.scrap GoVideo "스크랩"model.zindex,
                        goBtnBox msg
                    ]
                ]
contentsBody item model scrap modelscrap goVideo scrapText zindex=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model scrap modelscrap goVideo scrapText zindex
               
            ]
        
        ]



contentsItem item loading scrap modelscrap govideo scrapText zindex=
            div [ class "tile is-parent is-vertical" ]
            [lazy2 div [ class "yf_notification" ]
                [ p [ class "video_title " ]
                    [ 
                            div [class ("imagethumb " ++ zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") ,onClick (govideo item.pairing)] []
                            , videoCall
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
                        ], text scrapText
                    ]
                ]
            , 
            pre [class"yf_explanation descriptionBackground"] [text (justok item.description)]
            , div [ class "yf_text" ]
               (List.indexedMap description item.exercise_items)
            ]


justok casees = 
    case casees of
        Just a ->
            a
    
        Nothing ->
            "  "

appcontentsItem item loading scrap modelscrap zindex goVideo = 
            div [ ]
            [  p [ class "m_yf_container" ]
                    [ 
                        div [ class ("appimagethumb " ++ zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") , onClick (goVideo item.pairing) ][]
                        , videoCall
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
            div [class"m_explanation"] [
                pre [class "descriptionBackground"] [text (justok item.description)]
                , div [ class "m_yfwork_script" ]
                (List.indexedMap description item.exercise_items)
            ]
            ]

description idx item = 
    ul [class "yf_text"] [
            if item.is_rest then    
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "분")]
            else 
                li [] [text ((String.fromInt(item.sort)) ++ " . " ++  item.title ++ " " ++ String.fromInt(item.value) ++ "세트")] 
                    
        -- li [] [text]
    ]
    
goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]


videoCall =
    div [id "myElement"] []