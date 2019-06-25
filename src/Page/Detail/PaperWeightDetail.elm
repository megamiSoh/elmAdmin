module Page.Detail.PaperWeightDetail exposing (..)

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
    , videoId : String
    , falseData : AskDetail
    , isActive : Bool
    , errType : String
    , product_no : Int
    }
-- Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
type alias GetData = 
    { data : Maybe DetailData }

-- type alias CodeId = 
--     { code : String
--     , id : String }

type alias DetailData =    
    { description : String
    , difficulty_name : String
    , duration : String
    , exercise_items : List DetailDataItem
    , exercise_part_name : String
    , id : Int
    , inserted_at : String
    , is_buy : Bool
    , pairing : List Pairing
    , product_no : Int
    , thumbnail : String
    , title : String
    }

type alias AskDetailData = 
    { data : AskDetail }

type alias AskDetail = 
    { description : String
    , difficulty_name : String
    , duration : String
    , exercise_id : Int
    , exercise_items : List AskDetailItem
    , exercise_part_name : String
    , product_no : Int
    , thumbnail : String
    , title : String
    , is_buy: Bool }


type alias AskDetailItem = 
    { exercise_id : Int
    , is_rest : Bool
    , sort : Int
    , title : String
    , value : Int }


type alias DetailDataItem = 
    { action_id : Maybe Int
    , difficulty_name : Maybe String
    , duration : String
    , exercise_id : Int
    , exercise_name : Maybe String
    , instrument_name : Maybe String
    , is_rest : Bool
    , mediaid : String
    , part_detail_name : List (Maybe String)
    , sort : Int
    , thembnail : String
    , title : String
    , value : Int }

type alias Pairing = 
    { file : String
    , image : String
    , title : String}

listDataInit =
    { description = ""
    , difficulty_name = ""
    , duration = ""
    , exercise_items = []
    , exercise_part_name = ""
    , id = 0
    , inserted_at = ""
    , is_buy = False
    , pairing = []
    , product_no = 0
    , thumbnail = ""
    , title = ""}

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
        , videoId = ""
        , listData =  listDataInit
        , falseData = 
            { description = ""
            , difficulty_name = ""
            , duration = ""
            , exercise_id = 0
            , exercise_items = []
            , exercise_part_name = ""
            , product_no = 0
            , thumbnail = ""
            , title = ""
            , is_buy= True }
        , isActive = False
        , errType = ""
        , product_no = 0
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
    | BackDetail
    | VideoCall (List Pairing)
    | VideoEnd Encode.Value
    | VideoRecordComplete (Result Http.Error Decoder.Success)
    | ClickRight
    | ClickLeft
    | GoAnotherPage
    | GetFalseData (Result Http.Error AskDetailData)
    | PayConfirm
    | ReRegistExercise Int
    | ProductComplete (Result Http.Error Decoder.Success)
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PayConfirm ->
            ({model | isActive = not model.isActive}, Cmd.none)
        ProductComplete (Ok ok) ->
            (model, Cmd.batch[Api.showToast (Encode.string "재 구매 되었습니다.")
            , Route.pushUrl (Session.navKey model.session) Route.MJList])
        ProductComplete (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            ({model | errType ="ProductComplete"}, Cmd.batch[(Session.changeInterCeptor (Just serverErrors) model.session)
            ])
        ReRegistExercise product_no ->
            let
                body = Encode.object
                    [ ("product_no", Encode.int product_no)]
                        |> Http.jsonBody
            in
            
            ({model | product_no = product_no}, Api.post Endpoint.renewWeekExercise (Session.cred model.session) ProductComplete body Decoder.resultD )
        GetFalseData (Ok ok) ->
            ({model | falseData = ok.data }, Cmd.none)
        GetFalseData (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            ({model | errType = "GetFalseData"}, Cmd.batch[(Session.changeInterCeptor (Just serverErrors) model.session)
            ])
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (Encode.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        VideoRecordComplete (Ok ok) ->
            (model, Cmd.none)
        VideoRecordComplete (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            ({model | errType = "VideoRecordComplete"}, Cmd.batch[(Session.changeInterCeptor (Just serverErrors) model.session)
            ])
        VideoEnd complete ->
            let
                decodestr = Decode.decodeValue Decode.string complete
            in
                case decodestr of
                    Ok ok ->
                        (model,
                        Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred model.session) Decoder.resultD
                        )
                
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
            let
                body = Encode.object
                    [ ("product_no", Encode.int model.product_no)]
                        |> Http.jsonBody
            in
            
            ({model | session = session}, 
            case model.errType of
                "ProductComplete" ->
                    Api.post Endpoint.renewWeekExercise (Session.cred model.session) ProductComplete body Decoder.resultD
            
                "GetFalseData" ->
                    Api.get GetFalseData (Endpoint.mypaperweightDetail model.videoId) (Session.cred model.session) (Decoder.myaskDetailData AskDetailData AskDetail AskDetailItem)
                "VideoRecordComplete" ->
                    Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred model.session) Decoder.resultD
                "GetListData" ->
                   Api.get GetListData (Endpoint.mypaperweightDetail model.videoId) (Session.cred model.session) (Decoder.detailMypaperweight GetData DetailData DetailDataItem Pairing)
                _ ->
                   Api.get GetListData (Endpoint.mypaperweightDetail model.videoId) (Session.cred model.session) (Decoder.detailMypaperweight GetData DetailData DetailDataItem Pairing)
            )
        BackDetail ->
            ({model | need2login = False}, Cmd.none
            -- Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
            --  |> Api.get GetListData (Endpoint.scrapDetail model.videoId.code model.videoId.id ) (Session.cred model.session) 
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
            update GoVideo {model | listData = 
            case ok.data of
                Just data ->
                    data
            
                Nothing ->
                    listDataInit
            , scrap = False, loading = False}
        GetListData (Err err) -> 
            let
                serverErrors = Api.decodeErrors err
            in
            case serverErrors of
                "401" ->
                    ({model | errType = "GetListData"}, (Session.changeInterCeptor(Just serverErrors)model.session))
                "badbody" ->
                    (model, 
                    Api.get GetFalseData (Endpoint.mypaperweightDetail model.videoId) (Session.cred model.session) (Decoder.myaskDetailData AskDetailData AskDetail AskDetailItem)
                    )
                _ ->
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
                    Decode.decodeValue Decode.string id
            in
            case d of
                Ok item ->
                    ({model | videoId = item}, 
                    Api.get GetListData (Endpoint.mypaperweightDetail item) (Session.cred model.session) (Decoder.detailMypaperweight GetData DetailData DetailDataItem Pairing)
                    -- Decoder.yfDetailDetail GetData DetailData DetailDataItem Pairing
                    -- |>Api.get GetListData (Endpoint.scrapDetail item.code item.id ) (Session.cred model.session) 
                    )
            
                Err _ ->
                    (model, Cmd.none)
            
        BackPage ->
            ( model, 
            Route.pushUrl (Session.navKey model.session) Route.MJList
            -- -- Api.historyUpdate (Encode.string "myScrap")
            )

view : Model -> {title : String , content : Html Msg}
view model =
        { title = "내 스크랩"
        , content = 
            div [] [
                    div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage False]
                    , web BackPage model
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
                    [ text ((item.exercise_part_name) ++ " - " ++  (item.difficulty_name)) ]
                ]
            , pre [class"wordBreak descriptionBackground"][text (item.description)]
            , div [ class "m_work_script" ]
                (List.indexedMap YfD.description item.exercise_items)
            ]
web msg model= 
    if model.falseData.is_buy then
    div [ class "container" ]
                [
                    contentsBody model.listData model.loading model.zindex model.listData.title
                    ,goBtnBox msg
                ]

    else
        selectedItem model
    
contentsBody item loading zindex title =
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text title ],
                contentsItem item loading zindex
               
            ]
        
        ]

contentsItem item loading zindex =
            div [ class "tile is-parent is-vertical" ]
            [lazy2 div [ class "yf_notification" ]
                [ p [ class "video_title" ]
                    [ 
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
                    [ text (( item.exercise_part_name) ++ " - " ++  ( item.difficulty_name)) ]
                ]
            , pre [class"yf_explanation descriptionBackground"] [text ( item.description)]
            , div [ class "yf_text" ]
               (List.indexedMap description item.exercise_items)
            ]


justok casees = 
    case casees of
        Just a ->
            a
    
        Nothing ->
            "-"


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


selectedItem model = 
        div [class "container"]
        [
             div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text "체험 기간 만료" ]
            ]
            , div[class "paperweightSelectedItem_first"][
            img [src model.falseData.thumbnail ][]
            , div []
            [ div [class "mj_title"][text model.falseData.title
            , span [class "mj_title_part"][text (model.falseData.exercise_part_name ++ " - " ++ model.falseData.difficulty_name)]
            ]
            , span [class "mj_title_duration"]
            [ i [ class "fas fa-stopwatch" , style "padding-right" "3px"] []
            , text model.falseData.duration
            ]
            ,ul [class "mj_description"]
             (List.map askDetailItems model.falseData.exercise_items)
            
            ]
        ]
        , div [class "paperweightSelectedItem_second"][
            h3 [][text "운동설명"]
            , div [class "description"][
                text model.falseData.description
            ]
        ]
        , div [class "paperweightSelectedItem_third"]
        [ div [class "button is-link"
        , onClick PayConfirm
        ][text "재 구매"]
        , div [class "button is-danger", onClick BackPage][text "뒤로"]
        ]
        , payConfirmLayer model
        ]



askDetailItems item = 
    li [][
        text ((String.fromInt item.sort) ++ ". " ++ item.title ++ " x " ++ String.fromInt item.value ++ (if item.is_rest then " 분 " else " 세트 "))
    ]


payConfirmLayer model =
    div [ class "layerStyleWarn", style "display" ( if model.isActive then "flex" else "none"), id ( if model.isActive then "noScrInput" else "") ] [
    div [ class "yf_popup" ]
    [ 
        i [ class "fas fa-cart-arrow-down" , style "font-size" "3rem"]
        []
    , h1 [ class "popup_yf_h1", style "font-size" "1rem" ]
        [ text "확인을 클릭하시면 운동영상이 구매되며, 구매한 시점을 기준으로 유효기간이 연장됩니다."]
    , p [ class "yf_logoout_butbox" ]
        [ div [ class "button is-danger logout_danger" 
        , onClick (ReRegistExercise model.falseData.product_no)
        ]
            [ text "확인"]
        , div [ class "button is-light logout_cencel", onClick PayConfirm ]
            [ text "취소" ]
        ]
    ]
    ]