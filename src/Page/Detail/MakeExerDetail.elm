module Page.Detail.MakeExerDetail exposing(..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Page.Common exposing(..)
import Page.Detail.YourFitDetail as YfD
import Route exposing(..)
import Api as Api
import Api.Endpoint as Endpoint
import Http as Http
import Api.Decoder as Decoder

type alias Model = 
    { session : Session
    , check : Bool
    , checkDevice: String
    , getData : DetailData
    , loading : Bool
    , scrap : Bool
    , videoId : String
    , zindex : String
    , deleteAuth: String
    , errType : String
    }

type alias GetData = 
    { data : DetailData }

type alias DetailData =    
    { difficulty_name : Maybe String
    , duration : String
    , exercise_items : List YfD.DetailDataItem
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List Pairing
    , title : String
    , nickname : Maybe String
    , thumbnail : String
    , description : Maybe String}

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
        , loading = True
        , scrap = False
        , videoId = ""
        , zindex = ""
        , deleteAuth = ""
        , getData = 
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
        , errType = ""
        }
        , Cmd.batch 
        [  Api.getId ()
        , scrollToTop NoOp
        ]

        
    )

type Msg 
    = CheckDevice E.Value
    | BackPage
    | GetId E.Value
    | GetList (Result Http.Error GetData)
    | GoVideo (List Pairing)
    | Loading E.Value
    | Scrap Int
    | ScrapComplete (Result Http.Error Decoder.Success)
    | GotSession Session
    | SaveIdComplete E.Value
    | RecordComplete E.Value
    | VideoRecordComplete (Result Http.Error Decoder.Success)
    | NoOp

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Api.receiveId GetId
    , Api.videoSuccess Loading
    , Session.changes GotSession (Session.navKey model.session)
    , Api.successId SaveIdComplete
    , Api.videoWatchComplete RecordComplete ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
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
        RecordComplete str ->
            let
                decodestr = Decode.decodeValue Decode.string str
            in
                case decodestr of
                    Ok ok ->
                        (model, Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred model.session) Decoder.resultD)
                
                    Err err ->
                        (model, Cmd.none)
        SaveIdComplete str ->
            let
                decodestr = Decode.decodeValue Decode.string str
            in
            case decodestr of
                Ok ok ->
                   (model,
                   Route.pushUrl(Session.navKey model.session) Route.TogetherW
                   ) 
            
                Err _ ->
                    (model, Cmd.none)
        GotSession session ->
           ({model | session = session}, 
            case model.errType of
                "record" ->
                    Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred session) Decoder.resultD
                "scrap" ->
                    Cmd.none
                "list" ->
                    (Decoder.yfDetailDetail GetData DetailData YfD.DetailDataItem YfD.Pairing)
                        |> Api.get GetList (Endpoint.makeDetail model.videoId) (Session.cred session)  
                _ ->
                    (Decoder.yfDetailDetail GetData DetailData YfD.DetailDataItem YfD.Pairing)
                        |> Api.get GetList (Endpoint.makeDetail model.videoId) (Session.cred session)  
           )
        ScrapComplete (Ok ok) ->
            let
                text = E.string "스크랩 되었습니다."
            in
            
            ({model | scrap = not model.scrap}, Cmd.none)
        ScrapComplete (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "scrap"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        GoVideo pairing->
            let
                videoList = 
                    E.object 
                        [("pairing", (E.list videoEncode) pairing) ]

                videoEncode p=
                    E.object
                        [ ("file", E.string p.file)
                        , ("image", E.string p.image)
                        , ("title", E.string p.title)
                        ]
            in
            ({model | zindex = "zindex"}, Api.videoData videoList)
        GetList(Ok ok) ->
            ({model | getData = ok.data, loading = False}, Cmd.none)
        GetList(Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "list"}, (Session.changeInterCeptor(Just serverErrors)model.session))
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
        GetId id ->
            let
                result = Decode.decodeValue Decode.string id
            in
            case result of
                Ok string ->
                    ({model | videoId = string} , 
                    (Decoder.yfDetailDetail GetData DetailData YfD.DetailDataItem YfD.Pairing)
                    |> Api.get GetList (Endpoint.makeDetail string) (Session.cred model.session)  
                    )
            
                Err _ ->
                    (model,Cmd.none)
        
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model | checkDevice = string}, Cmd.none)
                    Err _ ->
                        ({model | checkDevice = "pc"}, Cmd.none)
        BackPage ->
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.MakeExer
            )
        Scrap id ->
            (model, Cmd.batch[Api.saveId (E.string (String.fromInt id))])
          

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
        { title = "맞춤운동"
        , content = 
            div [class "container"] [
                appHeaderConfirmDetail model.getData.title "makeExerHeader" Route.MakeExer "fas fa-times"  Route.EditFilter "수정"
                , appcontentsItem model.getData model GoVideo model.zindex
            ]
        }
    else
        { title = "맞춤운동"
        , content = 
            div [] [
                web BackPage model
            ]
        }


web : msg  -> Model -> Html Msg
web msg model= 
    div [class "container"] [
        commonHeader "/image/icon_customworkout.png" "맞춤운동" ,
        contentsBody model.getData model.loading Scrap model.scrap GoVideo "공유하기" model.zindex,
        goBtn BackPage 

    ]

app : msg -> Model -> Html Msg
app msg model= 
    div [class "container"] [
        appHeaderConfirmDetail model.getData.title "makeExerHeader" Route.MakeExer "fas fa-times"  Route.EditFilter "수정"
        , div [] [
            div [class "spinnerBack", style "display" (if model.loading then "inline-block" else "none")] [
                spinner
                ]
        ]
        , appcontentsItem model.getData model GoVideo model.zindex
    ]

goBtn : msg -> Html msg
goBtn back  = 
    div [ class "make_yf_butbox" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button yf_largebut", onClick back ]
                [ text "뒤로" ]
            ]
        , div [ class "yf_nextbtm" ]
            [ a [ class "button is-dark yf_editbut", Route.href Route.EditFilter]
                [ text "수정" ]
            ]
        ]


appcontentsItem : DetailData -> Model -> (List Pairing -> msg) -> String -> Html Msg
appcontentsItem item model goVideo zindex= 
            div [ ]
            [ div []
                [ p [ class "m_yf_container" ]
                     [ 
                         div [] [
                        div [class ("appimagethumb " ++ zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") , onClick (GoVideo item.pairing)] [] ,
                             videoCall
                         ]
                ]
            , div [ class "m_yf_work_textbox" ]
                [ div [ class "m_yf_work_time" ]
                    [ span []
                        [ i [ class "fas fa-clock m_yf_timeicon" ]
                            []
                        ], text item.duration
                    ]
                , div [ class "m_yf_work_text" ]
                    [ text (justokData item.exercise_part_name)
                    ,  text " "
                    ,  text (justokData item.difficulty_name) ]
                , div [ class "m_yf_scrapt", onClick (Scrap item.id) ]
                    [ span []
                        [ i [ class "far fa-bookmark" ]
                            []
                        ]
                    ]
                ]
             , pre [class"m_work_maketext descriptionBackground wordBreak"] [
                text (justokData item.description)    
                 ]
                
                , div [ class "m_yfwork_script" ]
                  (List.indexedMap YfD.description item.exercise_items)   
            ]
        ]


contentsBody : DetailData -> Bool ->  (Int -> msg) -> Bool -> (List Pairing -> msg) -> String -> String -> Html msg
contentsBody item model scrap modelscrap goVideo scrapText zindex=
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model scrap modelscrap goVideo scrapText zindex  
            ]        
        ]

contentsItem : DetailData -> Bool -> (Int -> msg) -> Bool -> (List Pairing -> msg) -> String -> String -> Html msg
contentsItem item loading scrap modelscrap govideo scrapText zindex =
            div [ class "tile is-parent is-vertical" ]
            [ div [ class "yf_notification" ]
                [ p [ class "video_title" ]
                    [ 
                         div [] [
                        div [class ("imagethumb " ++ zindex), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thumbnail ++") ") , onClick (govideo item.pairing)] [] ,
                             videoCall
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
                    [ text ((justokData item.exercise_part_name) ++ "  " ++  (justokData item.difficulty_name)) ]
                , div [ class "yf_scrapt", onClick (scrap item.id) ]
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
            
            , div [ class "yf_explanation" ]
            [
                pre [class "descriptionBackground wordBreak"] [ text (justokData item.description)]
                , div [ class "yf_text" ] (List.indexedMap YfD.description item.exercise_items)
            ]
            ]


justokData : Maybe String -> String            
justokData result = 
    case result of
        Just ok ->
            let
                replace = 
                    ok
                        |> String.replace "%26" "&"
                        |> String.replace "%25" "%"  
            in
                replace
        Nothing ->
            ""

videoCall : Html msg
videoCall = 
    div [id "myElement"] []