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

type alias Model 
    = {
        session : Session
        , check : Bool
        , checkDevice: String
        , getData : DetailData
        , loading : Bool
        , scrap : Bool
        , videoId : String
        , deleteAuth: String
    }

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
    , thumbnail : String}

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
        , scrap = False
        , videoId = ""
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
            , thumbnail = ""}
        }
        , Cmd.batch 
        [  Api.getId ()
        ]
        
    )
    
-- 
type Msg 
    = CheckDevice E.Value
    | BackPage
    | GetId E.Value
    | GetList (Result Http.Error YfD.GetData)
    | GoVideo (List Pairing)
    | Loading E.Value
    | Scrap Int
    | ScrapComplete (Result Http.Error Decoder.Success)
    | GotSession Session
    | SaveIdComplete E.Value

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
    , Api.successId SaveIdComplete ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveIdComplete str ->
            let
                decodestr = Decode.decodeValue Decode.string str
            in
            case decodestr of
                Ok ok ->
                   (model,Route.pushUrl(Session.navKey model.session) Route.TogetherW) 
            
                Err _ ->
                    (model, Cmd.none)
        GotSession session ->
            -- if model.deleteAuth == "scrap" then
            --     update Scrap {model | session = session }
            -- else
           ({model | session = session}, Api.get GetList (Endpoint.makeDetail model.videoId) (Session.cred session)  (Decoder.yfDetailDetail YfD.GetData YfD.DetailData YfD.DetailDataItem YfD.Pairing)
           )
        ScrapComplete (Ok ok) ->
            let
                text = E.string "스크랩 되었습니다."
            in
            
            ({model | scrap = not model.scrap}, Cmd.none)
        ScrapComplete (Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err  
            in
             ({model | deleteAuth = "scrap"},(Session.changeInterCeptor (Just serverErrors) model.session))
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
            (model, Api.videoData videoList)
        GetList(Ok ok) ->
            ({model | getData = ok.data, loading = False}, Cmd.none)
        GetList(Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err  
            in
             (model,(Session.changeInterCeptor (Just serverErrors) model.session))
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
        GetId id ->
            let
                result = Decode.decodeValue Decode.string id
            in
            case result of
                Ok string ->
                    ({model | videoId = string} , Api.get GetList (Endpoint.makeDetail string) (Session.cred model.session)  (Decoder.yfDetailDetail YfD.GetData YfD.DetailData YfD.DetailDataItem YfD.Pairing)
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
            Route.pushUrl (Session.navKey model.session) Route.MakeExer)
        Scrap id ->
            (model, Cmd.batch[Api.saveId (E.string (String.fromInt id))])
          

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.check then
        app BackPage model
        else
        web BackPage model
            }

web msg model= 
    div [class "container"] [
        commonHeader "/image/icon_customworkout.png" "맞춤운동" ,
        contentsBody model.getData model.loading Scrap model.scrap GoVideo "공유하기",
        goBtn BackPage 

    ]
app msg model= 
    div [class "container"] [
        appHeaderConfirmDetail model.getData.title "makeExerHeader" Route.MakeExer "fas fa-times"  Route.EditFilter "수정"
        ,if model.loading then
        div [class "spinnerBack"] [
            spinner
            ]
        else 
        div [] []
        , appcontentsItem model.getData model GoVideo
    ]
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



appcontentsItem item model goVideo= 
            div [ ]
            [ div []
                [ p [ class "m_yf_container" ]
                    [ 
                        img [src item.thumbnail , onClick (goVideo item.pairing)][]                        
                        , videoCall
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
            , div [ class "m_work_script" ]
                  (List.indexedMap YfD.description item.exercise_items)
                
            ]

contentsBody item model scrap modelscrap goVideo scrapText=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model scrap modelscrap goVideo scrapText
               
            ]
        
        ]



contentsItem item loading scrap modelscrap govideo scrapText=
            div [ class "tile is-parent is-vertical" ]
            [ div [ class "yf_notification" ]
                [ p [ class "title" ]
                    [ 
                         div [] [
                             img [ src item.thumbnail , onClick (govideo item.pairing)] []
                            , videoCall
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
            , 
            -- if loading then 
            -- div [] []
            -- else
            div [ class "yf_text" ]
               (List.indexedMap YfD.description item.exercise_items)
            ]

justokData result = 
    case result of
        Just ok ->
            ok
        Nothing ->
            ""
videoCall = 
    div [id "myElement"] []