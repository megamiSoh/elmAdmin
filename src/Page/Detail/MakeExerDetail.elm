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
    }

type alias DetailData =    
    { difficulty_name : Maybe String
    , duration : String
    , exercise_items : List YfD.DetailDataItem
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List YfD.Pairing
    , title : String}

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , loading = True
        , getData = 
            { difficulty_name = Nothing
            , duration = ""
            , exercise_items = []
            , exercise_part_name = Nothing
            , id = 0
            , inserted_at = ""
            , pairing = []
            , title = ""}
        }
        , Cmd.batch 
        [ P.checkMobile ()
        , Api.getId ()
        ]
        
    )
    
-- 
type Msg 
    = CheckDevice E.Value
    | BackPage
    | GetId E.Value
    | GetList (Result Http.Error YfD.GetData)
    | GoVideo
    | Loading E.Value

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
    , Api.videoSuccess Loading ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoVideo ->
            let
                videoList = 
                    E.object 
                        [("pairing", (E.list videoEncode) model.getData.pairing) ]

                videoEncode p=
                    E.object
                        [ ("file", E.string p.file)
                        , ("image", E.string p.image)
                        , ("title", E.string p.title)
                        ]
            in
            (model, Api.videoData videoList)
        GetList(Ok ok) ->
            update GoVideo {model | getData = ok.data}
        GetList(Err err) ->
            (model,Cmd.none)
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
                    (model , Api.get GetList (Endpoint.makeDetail string) (Session.cred model.session)  (Decoder.yfDetailDetail YfD.GetData YfD.DetailData YfD.DetailDataItem YfD.Pairing)
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
            (model, Route.pushUrl (Session.navKey model.session) Route.MakeExer)
          

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
        YfD.contentsBody model.getData model.loading,
        goBtn BackPage 

    ]
app msg model= 
    div [class "container"] [
        appHeaderBDetail model.getData.title "makeExerHeader" BackPage
        , if model.loading then spinner else span [] []
        , appcontentsItem model.getData
    ]
goBtn back  = 
    div [ class "make_yf_butbox" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button yf_largebut", onClick back ]
                [ text "뒤로" ]
            ]
        , div [ class "yf_nextbtm" ]
            [ a [ class "button is-dark yf_editbut", Route.href Route.FilterS1 ]
                [ text "수정" ]
            ]
        ]

contentsData 
    = {
        name = "내가만든운동영상",
        time = "00:00" ,
        exerPart = "상체", 
        level = "하",
        exercise = "헬스",
        tool = "맨손",
        video = "/image/dummy_video_image2.png",
        description = "1. 덤벨 숄더 프레스는 전면 삼각근과 측면 삼각근을 강화시키는 운동입니다. 바벨 프레스보다 더 큰 가동범위로 훈련할 수 있는 장점이 있습니다." 
    }

appcontentsItem item= 
            div [ ]
            [ div []
                [ p [ class "m_yf_container" ]
                    [ 
                         div [ id "myElement" ] [
                            ]
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
                    [ text ((justokData item.exercise_part_name) ++ " - " ++ (justokData item.difficulty_name)) ]
                , div [ class "m_yf_scrapt" ]
                    [ span []
                        [ i [ class "far fa-bookmark" ]
                            []
                        ]
                    ]
                ]
            , div [ class "m_work_script" ]
                  (List.indexedMap YfD.description item.exercise_items)
                
            ]
justokData result = 
    case result of
        Just ok ->
            ok
        Nothing ->
            ""
-- description item = 
--     ul [] [
--         li [] [text item]
--     ]
    