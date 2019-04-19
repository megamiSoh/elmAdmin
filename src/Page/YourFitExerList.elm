module Page.YourFitExerList exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Route exposing(..)
import Port exposing(..)
import Page.Common exposing (..)
import Api.Endpoint as Endpoint
import Api as Api
import Http as Http
import Json.Decode as Decode exposing(..)
import Json.Encode as Encode
import Api.Decoder as Decoder
import Page.Common exposing (..)
import Page.YourFitExer as YfE
type alias Model  = 
    { session : Session
    , isActive : String
    , exercise_part_code : String
    , difficulty_code : List String
    , listData : List DetailData
    , levelData : List Level
    , check : Bool
    , screenInfo : ScreenInfo
    , partDataName : List YfE.ListData
    , infiniteLoading : Bool
    , takeList : Int
    , resultLen : Int
    }



type alias ListData = 
    { data : List DetailData }

type alias DetailData = 
    { difficulty_name : String
    , duration : String
    , exercise_part_name : String
    , id : Int
    , mediaid : String
    , thembnail: String
    , title : String
    }
type alias LevelData = 
    { data : List Level }

type alias Level = 
    { code : String 
    , name : String}

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}
-- levelDecoder

detailEncoder part level session = 
    let
        list = 
            Encode.object
                [ ("exercise_part_code", Encode.string part)
                , ("difficulty_code", (Encode.list Encode.string) level)]
    
        body =
            list
                |> Http.jsonBody
    in
    (Decoder.yourfitDetailListData ListData DetailData)
    |> Api.post Endpoint.yfDetail (Session.cred session) GetList body 

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
     (
        { session = session
        , check = mobile
        , isActive = "" 
        , takeList = 5
        , resultLen = 0
        , exercise_part_code = ""
        , difficulty_code = []
        , infiniteLoading = False
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , listData = []
        , levelData = []
        , partDataName = []
        },
         Cmd.batch
            [ Api.getKey () 
            , Decoder.levelDecoder LevelData Level
            |> Api.get GetLevel Endpoint.level (Session.cred session) 
            ,Decoder.yourfitList YfE.YourFitList YfE.ListData YfE.ExerciseList 
            |> Api.get GetPart Endpoint.yourfitVideoList (Session.cred session)
            ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Api.receiveKey ReceiveId
    , Api.successId Success
    , Session.changes GotSession (Session.navKey model.session)]

type Msg 
    = IsActive String
    | GetList (Result Http.Error ListData)
    | ReceiveId Encode.Value
    | GetLevel (Result Http.Error LevelData)
    | DetailGo Int
    | Success Encode.Value
    | GotSession Session
    | BackPage
    | GetPart (Result Http.Error YfE.YourFitList)
    | ScrollEvent ScreenInfo
    -- | ReceiveId Encode.Value



toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)



scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                    if model.takeList < model.resultLen then
                        if model.isActive == "" then
                            ({model | takeList = model.takeList + 3, infiniteLoading = True}, detailEncoder model.exercise_part_code model.difficulty_code model.session)
                        else
                            ({model | takeList = model.takeList + 3, infiniteLoading = True}, detailEncoder model.exercise_part_code [model.isActive] model.session )
                    else
                        if model.isActive == "" then
                            ({model | takeList = model.takeList, infiniteLoading = False}, detailEncoder model.exercise_part_code model.difficulty_code model.session)
                        else
                            ({model | takeList = model.takeList, infiniteLoading = False}, detailEncoder model.exercise_part_code [model.isActive] model.session )
            else
                (model, Cmd.none)
        GetPart (Ok ok) ->
            ({model | partDataName = ok.data}, Cmd.none)
                
            
        GetPart (Err err) ->
            (model, Cmd.none) 
        BackPage ->
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.YourFitExer
            -- Api.historyUpdate (Encode.string "yourfitExercise")
            )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        Success str ->
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.YourfitDetail
            -- Api.historyUpdate (Encode.string "yourfitDetail")
            )
        DetailGo id ->
            let
                encodeId = Encode.int id
            in
            
            (model, Api.saveId (encodeId))
        GetLevel (Ok ok) ->
            ({model | levelData = ok.data}, Cmd.none)
        GetLevel (Err err) ->
            (model, Cmd.none)
        ReceiveId id ->
            let
                idDecode = Decode.decodeValue Decode.string id
            in
                case idDecode of
                    Ok ok ->
                        ({model | exercise_part_code = ok}, detailEncoder ok model.difficulty_code model.session ) 
                
                    Err _ ->
                         (model, Cmd.none)
            
        GetList (Ok ok) ->
            let
                makeList = 
                    List.take model.takeList ok.data
            in
            ({model | listData = makeList, infiniteLoading = False, resultLen = List.length (ok.data)}, Cmd.none)
        GetList (Err err) ->
            (model, Cmd.none)
        IsActive level ->
            if level == "" then
            ( {model | isActive = level}, detailEncoder model.exercise_part_code [] model.session )
            else
            ( {model | isActive = level}, detailEncoder model.exercise_part_code [level] model.session )
        
view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.check then 
         div [] [
            let
                f=
                    List.head (
                        List.filter (\x -> 
                            x.code == model.exercise_part_code
                        )model.partDataName
                    )
                
            in
            case f of
                Just result ->
                    appHeaderRDetailClick result.name "yourfitListDetail yourfitHeader" BackPage "fas fa-angle-left"
                Nothing ->
                    appHeaderRDetailClick "" "yourfitListDetail yourfitHeader" BackPage "fas fa-angle-left"
            ,app model] 
            else
            web model
    } 
app model =    
    div ([ class "container" ] ++ [style "max-height" "100%"])
                [  div [class "heightFix"] [
                    apptapbox model
                ],     
                    if List.length(model.listData) > 0 then
                    div ([ class "searchbox" ] ++ [scrollEvent ScrollEvent])
                    (List.indexedMap contentsLayout2 model.listData)
                    else
                    div [class "noResult"] [
                        text "검색결과가 없습니다."
                    ],
                    if model.infiniteLoading then
                    div [class "loadingPosition"] [
                    spinner
                    ]
                    else
                    span [] [] 
                                                      
                ]
web model= 
    div [ class "containerwrap" ]
                [ div [ class "container" ]
                    [ div [ class "notification yf_workout" ]
                        [
                            commonHeader "/image/icon_workout.png" "유어핏운동",
                            div [ class "yf_yfworkout_search_wrap" ]
                            [
                                tapbox model,
                                div [ class "earchbox_wrap" ]
                                    [ 
                                        if List.length(model.listData) > 0 then
                                        div [ class "yf_searchbox" ]
                                        (List.map contentsLayout model.listData)
                                        else
                                        div [class "yf_noResult"] [
                                            text "검색결과가 없습니다."
                                        ]
                                    ]
                            ]                                   
                        ]
                    ] 
                ]
tapbox model =
    div [ class "yf_tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large" ]
            [ ul []
                (
                [li [ classList [
                    ("yf_active", model.isActive == "")
                    ], onClick (IsActive "") ]
                    [ text "전체" 
                    ]] ++ 
                List.map (\x -> 
                        li [ classList [
                            ("yf_active", model.isActive == x.code)
                        ],  onClick (IsActive x.code) ]
                            [  text x.name ]
                        ) model.levelData
                )
            ]
        ]

apptapbox model =    
    div [ class "m_to_yourfitListDetail" ]
                 (
                [
                div [ classList [
                    ("m_yourfitListDetail_yf_active", model.isActive == "")
                    ], onClick (IsActive "") ]
                    [ text "전체" 
                    ]] ++ 
                List.map (\x -> 
                        div [ classList [
                            ("m_yourfitListDetail_yf_active", model.isActive == x.code)
                        ],  onClick (IsActive x.code) ]
                            [  text x.name ]
                        ) model.levelData
                )
            

goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]
        
contentsLayout item = 
    div [ class "column is-multiline videoboxwrap" , onClick (DetailGo item.id)]
        [ div [ class "video_image" ]
            [ img [ class "vpic1", src item.thembnail, alt "dummy_video_image" ]
                []
            ]
        , div [ class "workout_title" ]
            [ text item.title ]
        , div [ class "iconbox" ]
            [ span []
                [ i [ class "fas fa-clock" ]
                    []
                ]
            ]
        , div [ class "timebox" ]
            [ text item.duration ]
        , div [ class "partbox" ]
            [ text item.exercise_part_name ]
        , div [ class "levelbox" ]
            [ text item.difficulty_name ]
        ]

contentsLayout2 idx item = 
    div [ class "column is-multiline m_videoboxwrap" , onClick (DetailGo item.id)]
        [ div [ class "m_work_videobox" ]
            [ 
                img [ class "appvpic1", src item.thembnail, alt "dummy_video_image" ]
                []
            ]
        , div [class "m_workout_title"] [
            div []
            [ text item.title ]
            , div [ class "m_iconbox" ]
                [ div [ class "m_partbox" ]
                [ text item.exercise_part_name ]
                , div [ class "m_levelbox" ]
                [ text item.difficulty_name ]
                ]
            , div [class "m_timebox"]
                [ i [ class "fas fa-clock" ]
                    []
                        , div [ class "m_timebox" ]
                    [ text item.duration ]
                ]
        ]
        ]
