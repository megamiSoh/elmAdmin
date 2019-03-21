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

type alias Model  = 
    { session : Session
    , isActive : String
    , exercise_part_code : String
    , difficulty_code : List String
    , listData : List DetailData
    , levelData : List Level
    , check : Bool
    }



type alias ListData = 
    { data : List DetailData }

type alias DetailData = 
    { difficulty_name : String
    , exercise_part_name : String
    , id : Int
    , title : String}

type alias LevelData = 
    { data : List Level }

type alias Level = 
    { code : String 
    , name : String}

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
     Api.post Endpoint.yfDetail (Session.cred session) GetList body (Decoder.yourfitDetailListData ListData DetailData)

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
     (
        { session = session
        , check = mobile
        , isActive = "" 
        , exercise_part_code = ""
        , difficulty_code = []
        , listData = []
        , levelData = []
        },
         Cmd.batch
            [ Api.getKey () 
            , Api.get GetLevel Endpoint.level (Session.cred session) (Decoder.levelDecoder LevelData Level)
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
    -- | ReceiveId Encode.Value



toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        Success str ->
            (model, Route.pushUrl (Session.navKey model.session) Route.YourfitDetail)
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
            ({model | listData = ok.data}, Cmd.none)
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
        div [ class "containerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_workout.png" "유어핏운동",
                        div [ class "yf_yfworkout_search_wrap" ]
                        [
                            tapbox model,
                            div [ class "searchbox_wrap" ]
                                [ 
                                    if List.length(model.listData) > 0 then
                                    div [ class "searchbox" ]
                                    (List.map contentsLayout model.listData)
                                    else
                                    div [] [
                                        text "검색결과가 없습니다."
                                    ]
                                ]
                        ]                                   
                    ]
                ] 
            ]
    } 


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
            [ img [ class "vpic1", src "/image/dummy_video_image.png", alt "dummy_video_image" ]
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
            [ text " ㅡ " ]
        , div [ class "partbox" ]
            [ text item.exercise_part_name ]
        , div [ class "levelbox" ]
            [ text item.difficulty_name ]
        ]

