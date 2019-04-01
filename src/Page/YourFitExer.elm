port module Page.YourFitExer exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
-- import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)
import Page.Common exposing (..)
import Port as Ports
import Api.Endpoint as Endpoint
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Json.Encode as Encode
import Swiper
import Html.Lazy exposing (lazy, lazy2, lazy3)
type alias Model 
    = {
        session : Session
        , title : String
        , checkDevice : String
        , data : List ListData
        , check : Bool
        , loading : Bool
        , sumCount : Int
        , menuOpen : Bool
        , swipingState : Swiper.SwipingState
        , swipeCode : String
        , leftWidth : Int
        , lazyImg : String
        , count : Int
    }

type alias YourFitList =
    { data : List ListData }

type alias ListData = 
    { code : String
    , exercises : List ExerciseList
    , name : String
    }

type alias ExerciseList = 
    { difficulty_name : String
    , duration : String
    , exercise_part_name : String
    , id : Int
    , mediaid : String
    , thembnail: String
    , title : String
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
     (
        { session = session
        , check = mobile
        , title = ""
        , checkDevice = ""
        , data = []
        , swipeCode = ""
        , loading = True
        , sumCount = 0
        , menuOpen = False
        , count = 1
        , leftWidth = 0
        , lazyImg = "../image/05iu6bgl-320.jpg"
        , swipingState = Swiper.initialSwipingState
        }
        , Cmd.batch
        [ Ports.checkMobile ()
        , Api.get GetList Endpoint.yourfitVideoList (Session.cred session) (Decoder.yourfitList YourFitList ListData ExerciseList )
        
        ]
    )


type Msg 
    = CheckDevice Encode.Value
    | GetList (Result Http.Error YourFitList)
    | GoDetail String
    | Success Encode.Value
    | GoContentsDetail Int
    | SuccessId Encode.Value
    | GotSession Session
    | Swiped String Swiper.SwipeEvent 
    | GetIndex String
    | OnLoad Int

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onLoad msg =
    on "load" (Decode.succeed msg)

subscriptions :Model -> Sub Msg
subscriptions model=
    Sub.batch
    [ Ports.check CheckDevice
    , Api.successSave Success
    , Session.changes GotSession (Session.navKey model.session)
    , Api.successId SuccessId]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoad idx->
            if model.count >= model.sumCount then
            ({model | loading = False}, Cmd.none)
            else
            ({model | count = model.count + 1}, Cmd.none)
        GetIndex str -> 
            (model, Cmd.none)
        Swiped str evt ->
            let 
                ( newState, swipedLeft ) =
                    Swiper.hasSwipedRight evt model.swipingState
            in
                ( { model | menuOpen = swipedLeft, swipingState = newState, swipeCode = str , leftWidth = -200}, Cmd.none )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        SuccessId str ->  
            (model, Route.pushUrl (Session.navKey model.session) Route.YourfitDetail)

        GoContentsDetail id ->
            let
                encodeId = Encode.int id
            in
            
            (model, Api.saveId (encodeId))
        Success str ->
            (model,Route.pushUrl (Session.navKey model.session) Route.YourFitList)
        GoDetail code ->
            let
                go = Encode.string code
            in
            (model, Api.saveKey go)
        GetList (Ok ok) ->
            let
                new = List.map (\x ->
                        List.length (x.exercises)
                    ) ok.data
                result = List.sum new
            in
            if model.check then
            ({model | data = ok.data, sumCount = result}, Cmd.none)
            else 
            ({model | data = ok.data, sumCount = result, loading = False}, Cmd.none)
        GetList (Err err) ->
            (model, Cmd.none)

        CheckDevice str ->
           let 
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ -> 
                        ({model | checkDevice = ""}, Cmd.none)







    
view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourfitExercise"
    , content = 
        webOrApp model
    }


webOrApp model =
        if model.check then
        div [class "appWrap" ] [
                    appHeaderSearch "유어핏운동" "yourfitHeader",
                    if model.loading then
                    div [class "spinnerBack"] [
                        spinner
                        ]
                    else 
                    div [] []
                   , app model
            ]
        else
            div [] [
            if model.loading then
                div [class "spinnerBack"] [
                spinner
                ]
            else 
            div [] []
            , web model
            ]
            
web model =
    div [ class "yourfitExercise_yf_workoutcontainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_workout.png" "유어핏운동",
                        if model.loading then
                            spinner
                        else
                        div [] (List.map (\x -> 
                            lazy bodyContents x
                            ) model.data)
                    ]
                ]
            ]


app model =
        div [ class "container" ]
            [ div [ class "flexContainer"]
                [
                       
                    div [] (List.map (\x -> 
                            lazy2 bodyContentsApp x model
                            ) model.data)
                    
                ]
            ]
            

bodyContents item = 
    div [ class "menubox_wrap"]
    [ 
        div [ class "yf_workoutmenubox1" ]
        [ div [ class "yf_workoutvideopic" ]
            [ 
                if item.code == "10" then
                img [ class "bigpic1", src "/image/workout_menu1.png", alt item.name ]
                []
                else if item.code == "20" then
                img [ class "bigpic1", src "/image/workout_menu2.png", alt item.name ]
                []
                else
                img [ class "bigpic1", src "/image/workout_menu3.png", alt item.name ]
                []
            ,
             div [ class "yf_workouttext1" ]
                [ text item.name ]
            ]
        , div [] (List.map videoItem item.exercises)
        , div [ class "yf_workoutaddwarp" ]
                [ div [ class "yf_workoutadd" , onClick (GoDetail item.code)]
                    [ div [ class "yf_workouttext3" ]
                        [ text "더보기" ]
                    ]
                ]
        ]
    ]


bodyContentsApp item model= 
    div [class "notification m_yfwo_menubox1"][
    div [ class "m_menubox_wrap" ]
    [
         div ([class "originSwipeStyle"]
         ++ [style "width" ( String.fromInt (50 * (List.length(item.exercises))+ 34 ) ++ "vw")]
                        ++ Swiper.onSwipeEvents (Swiped item.code )++ [onClick (GetIndex item.code)]) 
                    [    
         div [ class "m_yf_workoutmenubox" ]
        [ div [ class "yf_workoutvideopic" ]
            [ if item.code == "10" then
                img [ class "bigpic1", src "/image/workout_menu1.png", alt item.name ]
                []
                else if item.code == "20" then
                img [ class "bigpic1", src "/image/workout_menu2.png", alt item.name ]
                []
                else
                img [ class "bigpic1", src "/image/workout_menu3.png", alt item.name ]
                []
            , div [ class "m_yf_stretchingtext" ]
                [ text item.name ]
            ]
        ,
        div [] (List.indexedMap (\idx x -> videoItemApp idx x model) item.exercises)
        , div [ class "yf_workoutaddwarp" ]
                [ div [ class "m_yf_workoutadd", onClick (GoDetail item.code) ]
                    [ div [ class "m_addtext" ]
                        [ text "더보기" ]
                    ]
                ]
                ]
        
                    ]
    ]]



videoItem item = 
    div [ class "yf_workoutvideoboxwrap" , onClick (GoContentsDetail item.id)]
            [ div [ class "yf_workoutvideo_image" ]
                [ img [ class "yf_workoutvpic1", src item.thembnail ]
                    []
                ]
            , div [ class "yf_workoutvideo_lavel_bg" ]
                [ div [ class "level" ]
                    [ text item.difficulty_name ]
                ]
            , div [ class "yf_workoutworkout_title" ]
                [ text item.title ]
            , div [ class "m_timebox" ]
                [
                    i [ class "fas fa-stopwatch" ]
                    []
                    , text " "
                    , text item.duration ]
            ]
lazyImageview item lazy= 
    img [class "m_workoutvpic",  src item ] [
       
    ]

videoItemApp idx item model = 
    div [ class "m_workoutvideoboxwrap" , onClick (GoContentsDetail item.id)]
            [   
                div [ class "m_yf_workoutvideo_image" ]
                [ 
                    -- lazy2 lazyImageview item.thembnail model.lazyImg
                    img [ class "m_workoutvpic", 
                    src item.thembnail, onLoad (OnLoad idx)]
                    []
                ]
            , div [ class "m_yf_workoutvideo_lavel_bg" ]
                [ div [ class "m_level" ]
                    [ text item.difficulty_name ]
                ]
            , div [ class "m_yf_workoutworkout_title" ]
                [ text item.title ]
            , div [ class "m_timebox" ]
                [ 
                    i [ class "fas fa-stopwatch" ]
                    []
                    , text " "
                    , text item.duration ]
            
            ]


