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
import Html.Lazy exposing (lazy, lazy2)
type alias Model 
    = {
        session : Session
        , title : String
        , checkDevice : String
        , data : List ListData
        , check : Bool
        , loading : Bool
        , menuOpen : Bool
        , swipingState : Swiper.SwipingState
        , swipeCode : String
        , leftWidth : Int
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
    , exercise_part_name: String
    , id : Int
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
        ,  menuOpen = False
        , leftWidth = 0
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

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check



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
        GetIndex str -> 
            let _ = Debug.log "index" str
                
            in
            
            (model, Cmd.none)
        Swiped str evt ->
            let _ = Debug.log "eve" evt
                ( newState, swipedLeft ) =
                    Swiper.hasSwipedRight evt model.swipingState
            in
                let _ = Debug.log "left" newState
                    
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
            ({model | data = ok.data, loading = False}, Cmd.none)
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





navigation : Model -> Html Msg
navigation model =
    nav []
        [ ul [ class "navigtion swipenavStyle", style "left" (if model.menuOpen then "0" else "-50px") ]
            [ li [ class "nav-item" ] [ a [] [ text "Home" ] ]
            , li [ class "nav-item" ] [ a [] [ text "About" ] ]
            , li [ class "nav-item" ] [ a [] [ text "Contact" ] ]
            ]
        ]

    
view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourfitExercise"
    , content = 
        webOrApp model
    }


webOrApp model =
        if model.check then
        div [] [
                  
                    appHeaderSearch "유어핏운동" "yourfitHeader",
                    if model.loading then
                    spinner
                    else 
                    app model
            ]
        else
            web model
            
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
                            bodyContents x
                            ) model.data)
                    ]
                ]
            ]


app model =
        div [ class "container" ]
            [ div []
                [
                       
                    div [] (List.map (\x -> 
                            bodyContentsApp x model
                            ) model.data)
                    
                ]
            ]
            

bodyContents item = 
    div [ class "menubox_wrap" ]
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
         ++ [style "width" ( String.fromInt (380 * (List.length(item.exercises)) + 220) ++ "px")]
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
        div [] (List.map (\x -> videoItemApp x model) item.exercises)
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
                [ img [ class "yf_workoutvpic1", src "image/dummy_video_image.png" ]
                    []
                ]
            , div [ class "yf_workoutvideo_lavel_bg" ]
                [ div [ class "level" ]
                    [ text item.difficulty_name ]
                ]
            , div [ class "yf_workoutworkout_title" ]
                [ text item.title ]
            ]

videoItemApp item model = 
    div [ class "m_workoutvideoboxwrap" , onClick (GoContentsDetail item.id)]
            [   
                div [ class "m_yf_workoutvideo_image" ]
                [ img [ class "m_workoutvpic", src "image/dummy_video_image.png"  ]
                    []
                ]
            , div [ class "m_yf_workoutvideo_lavel_bg" ]
                [ div [ class "m_level" ]
                    [ text item.difficulty_name ]
                ]
            , div [ class "m_yf_workoutworkout_title" ]
                [ text item.title ]
            ]


