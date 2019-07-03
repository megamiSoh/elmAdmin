module Page.Detail.MyPostDetail exposing(..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
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
    , getData : TogetherData
    , loading : Bool
    , scrap : Bool
    , postId : String
    , zindex : String
    }

type alias TogetherDataWrap = 
    { data : TogetherData 
    }

type alias TogetherData = 
    { content : Maybe String
    , detail : Maybe (List DetailTogether)
    , id : Int
    , inserted_at : String
    , is_delete : Bool
    , link_code : String
    , recommend_cnt : Int
    , nickname: Maybe String
    }
type alias DetailTogether = 
    { thembnail : String
    , difficulty_name : Maybe String
    , duration : String
    , exercise_items : List TogetherItems
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List Pairing 
    , title : String
    }

type alias TogetherItems = 
    { exercise_id : Int
    , is_rest : Bool
    , sort : Int
    , title : String
    , value : Int }

type alias Pairing = 
    { file : String
    , image : String
    , title : String 
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , loading = True
        , scrap = False
        , postId = ""
        , zindex =""
        , getData = 
            { content = Nothing
            , detail = Nothing
            , id = 0
            , inserted_at = ""
            , is_delete = False
            , link_code = ""
            , recommend_cnt = 0
            , nickname = Nothing
            }
        }
        , Cmd.batch 
        [  Api.getId ()
        ]
        
    )
    -- 
-- 
type Msg 
    = GotSession Session
    | BackPage
    | GetId E.Value
    | GetList (Result Http.Error TogetherDataWrap)
    | GoVideo
    | Loading E.Value
    | VideoCall (List Pairing)
    | ClickRight
    | ClickLeft
    | GoAnotherPage

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
    , Session.changes GotSession (Session.navKey model.session) ]

justList item = 
    case item of
        Just a ->
            a
    
        Nothing ->
            []

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (E.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        VideoCall pairing ->
            let
                encodePairing pair= 
                    E.object 
                        [ ("file", E.string pair.file)
                        , ("image", E.string pair.image)
                        , ("title", E.string pair.title)]
                listPair = 
                    E.list encodePairing pairing
            in
            
            ({model | zindex = "zindex"}, Api.videoData listPair)
        GoVideo ->
            let
                head = List.head (justList model.getData.detail)
                result = 
                    case head of
                        Just a ->
                            a.pairing
                    
                        Nothing ->
                            []
                videoList = 
                    E.object 
                        [ ("pairing", E.list videoEncode result)]

                videoEncode p=
                    E.object
                        [ ("file", E.string p.file)
                        , ("image", E.string p.image)
                        , ("title", E.string p.title)
                        ]
            in
            (model, Cmd.none)
        GetList(Ok ok) ->
            update GoVideo {model | getData = ok.data, loading = False}
        GetList(Err err) ->
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
        GetId id ->
            let
                result = Decode.decodeValue Decode.string id
            in
            case result of
                Ok string ->
                    ({model | postId = string} , 
                    Decoder.mypostDataWrap TogetherDataWrap TogetherData DetailTogether TogetherItems Pairing
                    |>Api.get GetList (Endpoint.postList string) (Session.cred model.session)  
                    )
            
                Err _ ->
                    (model,Cmd.none)
        
        GotSession session ->
            ({model | session = session} , 
            Decoder.mypostDataWrap TogetherDataWrap TogetherData DetailTogether TogetherItems Pairing
            |> Api.get GetList (Endpoint.postList model.postId) (Session.cred session) 
            )
        BackPage ->
            (model, 
            -- -- Api.historyUpdate (E.string "myPost")
            Route.pushUrl (Session.navKey model.session) Route.MyPost
            )
          

view : Model -> {title : String , content : Html Msg}
view model =
    -- if model.check then
    --     { title = "내 게시물 관리"
    --     , content = 
    --         div [] [
    --             app BackPage model
    --         ]
    --     }
    -- else
        { title = "내 게시물 관리"
        , content = 
            div [] [
                div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage False]
                , web BackPage model
            ]
        }
web msg model= 
    div [class "container"] [
        div [ class "yf_yfworkout_search_wrap" ]
        [ div []
        ( List.map( \x-> 
            contentsItem x model
        ) (justList model.getData.detail)) 
        -- ( List.map( \x-> 
        --     appcontentsItem x model
        -- ) model.getData.detail) 
        -- contentsBody model.getData model.loading
        , goBtn
        ]
    ]
app model video goback = 
        div []
        ( List.map( \x-> 
            appcontentsItem x model video goback
        ) (justList model.getData.detail)) 
goBtn  = 
    div [ class "make_yf_butbox" ]
        [ div [ class "yf_backbtm" ]
            [ a [ class "button yf_largebut", Route.href Route.MyPost ]
                [ text "뒤로" ]
            ]
        , div [ class "yf_nextbtm" ]
            [ a [ class "button is-dark yf_editbut", Route.href Route.FilterS1 ]
                [ text "수정" ]
            ]
        ]

contentsBody item model=
    
    div [ class "yf_yfworkout_search_wrap" ]
        [ div [ class "tapbox" ]
            [ div [ class "yf_large" ]
                [ text item.title ],
                contentsItem item model
               
            ]
        
        ]

contentsItem item model=
            div [ class "tile is-parent is-vertical" ]
            [div [ class "yf_notification" ]
                [
                    div [ class "tapbox" ]
                    [ div [ class "yf_large" ]
                        [ text item.title ]
                    ]
                , div [class "postVideoWrap"] [
                    div [ class ("imagethumb " ++ model.zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thembnail ++") ") , onClick (VideoCall item.pairing) ][]


                    -- img [class "postImg" ,src item.thembnail, onClick (VideoCall item.pairing) ] []
                , div [id "myElement", style "height" (if String.isEmpty model.zindex then "0px" else "auto") ] []

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
                    [ text ((justokData item.exercise_part_name) ++ " - " ++  (justokData item.difficulty_name)) ]
                ]
            , 
            pre [class "wordBreak descriptionBackground"]
                    [ 
                    text (justokData model.getData.content)
                    ]
        ,
            div [ class "yf_text" ]
               (List.indexedMap YfD.description (List.sortBy .sort item.exercise_items))
            ]



appcontentsItem item model video goback= 
            div [ ]
            [  appHeaderRDetailClick2 item.title "myPageHeader" goback "fas fa-times" 
                , div [class "PostVideo"] [
                     div [ class ("appimagethumb " ++ model.zindex ), style "background-image" ("url(../image/play-circle-solid.svg) ,url("++ item.thembnail ++") ") , onClick (video item.pairing) ][]
                    , div [id "myElement"][]
                ], 
                div [ class "m_yf_post_textbox" ]
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
                ]
            ,  div []
                [ pre [class "wordBreak descriptionBackground"]
                    [ 
                    text (justokData model.getData.content)
                    ]
                ]
            , div [ class "m_work_script" ]
                  (List.indexedMap YfD.description (List.sortBy .sort item.exercise_items))
                
            ]
justokData result = 
    case result of
        Just ok ->
            ok
        Nothing ->
            ""

-- goBtn back  = 
--     div [ class "make_yf_butbox" ]
--         [ div [ class "yf_backbtm" ]
--             [ div [ class "button yf_largebut", Route.href Route.MyPost ]
--                 [ text "뒤로" ]
--             ]
--         , div [ class "yf_nextbtm" ]
--             [ a [ class "button is-dark yf_editbut", Route.href Route.EditFilter]
--                 [ text "수정" ]
--             ]
--         ]