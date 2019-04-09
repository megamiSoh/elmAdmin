module Page.Detail.TogetherWrite exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Json.Decode as Decode
import Json.Encode as Encode
import String
import Route exposing (..)
import Port as P
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Page.Detail.YourFitDetail as YfD
import Page.Detail.Editorwirte exposing (..)
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline

defaultOptions =
    { softAsHardLineBreak = False
    , rawHtml = ParseUnsafe
    }

type alias Model 
    = {
        session : Session
        , fileName : String
        , onDemandText : String
        , options : Markdown.Config.Options
        , showToC : Bool
        , checkDevice : String
        , selectedTab : EditorTab
        , selectedPreviewTab : PreviewTab
        , check : Bool
        , videoId : String
        , content : String
        , getData : DetailData
        , inputValue : String
        , listShow : Bool

    }

type alias DetailData =    
    { difficulty_name : Maybe String
    , duration : String
    , exercise_items : List YfD.DetailDataItem
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List YfD.Pairing
    , title : String
    , nickname : Maybe String
    , thumbnail: String}

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , fileName = ""
        , checkDevice = ""
        , onDemandText = ""
        , options = defaultOptions
        , selectedTab = Editor
        , selectedPreviewTab = RealTime
        , showToC = False
        , videoId = ""
        , content = ""
        , listShow = False
        , inputValue = ""
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
        , check = mobile}
        , Api.getId ()
    )
type EditorTab
    = Editor


type PreviewTab
    = RealTime


type Msg 
    = GetFile String
    -- | CheckDevice E.Value
    | BackBtn
    | GetShare Encode.Value
    | GoShare
    | GoRegist
    | Content String
    | ShareComplete (Result Http.Error Decoder.Success)
    | GetList (Result Http.Error YfD.GetData)
    | ListShow 
    -- | KeyDown Int

shareTogether id session content= 
    let
        body = 
            Encode.object   
                [ ("content" , Encode.string content)]
                |> Http.jsonBody     
            -- "content="
            -- ++ content 
            --     |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.togetherShare id) (Session.cred session) ShareComplete body (Decoder.resultD)


-- onKeyDown:(Int -> msg) -> Attribute msg
-- onKeyDown tagger = 
--     on "keydown" (Decode.map tagger keyCode)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions : Model -> Sub Msg
subscriptions model =
    Api.receiveId GetShare

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ListShow -> 
            ({model | listShow = not model.listShow}, Cmd.none)
        -- KeyDown key ->
        --     if key == 13 then
        --         update GoShare model
        --     else
        --         (model, Cmd.none)
        GetList(Ok ok) ->
            ({model | getData = ok.data}, Cmd.none)
        GetList(Err err) ->
            (model,Cmd.none)
        ShareComplete (Ok ok) -> 
            let
                text = Encode.string "공유되었습니다."
            in
            (model, Cmd.batch[Route.pushUrl (Session.navKey model.session) Route.Together,  Api.showToast text])
        ShareComplete (Err err) -> 
            (model, Cmd.none)
        Content str ->
            ({model | content = str}, Cmd.none)
        GoRegist ->
            (model, Cmd.none)
        GoShare ->
            let
                st = Encode.encode 0 (Encode.string model.content)
            in
                    (model, Cmd.batch[shareTogether model.videoId model.session model.content, Api.removeId () ])
            
            
        GetShare id -> 
            let _ = Debug.log "get" id
                idDecoder = Decode.decodeValue Decode.string id
                -- let _ = Debug.log "str" String.filter Char.isDigit str 
                
            in
            case idDecoder of
                Ok ok ->
                    ({model | checkDevice = "공유동영상", videoId = ok}, Api.get GetList (Endpoint.makeDetail ok) (Session.cred model.session)  (Decoder.yfDetailDetail YfD.GetData YfD.DetailData YfD.DetailDataItem YfD.Pairing) )
            
                Err _ ->
                    ({model | checkDevice = "공유동영상"}, Cmd.none)
            
        GetFile filename ->
                ({model | fileName = filename}, Cmd.none)
        BackBtn ->
            (model, Route.pushUrl (Session.navKey model.session) Route.Together)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
        if model.check then
         app model
        else
         web model
    }

web model = 
  div [class "container"] [
    div [class "notification yf_workout"] [
        commonJustHeader "../image/icon_together.png" "함께해요",
        div [class "yf_yfworkout_search_wrap together"] [
            bodyContents model
            ,goBtn model
        ]
    ]  ]

app model= 
    div [class "container"] [
        if model.checkDevice == "" then
        appHeaderConfirmDetailR "게시물작성" "togetherHeader" BackBtn GoRegist "등록" 
        else
        appHeaderConfirmDetailR "게시물작성" "togetherHeader" BackBtn GoShare "공유" 
        ,
        bodyContents_app model
    ]
bodyContents model = 
    div [ class "togetherWrite_searchbox" ]
        [ div [ class "togetherWrite_mediabox" ]
            [
                if model.checkDevice == "" then
                div [] []
                else
                -- div [] [
                --     img [ src "/image/dummy_video_image3.png" ]
                --     []
                -- , text model.checkDevice]
                videoContent model
               , div [class "togethertextarea"] [
                   editorView model.content Content False "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요"
               ]
            ]
        , div [ class "file has-name is-fullwidth togetherWrite_yf_upload" ]
            [ 
                if model.checkDevice == "" then
                label [ class "file-label" ]
                [ input [ class "file-input", type_ "file", name "resume" , onChange GetFile ]
                    []
                , span [ class "file-cta" ]
                    [ span [ class "file-icon" ]
                        [ i [ class "fas fa-upload" ]
                            []
                        ]
                    , span [ class "file-label" ]
                        [ text "사진올리기" ]
                    ]
                , span [ class "file-name" ]
                    [ 
                        if model.fileName == "" then
                            text "사진을 선택 해 주세요."
                        else 
                            text model.fileName
                    ]
                ]
                else 
                div [] []
            ]
        ]

bodyContents_app model = 
    div [ class "m_togetherWrite_searchbox" ]
        [ div [ class "m_togetherWrite_mediabox" ]
            [ 
                if model.checkDevice == "" then
                div [] []
                else
                div [] [
                text model.checkDevice
                , videoContent model
                ]
                ]
                -- , textarea [ class "textarea m_togetherWrite_textarea", placeholder "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요", rows 10 , onInput Content ]
                -- []
                , div [class "apptogethertextarea"] [editorView model.content Content False "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요"
            ]]
    
        -- ]
                
videoContent model = 
    div [ class "yf_box" ]
        [ 
            div [class "m_yf_img"] [
                img [ src model.getData.thumbnail ]
            []
            ]
        , div [ class "text_wrap" ]
            [ div [ class "yf_box_title" ]
                [ text model.getData.title ]
            , div [ class "togetherUl" ]
                [ ul []
                    [ li [] [
                        i [ class "fas fa-stopwatch" ]
                        [] ,
                        text " ",
                        text model.getData.duration]
                    , li [class "itemlist"] [
                        ul [] (
                            List.indexedMap ( \idx x ->
                            exerciseItems idx x model
                                -- exerciseItems idx (List.sortBy x.sort)
                            ) (List.sortBy .sort model.getData.exercise_items)
                            
                        )
                        ,
                        if List.length model.getData.exercise_items > 3 then
                            if model.listShow then
                                 div [onClick ListShow, class "button is-small"] [text "닫기"]
                            else
                                 div [onClick ListShow, class "button is-small"] [text "자세히.."]
                        else
                        div [] []
                    ]
                    , li []
                        [ text (String.dropRight 10(model.getData.inserted_at)) ]
                    ]
                ]
            ]
        ]       

exerciseItems idx item model= 
        li [
            classList
                [ ("hideList", idx >= 3)
                , ("allShow", model.listShow)]
        ] [text (((String.fromInt item.sort) ++ ". ") ++ item.title ++ " x " ++(
            if item.is_rest == True then
                String.fromInt item.value  ++ "분"
            else
                String.fromInt item.value ++ "세트"
        ))]

goBtn model = 
    div [] [
        div [ class " togetherWrite_yf_dark" ]
            [ 
                if model.checkDevice == "" then
                div [ class "button is-dark togetherWrite_yf_dark", onClick GoRegist ]
                [ text "등록하기" ] 
                else
                div [ class "button is-dark togetherWrite_yf_dark", onClick GoShare ]
                [ text "공유하기"  ] 
            ]
        , div [ class "yf_butbox" ]
            [ a [ class "button togetherWrite_yf_butbox", Route.href Route.Together ]
                [ text "뒤로" ]
            ]
    ]


onChange: (String -> msg) -> Html.Attribute msg
onChange tagger = 
    on "change" (Decode.map tagger targetValue)

targetFiles : Decode.Decoder (List String)
targetFiles = 
    Decode.at ["target", "files"] (Decode.list Decode.string)



