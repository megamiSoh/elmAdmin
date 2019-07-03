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
import Page.Detail.YourFitDetail as YfD

type alias Model  = 
    { session : Session
    , isActive : String
    , exercise_part_code : String
    , difficulty_code : List String
    , listyourfitData : List DetailData
    , levelData : List Level
    , check : Bool
    , screenInfo : ScreenInfo
    , partDataName : List YfE.ListData
    , infiniteLoading : Bool
    , takeList : Int
    , resultLen : Int
    , page : Int
    , per_page : Int
    , pageNum : Int
    , paginate : Paginate
    , scrollCount : Float
    , scrap : Bool
    , zindex : String
    , need2login : Bool
    , detailShow : Bool
    , videoId : String
    , listData : YfD.DetailData
    , loading : Bool
    , errType : String
    }



type alias ListData = 
    { data : List DetailData 
    , paginate : Paginate }

type alias Paginate = 
    { difficulty_code : List String
    , exercise_part_code : String
    , page : Int
    , per_page : Int
    , total_count : Int }

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

detailEncoder: String -> List String -> Session -> Int -> Int -> Cmd Msg
detailEncoder part level session page per_page = 
    let
        list = 
            Encode.object
                [ ("exercise_part_code", Encode.string part)
                , ("difficulty_code", (Encode.list Encode.string) level)
                , ("page", Encode.int page)
                , ("per_page", Encode.int per_page)]
    
        body =
            list
                |> Http.jsonBody
    in
    (Decoder.yourfitDetailListData ListData DetailData Paginate)
    |> Api.post Endpoint.yfDetail (Session.cred session) GetList body 

init : Session -> Bool -> (Model, Cmd Msg)
init session mobile =
     (
        { session = session
        , check = mobile
        , isActive = "" 
        , takeList = 5
        , resultLen = 0
        , exercise_part_code = ""
        , difficulty_code = []
        , videoId = ""
        , infiniteLoading = False
        , page = 1
        , scrap = False
        , need2login = False
        , detailShow = False
        , zindex = ""
        , per_page = 10
        , scrollCount = 0
        , loading = False
        , listData = 
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
        , paginate = 
            { difficulty_code = []
            , exercise_part_code = ""
            , page = 0
            , per_page = 0
            , total_count = 0 }
        , pageNum = 1
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , listyourfitData = []
        , levelData = []
        , partDataName = []
        , errType = ""
        },
         Cmd.batch
            [ Decoder.levelDecoder LevelData Level
            |> Api.get GetLevel Endpoint.level (Session.cred session) 
            ,Decoder.yourfitList YfE.YourFitList YfE.ListData YfE.ExerciseList 
            |> Api.get GetPart Endpoint.yourfitVideoList (Session.cred session)
            , Api.removeJw ()
            , mydata session
            ]
    )
mydata: Session -> Cmd Msg
mydata session = 
    Decoder.sessionCheckMydata
        |> Api.get MyInfoData Endpoint.myInfo (Session.cred session)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Api.receiveKey ReceiveId
    , Api.successId Success
    , Session.changes GotSession (Session.navKey model.session)
    , Api.touch ReceiveScroll]

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
    | MyInfoData (Result Http.Error Decoder.DataWrap)
    | PageBtn (Int, String)
    | ReceiveScroll Encode.Value
    | Scrap
    | ScrapComplete (Result Http.Error Decoder.Success)
    | NoOp
    | GoVideo (List YfD.Pairing)
    | GetListData (Result Http.Error YfD.GetData)
    | DetailBack

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

scrollEvent: (ScreenInfo -> msg) -> Attribute msg
scrollEvent msgscroll = 
    on "scroll" (Decode.map msgscroll scrollInfoDecoder)


scrollInfoDecoder : Decoder ScreenInfo
scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DetailBack ->
            if model.need2login then
                ({model |  need2login = False}, Cmd.none)
            else
            ({model | detailShow = False, need2login = False, zindex = ""}, Api.hideFooter ())
        GetListData (Ok ok) -> 
             ({model | listData = ok.data, scrap = False, loading = False}, scrollToTop NoOp)
        GetListData (Err err) -> 
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        NoOp ->
            (model, Cmd.none)
        ScrapComplete (Ok ok) ->
            let
                text = Encode.string "스크랩 되었습니다."
            in
            
            ({model | scrap = not model.scrap}, Api.showToast text )
        ScrapComplete (Err err) ->
            let
                error = Api.decodeErrors err
                cannotScrap = Encode.string "이미 스크랩 되었습니다."
            in
            if error == "401" then
                ({model | need2login = True, detailShow = False, errType = "scrap"}, 
                Cmd.batch [
                    Api.hideFooter () 
                    , scrollToTop NoOp
                    , (Session.changeInterCeptor (Just error ) model.session)
                ])
            else
                (model, Api.showToast cannotScrap)
        Scrap ->
            (model, 
            Decoder.resultD
            |> Api.get ScrapComplete (Endpoint.scrap model.videoId)(Session.cred model.session) )
        GoVideo pairing->
            let
                videoList = 
                    Encode.object 
                        [("pairing", (Encode.list videoEncode) pairing) ]

                videoEncode p=
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)
                        ]
            in
             ({model | zindex = "zindex"}, Api.videoData videoList)
        ReceiveScroll scr ->
            case Decode.decodeValue Decode.float scr of
                Ok ok ->
                    let 
                        endOfPage =  model.paginate.total_count // model.per_page 
                    in
                    if model.scrollCount /= ok then
                        if model.page < (endOfPage + 1) then
                        ({model | scrollCount = ok, infiniteLoading = True, page = model.page + 1}, detailEncoder model.exercise_part_code model.difficulty_code model.session (model.page + 1) model.per_page)
                        else
                        ({model | scrollCount = ok }, Cmd.none)
                    else
                        ({model | scrollCount = ok }, Cmd.none)
                Err err ->
                    (model, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, detailEncoder model.exercise_part_code model.difficulty_code model.session idx model.per_page)
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, detailEncoder model.exercise_part_code model.difficulty_code model.session idx model.per_page)
                "go" -> 
                    ({model | page = idx}, detailEncoder model.exercise_part_code model.difficulty_code model.session idx model.per_page)
                _ ->
                    (model, Cmd.none)
        MyInfoData (Ok ok) ->
            (model, Cmd.none) 
        MyInfoData (Err err) ->
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            if serverErrors == "401" then
            ({model | errType = "myInfo"}, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            , case model.errType of
                "scrap" ->
                    mydata session
            
                "myInfo" ->
                    Decoder.resultD
                    |> Api.get ScrapComplete (Endpoint.scrap model.videoId)(Session.cred session)
                _ ->
                    mydata session
            )
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
                (model, Cmd.none)
        GetPart (Ok ok) ->
            ({model | partDataName = ok.data}, Cmd.none)
                
            
        GetPart (Err err) ->
            (model, Cmd.none) 
        BackPage ->
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.YourFitExer
            )
        Success str ->
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.YourfitDetail
            -- Api.historyUpdate (Encode.string "yourfitDetail")
            )
        DetailGo id ->
            let
                encodeId = Encode.int id
                stringId = String.fromInt id
            in
            if model.check then
                ({model | detailShow = True,  videoId = stringId}, 
                    Cmd.batch[(Decoder.yfDetailDetail YfD.GetData YfD.DetailData YfD.DetailDataItem YfD.Pairing)
                    |>Api.get GetListData (Endpoint.yfDetailDetail (stringId) ) (Session.cred model.session) 
                    , Api.hideFooter () ]
                    )
            else
            (model, Api.saveId (encodeId))
        GetLevel (Ok ok) ->
            ({model | levelData = ok.data}, Api.getKey () )
        GetLevel (Err err) ->
            (model, Cmd.none)
        ReceiveId id ->
            let 
                idDecode = Decode.decodeValue Decode.string id
            in
                case idDecode of
                    Ok ok ->
                        ({model | exercise_part_code = ok}, detailEncoder ok model.difficulty_code model.session model.page model.per_page ) 
                
                    Err err ->
                         (model, Api.getKey ())
            
        GetList (Ok ok) ->
            let
                makeList = 
                    List.take model.takeList ok.data
            in
            if model.check then
            ({model | listyourfitData = ok.data ++ model.listyourfitData, infiniteLoading = False, resultLen = List.length (ok.data), paginate = ok.paginate}, Cmd.none)
            else
            ({model | listyourfitData = ok.data, infiniteLoading = False, resultLen = List.length (ok.data), paginate = ok.paginate}, Cmd.none)
        GetList (Err err) ->
            (model, Cmd.none)
        IsActive level ->
            if level == "" then
            ( {model | isActive = level}, detailEncoder model.exercise_part_code [] model.session model.page model.per_page)
            else
                if model.check then
                ( {model | isActive = level, listyourfitData = []}, detailEncoder model.exercise_part_code [level] model.session 1 model.per_page )
                else
                ( {model | isActive = level}, detailEncoder model.exercise_part_code [level] model.session 1 model.per_page )

findFilterName: Model -> Maybe YfE.ListData
findFilterName model=
    List.head (
        List.filter (\x -> 
            x.code == model.exercise_part_code
        ) model.partDataName
    )
view : Model -> {title : String , content : Html Msg}
view model =
    case findFilterName model of
        Just result ->
            if model.check then
                if List.length(model.listyourfitData) > 0 then
                { title = "유어핏 운동"
                , content = 
                   div [] [ 
                    div [] [
                    appHeaderRDetailClick result.name "yourfitListDetail yourfitHeader" BackPage "fas fa-angle-left"
                    , div [class "heightFix"] [
                        apptapbox model
                    ]  
                    , app model
                    ]
                    ,div [class ("container myaccountStyle " ++ if model.need2login then "account yfdetailShow" else ""), id (if model.need2login then "noScrInput" else "")] [
                        appHeaderRDetailClick "로그인" "yourfitHeader" DetailBack "fas fa-times"
                        , need2loginAppDetail DetailBack
                    ]
                    , YfD.app model DetailBack Scrap GoVideo
                    ]
                } 
                else
                { title = "유어핏 운동"
                , content = 
                    div [] [
                    appHeaderRDetailClick result.name "yourfitListDetail yourfitHeader" BackPage "fas fa-angle-left"
                    ,nocontentsapp model
                    ]
                } 
            else
                if List.length(model.listyourfitData) > 0 then
                    { title = "유어핏 운동"
                    , content = 
                     div [ class "containerwrap" ]
                    [ div [ class "container" ]
                        [ div [ class "notification yf_workout" ]
                            [
                                commonHeader "/image/icon_workout.png" result.name,
                                div [ class "yf_yfworkout_search_wrap" ]
                                [
                                    tapbox model,
                                    div [ class "earchbox_wrap" ]
                                        [ 
                                            div [ class "yf_searchbox" ]
                                            (List.map contentsLayout model.listyourfitData)
                                        ]
                                    ,pagination
                                    PageBtn
                                    model.paginate
                                    model.pageNum
                                            ]                                   
                            ]
                        ] 
                    ]}
                else
                { title = "유어핏 운동"
                , content = 
                    div [ class "containerwrap" ]
                    [ div [ class "container" ]
                        [ div [ class "notification yf_workout" ]
                            [
                                commonHeader "/image/icon_workout.png" result.name,
                                div [ class "yf_yfworkout_search_wrap" ]
                                [
                                    tapbox model,
                                    div [ class "earchbox_wrap" ]
                                        [ 
                                            div [class "yf_noResult"] [
                                                text "검색결과가 없습니다."
                                            ]
                                        ]
                                ]                                   
                            ]
                        ] 
                    ]
                } 
    
        Nothing ->
            if model.check then 
                { title = "유어핏 운동"
                , content = 
                    div [][
                    appHeaderRDetailClick "" "yourfitListDetail yourfitHeader" BackPage "fas fa-angle-left"
                    , div [][text "컨텐츠를 불러올수 없습니다."]
                    ]
                }
                
            else
                { title = "유어핏 운동"
                , content = div [class "spinnerBackWeb"] [spinner]
                }
                
app: Model -> Html Msg
app model =    
    div ([ class "container" ] )
    [    
        div ([ class "searchbox scrollHegiht", id "searchHeight" ] ++ [scrollEvent ScrollEvent])
        [ div [class " togetherscrollContent"] (List.indexedMap contentsLayout2 model.listyourfitData)
        , div [class "loadingPosition", style "display" (if model.infiniteLoading then "block" else "none")] [
        spinner
        ]
        ]
                                            
    ]
nocontentsapp : Model -> Html Msg
nocontentsapp model =    
    div ([ class "container" ] ++ [style "max-height" "100%"])
    [  div [class "heightFix"] [
        apptapbox model
    ],  
        div [class "noResult"] [
            text "검색결과가 없습니다."
        ]                       
    ]
tapbox : Model -> Html Msg
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
apptapbox: Model -> Html Msg
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
            
goBtnBox: Msg -> Html Msg
goBtnBox backPage = 
    div [ class "searchbox_footer" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick backPage]
                [ text "뒤로" ]
            ]
        ]
contentsLayout: DetailData -> Html Msg  
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
contentsLayout2: Int -> DetailData -> Html Msg
contentsLayout2 idx item = 
    div [ class "column is-multiline m_videoboxwrap" , onClick (DetailGo item.id)]
        [ div [ class "m_work_videobox" ]
            [ 
                img [ class "appvpic1", src item.thembnail, alt "dummy_video_image" ]
                []
            ]
        , div [class "m_workout_title"] [
            div [class"m_list_title"]
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
                        , div [ class "" ]
                    [ text item.duration ]
                ]
        ]
        ]
