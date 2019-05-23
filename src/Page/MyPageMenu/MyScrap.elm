module Page.MyPageMenu.MyScrap exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port exposing(..)
import Api as Api
import Route as Route
import Json.Encode as Encode
import Http as Http
import Api.Endpoint as Endpoint
import Json.Decode as Decode
import Api.Decoder as Decoder
import Page.Detail.MyScrapDetail as MyD

type alias Model 
    = {
        session : Session
        , check : Bool
        , data : Data
        , infiniteLoading : Bool
        , checkList : List String
        , screenInfo : ScreenInfo
        , dataList : List DataList
        , page : Int
        , per_page :Int
        , count : Int
        , loading : Bool
        , pageNum : Int
        , zindex : String
        , listData : MyD.DetailData
        , scrap : Bool
        , showDetail : Bool
        , videoId : String
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}


type alias Data = 
    { data : List DataList
    , paginate : Paginate }

type alias DataList = 
    { detail : List DetailData 
    , scrap_code : String
    , scrap_id : Int
     }

type alias DetailData = 
    { id : Int
    , lookup :Int
    , lookup_at : Maybe String
    , mediaid : String
    , thembnail : String
    , title : String}

type alias Paginate = 
    { page : Int
    , per_page : Int
    , total_count : Int
    , user_id : Int }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , page = 1
        , per_page = 10
        , check = mobile
        , dataList = []
        , checkList = []
        , count = 1
        , loading = True
        , pageNum = 1
        , zindex = ""
        , scrap = False
        , videoId = ""
        , infiniteLoading = False
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , data = 
            { data = []
            , paginate = 
                { page = 0
                , per_page = 0
                , total_count = 0
                , user_id = 0
            }
        }
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
        , showDetail = False
        }
        , Cmd.batch [
            scrapDataEncoder 1 10 session
            , Api.removeJw ()
        ]
    )



type Msg 
    = NoOp
    | GetList (Result Http.Error Data)
    | ScrollEvent ScreenInfo
    | OnLoad
    | GetCodeId (String, Int)
    | SaveComplete Encode.Value
    | GotSession Session
    | PageBtn (Int, String)
    | GetListData (Result Http.Error MyD.GetData)
    | GoVideo (List MyD.Pairing)
    | BackBtn
    | VideoEnd Encode.Value
    | VideoRecordComplete (Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

onLoad msg =
    on "load" (Decode.succeed msg)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

scrapDataEncoder page per_page session = 
    let
        list = 
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page)]
                    |> Http.jsonBody
    in
    (Decoder.myscrapData Data DataList DetailData Paginate)
    |> Api.post Endpoint.scrapList (Session.cred session) GetList list 
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.successId SaveComplete
    , Session.changes GotSession (Session.navKey model.session)
    , Api.videoWatchComplete VideoEnd]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VideoRecordComplete (Ok ok) ->
            (model, Cmd.none)
        VideoRecordComplete (Err err) ->
            (model, Cmd.none)
        VideoEnd complete ->
            let
                decodestr = Decode.decodeValue Decode.string complete
            in
                case decodestr of
                    Ok ok ->
                        (model, Api.get VideoRecordComplete  (Endpoint.videoCompleteRecord model.videoId)  (Session.cred model.session) Decoder.resultD)
                
                    Err err ->
                        (model, Cmd.none)
        BackBtn ->
            ({model | showDetail = False, zindex = "" }, Cmd.none)
        GoVideo pairing ->
            let 
                videoList = 
                    Encode.object 
                        [("pairing", (Encode.list videoEncode) model.listData.pairing) ]

                videoEncode p=
                    Encode.object
                        [ ("file", Encode.string p.file)
                        , ("image", Encode.string p.image)
                        , ("title", Encode.string p.title)
                        ]
            in
            
            ({model | zindex = "zindex"}, Api.videoData videoList)
        GetListData (Ok ok) -> 
            ({model | listData = ok.data, scrap = False, loading = False, showDetail = True}, Cmd.none)
        GetListData (Err err) -> 
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, scrapDataEncoder idx model.per_page model.session)
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, scrapDataEncoder idx model.per_page model.session)
                "go" -> 
                    ({model | page = idx}, scrapDataEncoder idx model.per_page model.session)
                _ ->
                    (model, Cmd.none)
        GotSession session ->
            ({model | session = session},
            scrapDataEncoder model.page model.per_page session
            )
        SaveComplete complete ->
            let
                save = Decode.decodeValue Decode.string complete
            in
                case save of
                    Ok ok ->
                        (model, 
                        Route.pushUrl (Session.navKey model.session) Route.ScrapD
                        -- Api.historyUpdate (Encode.string "myScrapDetail")
                        )
                
                    Err _ ->
                       (model, Cmd.none) 
            
        GetCodeId (code, id ) ->
            let 
                stringInt = String.fromInt (id)
                codeIdEncoder = 
                    Encode.object
                        [("code", Encode.string code)
                        , ("id", Encode.string stringInt)]
            in
            if model.check then
            ({model | videoId = stringInt }, 
                Decoder.yfDetailDetail MyD.GetData MyD.DetailData MyD.DetailDataItem MyD.Pairing
                    |>Api.get GetListData (Endpoint.scrapDetail code stringInt) (Session.cred model.session) )
            else
            (model, Api.saveId codeIdEncoder )
        OnLoad ->
            if model.count >= List.length model.data.data then
            ({model | loading = False}, Cmd.none)
            else
            ({model | count = model.count + 1}, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                -- case toInt of
                --     Just val ->
                        -- if (val  < (model.takeList + 10)) then
                        --     ({model | takeList = val, infiniteLoading = False},Cmd.none)
                        -- else 
                if List.length model.checkList > 0 then
                (model, Cmd.none)
                else
                ({model | infiniteLoading = True}, scrapDataEncoder model.page model.per_page model.session)
                    -- Nothing ->
                    --     (model, Cmd.none)
                
            else
                (model, Cmd.none)
        NoOp ->
            ( model, Cmd.none )
        GetList (Ok ok) -> 
            -- if model.check then
                if ok.data == [] then
                    ({model | infiniteLoading = False, checkList = ["empty"], loading = False}, Cmd.none)
                else
                    ({model | data = ok, dataList = model.dataList ++ ok.data, page = model.page + 1, infiniteLoading = False, loading = False}, Cmd.none)
            -- else
            --     ({model | data = ok}, (scrollToTop NoOp))
        GetList (Err err) -> 
            let
                serverErrors = 
                    Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else 
            (model, Route.load ("#/myScrap"))

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
            { title = "나의 스크랩"
            , content = 
                div [] [
                        div [class ("topSearch_container " ++ (if model.showDetail then "fadeContainer" else ""))] [
                        appHeaderRDetail "나의 스크랩리스트" "myPageHeader  whiteColor" Route.MyPage "fas fa-angle-left", 
                        div [class "spinnerBack", style "display" (if model.loading then "flex" else "none" )] [
                            spinner
                            ]
                        , div [class "noResult", style "display" (if List.isEmpty model.data.data then "fixed" else "none")] [
                                text "스크랩한 게시물이 없습니다."
                            ]
                        , div [ class "scrollheight", scrollEvent ScrollEvent ] (
                                List.map listappDetail model.dataList
                            )
                        , div [class "loadingPosition", style "display" (if model.infiniteLoading then "block" else "none")] [
                            infiniteSpinner
                            ]
                        ]
                        , div [class ("myaccountStyle myScrapStyle " ++ (if model.showDetail then "account" else "")) ][MyD.app model BackBtn GoVideo]
                ]
            }
        else
        { title = "나의 스크랩"
        , content = 
            div [ class "container" ]
                [
                    commonJustHeader "/image/icon_list.png" "나의 스크랩",
                    div [ class "yf_yfworkout_search_wrap" ]
                    [
                        div [] [
                            div [class "myScrap_mediabox"] (
                            List.map listwebDetail model.data.data
                        )
                        , pagination 
                        PageBtn
                        model.data.paginate
                        model.pageNum
                        ]
                    ]
                ]
        }

listwebDetail item = 
   div [onClick (GetCodeId (item.scrap_code, item.scrap_id))] (
        List.map scrapItem item.detail
    )
listappDetail item = 
    div [onClick (GetCodeId (item.scrap_code, item.scrap_id))] (
        List.map appcontent item.detail
    )

appcontent item= 
        div [ class "containerm_mypage_scrap" ]
        [ div []
            [ div [ class "yf_box m_yf_box_scrap" ] 
                [ img [ src item.thembnail ]
                    []
                , div [ class "m_scrap_boxtext" ]
                    [ ul []
                        [ li [ class "m_scrap_box_name" ]
                            [ text item.title ]
                        ,li [ class "m_scrap_date" ]
                            [ text (String.dropRight 15 (justData(item.lookup_at))) ]
                        ]
                    ]
                ]
            ]
        ]

justData just =
    case just of
        Just a ->
            a
    
        Nothing ->
            ""
contentsCount count=
    div []
        [ div [ class "myScrap_yf_large" ]
            [ text ("총 "++ String.fromInt count ++" 건의 결과") ]
        ]
 

scrapItem item= 
    div [ class "myScrap_yf_box" ]
        [ img [ src item.thembnail ]
            []
        , i [ class "fas fa-bookmark myScrap_yf_icon_mark" ]
            []
        , div [ class "myScrap_box_wrap" ]
            [ div [ class "myScrap_yf_box_title" ]
                [ text item.title ]
            , ul []
                [  li [ class "myScrap_box_date" ]
                    [ text (String.dropRight 15 (justData(item.lookup_at)))]
                ]
            ]
        ]
