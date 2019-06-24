module Page.MyPageMenu.PaperWeightList exposing (..)
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
        , infiniteLoading : Bool
        , checkList : List String
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
        , showMenu : Bool
        , getList : List PaperWeightList
        , pagenation : ListPagenate
    }


type alias PaperweightData = 
    { data : List PaperWeightList
    , paginate : ListPagenate }

type alias PaperWeightList = 
    { bought_at : String
    , detail : List DetailPaperWeight 
    , end_at : String
    , is_buy : Bool
    , product_code : String
    , product_id : Int
    , product_no : Int
    , start_at : String }

type alias DetailPaperWeight = 
    { difficulty_name : String
    , duration : String
    , exercise_part_name : String
    , id : Int
    , thembnail : String
    , title : String}

type alias ListPagenate = 
    { page : Int 
    , per_page : Int
    , total_count : Int
    , user_id : Int }


paperweightEncoder page per_page session=
    let
        body = Encode.object 
            [ ("page", Encode.int page)
            , ("per_page", Encode.int per_page)]
                |> Http.jsonBody
    in
    Api.post Endpoint.myPaperweightList (Session.cred session) GetList body (Decoder.myPaperweightList PaperweightData PaperWeightList DetailPaperWeight ListPagenate)


-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , page = 1
        , per_page = 10
        , check = mobile
        , checkList = []
        , count = 1
        , loading = True
        , pageNum = 1
        , zindex = ""
        , scrap = False
        , videoId = ""
        , infiniteLoading = False
        , showMenu = False
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
        , getList = []
        , pagenation = 
            { page = 1 
            , per_page = 10
            , total_count = 0
            , user_id = 0 }
        }
        , Cmd.batch [
            paperweightEncoder 1 10 session
            , Api.removeJw ()
            , Api.mypageMenu (Encode.bool False)
        ]
    )



type Msg 
    = NoOp
    | GetCodeId (String, Int)
    | SaveComplete Encode.Value
    | GotSession Session
    | PageBtn (Int, String)
    | GetListData (Result Http.Error MyD.GetData)
    | GoVideo (List MyD.Pairing)
    | BackBtn
    | VideoEnd Encode.Value
    | VideoRecordComplete (Result Http.Error Decoder.Success)
    | ClickRight
    | ClickLeft
    | GoAnotherPage
    | ShowMenu
    | GetList (Result Http.Error PaperweightData)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


onLoad msg =
    on "load" (Decode.succeed msg)



 
    
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.successId SaveComplete
    , Session.changes GotSession (Session.navKey model.session)
    , Api.videoWatchComplete VideoEnd]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetList (Ok ok) ->
            ({model |  getList = ok.data, pagenation = ok.paginate, loading = False}, Cmd.none)
        GetList (Err err) ->
            (model, Cmd.none)
        ShowMenu ->
            ({model | showMenu = not model.showMenu}, Cmd.none)
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (Encode.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
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
            ({model | showDetail = False, zindex = "" }, Api.hideFooter ())
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
                    ({model | page = idx, pageNum = model.pageNum - 1}, paperweightEncoder idx model.per_page model.session)
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, paperweightEncoder idx model.per_page model.session)
                "go" -> 
                    ({model | page = idx}, paperweightEncoder idx model.per_page model.session)
                _ ->
                    (model, Cmd.none)
        GotSession session ->
            ({model | session = session}, paperweightEncoder model.page model.per_page model.session
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
        -- OnLoad ->
        --     if model.count >= List.length model.data.data then
        --     ({model | loading = False}, Cmd.none)
        --     else
        --     ({model | count = model.count + 1}, Cmd.none)
        
        NoOp ->
            ( model, Cmd.none )
        

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
            { title = "문진운동 "
            , content = 
                div [] [
                        div [class ("topSearch_container " ++ (if model.showDetail then "fadeContainer" else ""))] [
                        appHeaderRDetail "문진운동 리스트" "myPageHeader  whiteColor" Route.MyPage "fas fa-angle-left", 
                        div [class "spinnerBack", style "display" (if model.loading then "flex" else "none" )] [
                            spinner
                            ]
                        , div [class "noResult", style "display" (if List.isEmpty model.getList then "fixed" else "none")] [
                                text "스크랩한 게시물이 없습니다."
                            ]
                        , div [ class "scrollheight" ] 
                            -- [listappDetail model]
                        (
                                List.map (\x -> listwebDetail x model) model.getList
                            )
                        , div [class "loadingPosition", style "display" (if model.infiniteLoading then "block" else "none")] [
                            infiniteSpinner
                            ]
                        ]
                        , div [class ("myaccountStyle myScrapStyle " ++ (if model.showDetail then "account" else "")) ][MyD.app model BackBtn GoVideo]
                ]
            }
        else
        { title = "문진운동 "
        , content = 
            div [  ]
                [
                    div [class "mypageHiddenMenu", onClick ShowMenu] []
                    , div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage model.showMenu]
                    ,div [class "container"]
                    [ commonJustHeader "/image/icon_list.png" "문진운동 ",
                    div [ class "yf_yfworkout_search_wrap" ]
                    [
                        div [] [
                            div [class "myScrap_mediabox"]
                             (
                            List.map (\x -> listwebDetail x model) model.getList
                            )
                        , pagination 
                            PageBtn
                            model.pagenation
                            model.pageNum
                        ]
                    ]
                ]]
        }

listwebDetail item model = 
   div [] 
   (
        List.map (\x -> videoItem x model item ) item.detail
    )
listappDetail item model = 
    div [] 
    (
        List.map (\x -> videoItem x model item ) item.detail
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
 

videoItem item model getlist= 
    div [ class "mjList_container" ]
        [ div [class"list_overlay"]
        [i [ class "fas fa-play overlayplay_list" ][]],

            div [class "mj_wrap"][
                 div [ class "yf_workoutvideo_image" ]
                [ 
                    img [ class "yf_workoutvpic1", src item.thembnail ]
                    []
                ]
            , div [ class "yf_workoutvideo_lavel_bg" ]
                [ div [ class "level" ]
                    [ text item.difficulty_name ]
                ]
            ]
            , div [class "mjList_title"][
            div [ class "yf_workoutworkout_title" ]
                [ text item.title 
                , div [][
                            text item.exercise_part_name
                        ]
                    ]
            , div [][
                        i [ class "fas fa-stopwatch" ]
                        []
                        , text " "
                        , text item.duration
                    ]
            , div [ class ( if model.check then "notRow" else "") ]
                [
                    p [class ("limitedDate " ++ if model.check then "notMargin" else " rowPaperWeight")]
                    [ p [][text ("( " ++ getlist.start_at ++ " ~ " ++ getlist.end_at ++  " )")]
                    ]
                ]
                    
            ]
        ]
