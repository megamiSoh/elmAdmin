module Page.Together exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Html.Lazy exposing (lazy, lazy2)
import Page as Page
import Task

type alias Model 
    = {
        session : Session,
        isActive : String,
        checkDevice : String
        , check : Bool
        , page : Int
        , per_page : Int
        , like : Int
        , scrap : Bool
        , count : Int
        , togetherData : TogetherDataWrap
        , screenInfo : ScreenInfo
        , loading : Bool
        , infiniteLoading : Bool
        , appData : List TogetherData
        , ofheight : Int
        , need2login : Bool
        , videoStart : String
        , showAllText : Int
        , likeList : List Int
        , idx : Int
        , showDetail : Bool
        , pageNum : Int
        , height : String
        , oneOfdata : List TogetherData
        , cnt : Int
    }
type alias TogetherDataWrap = 
    { data : List TogetherData 
    , paginate : Paginate
    }

type alias TogetherLikeWrap = 
    {data : TogetherData}
type alias TogetherData = 
    { content : Maybe String
    , detail : Maybe (List DetailTogether)
    , id : Int
    , inserted_at : String
    , is_delete : Bool
    , link_code : String
    , recommend_cnt : Int
    , nickname : Maybe String
    , profile : Maybe String
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
type alias Paginate = 
    { page : Int
    , per_page : Int
    , total_count : Int }
type alias Like = 
    { data : LikeData}

type alias LikeData = 
    { count : Int }
type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    =
     (
        { session = session
        , need2login = False
        , showDetail = False
        , infiniteLoading = False
        , appData = []
        , ofheight = 0
        , count = 1
        , likeList = []
        , idx = 0
        , pageNum = 1
        , height = ""
        , cnt = 0
        , oneOfdata = []
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , isActive = "All"
        , checkDevice = ""
        , check = mobile
        , page = 1
        , videoStart = ""
        , per_page = 10
        , loading = True
        , showAllText = 0
        , like = 0
        , scrap = False
        , togetherData = 
            { data = []
            , paginate = 
                { page = 0
                , per_page = 0
                , total_count = 0 }
            }
        }
        ,  Cmd.batch 
        [  dataEncoder 1 10 session]
    )

dataEncoder page perpage session=
    let
        list = 
            Encode.object 
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int perpage)]
                |> Http.jsonBody

    in
    (Decoder.togetherdatawrap  TogetherDataWrap TogetherData DetailTogether Paginate TogetherItems Pairing)
    |> Api.post Endpoint.togetherList (Session.cred session) GetData list 

loadingEncoder page perpage session = 
    let
        list = 
            Encode.object 
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int perpage)]
                |> Http.jsonBody

    in
    (Decoder.togetherdatawrap  TogetherDataWrap TogetherData DetailTogether Paginate TogetherItems Pairing)
    |>Api.post Endpoint.togetherList (Session.cred session) LoadingGetData list 

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)    
onLoad msg =
    on "load" (Decode.succeed msg)
likeApi session id = 
    (Decoder.togetherdatalikewrap  TogetherLikeWrap TogetherData DetailTogether  TogetherItems Pairing)
    |> Api.get LikeUpdate (Endpoint.togetherlike id) (Session.cred session) 
type Msg 
    = IsActive String
    | CheckDevice Encode.Value
    | GetData (Result Http.Error TogetherDataWrap)
    -- | Loading Encode.Value
    | IsLike (Int, Int)
    | LikeComplete (Result Http.Error Like)
    | PageBtn (Int, String)
    | ScrollEvent ScreenInfo
    | LoadingGetData (Result Http.Error TogetherDataWrap)
    | ScrapComplete (Result Http.Error Decoder.Success)
    | Scrap Int
    | VideoCall ((List Pairing) ,Int)
    | OnLoad
    | ShowAllText Int
    | LikeUpdate (Result Http.Error TogetherLikeWrap)
    | TogetherDetail Int
    | NoOp
    | GetHeight Encode.Value
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Api.getHeightValue GetHeight
    -- Api.videoSuccess Loading

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetHeight height ->
            let
                heightDecoding =
                    Decode.decodeValue Decode.string height
            in
            case heightDecoding of
                Ok ok ->
                    ({model | showDetail = not model.showDetail, height = ok}, Cmd.none)        
                Err err ->
                    (model, Cmd.none)
        NoOp ->
            (model, Cmd.none)
        TogetherDetail id ->
            let
                getData = 
                    List.filter (\x -> 
                        x.id == id
                    )model.togetherData.data
            in
            if id == 0 then
            ({model | showDetail = not model.showDetail}, Api.getscrollHeight (Encode.bool (not model.showDetail)))
            else 
            ({model | oneOfdata = getData, showDetail = not model.showDetail}, Api.getscrollHeight (Encode.bool (not model.showDetail)))
        LikeUpdate (Ok ok) ->
            let
                udtLike = 
                    List.map (\x ->
                        if x.id == model.like then
                            {x | recommend_cnt = ok.data.recommend_cnt}
                        else
                            x
                    )model.appData 
                webLike = 
                    List.map (\x ->
                            {x | recommend_cnt = ok.data.recommend_cnt}
                    )model.oneOfdata
                webAllupdate = 
                    List.map (\x ->
                        if x.id == model.like then
                            {x | recommend_cnt = ok.data.recommend_cnt}
                        else
                            x
                    )model.togetherData.data
                
                webR = model.togetherData
                result = {webR | data = webAllupdate} 
            in
            ({model | appData = udtLike,oneOfdata = webLike, togetherData = result}, Cmd.none)
        LikeUpdate (Err err) ->
            (model, Cmd.none)
        ShowAllText id->
            ({model | showAllText = id}, Cmd.none)
        OnLoad ->
            if model.count >= List.length (model.togetherData.data) then
            ({model | loading = False}, Cmd.none)
            else
            ({model | count = model.count + 1, loading = False}, Cmd.none)
        VideoCall (p ,idx) ->  
            let 
                stringint = String.fromInt(idx)
                pair = 
                        Encode.object
                            [ ("pairing",Encode.list pairing p)
                            , ("id", Encode.string stringint)]
                pairing x=
                    Encode.object
                        [ ("file", Encode.string x.file)
                        , ("image", Encode.string x.image)
                        , ("title", Encode.string x.title)]
                -- id = 
                --     Encode.string (String.fromInt(idx))
            in
            ({model | videoStart = stringint}, Api.togetherDataList pair)
        ScrapComplete (Ok ok) ->
            let
                text = Encode.string "스크랩 되었습니다."
            in
            
            ({model | scrap = not model.scrap}, Api.showToast text)
        ScrapComplete (Err err) ->
            let
                error = Api.decodeErrors err
                cannotScrap = Encode.string "이미 스크랩 되었습니다."
            in
            if error == "401" then
                ({model | need2login = True}, Cmd.none )
            else
                (model, Api.showToast cannotScrap)
        Scrap id ->
            (model, Api.get ScrapComplete (Endpoint.togetherScrap (String.fromInt(id)))(Session.cred model.session) Decoder.resultD)
        LoadingGetData (Ok ok) ->
            let
                page = Encode.int (model.page + 1)
            in
            if ok.data == [] then
            ({model| infiniteLoading = False, loading = False}, Cmd.none)
            else
            ({model | appData = model.appData ++ ok.data, page = model.page + 1, infiniteLoading = False, loading = False}, Cmd.none
            -- , Api.togetherDataList page 
            )
        LoadingGetData (Err err) ->
            ({model | loading = False}, Cmd.none)
        
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                ({model | infiniteLoading = True}, Cmd.batch[loadingEncoder (model.page + 1)  model.per_page model.session])
                
            else
                ({model | ofheight = offsetHeight}, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, Cmd.batch[dataEncoder idx model.per_page model.session])
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, Cmd.batch[dataEncoder idx model.per_page model.session])
                "go" -> 
                    ({model | page = idx}, Cmd.batch[dataEncoder idx model.per_page model.session])
                _ ->
                    (model, Cmd.none)
        LikeComplete (Ok ok) ->
            -- let
            --     filter = 
            --         List.filter(\x -> x.id ==)
            -- in

            -- if model.check then
                if ok.data.count < model.cnt then
                    let
                        listF = 
                            List.filter(\x->
                                model.like /= x
                            )model.likeList
                    in
                    ({model | likeList= listF}
                    , likeApi model.session (String.fromInt model.like))
                else
                    ({model | likeList= model.likeList ++ [model.like]}
                    , likeApi model.session (String.fromInt model.like))
            -- else
            --     if ok.data.count == 0 then
            --         let
            --             listF = 
            --                 List.filter(\x->
            --                     model.like /= x
            --                 )model.likeList
            --         in
            --         ({model | likeList= listF}
            --         , dataEncoder model.page model.per_page model.session)
            --     else
            --         ({model | likeList= model.likeList ++ [model.like]}
            --         , dataEncoder model.page model.per_page model.session)
            -- (model
            -- , dataEncoder model.page model.per_page model.session)
            -- else
            -- ({model | like = 0}
            -- , dataEncoder model.page model.per_page model.session)
        LikeComplete (Err err) ->
            let
                serverErrors = Api.decodeErrors err
            in
                if serverErrors == "401" then 
                ({model | need2login = True}, Cmd.none )
            else
                (model, Cmd.none)
        IsLike (id, idx )->
            ({model | like = id, cnt = idx}, 
            (Decoder.togetherLike Like LikeData)
             |> Api.get LikeComplete (Endpoint.togetherLike (String.fromInt id)) (Session.cred model.session) )
        -- Loading success ->
        --     let
        --         d = Decode.decodeValue Decode.string success
        --     in
        --         case d of
        --             Ok item ->
        --                 ({model | loading = False},Cmd.none)
        --                 -- (model, Cmd.none)
                
        --             Err _->
        --                  ({model | loading = False},Cmd.none)
        GetData (Ok ok) ->
            ({model | togetherData = ok, appData = ok.data, loading = False} , (scrollToTop NoOp) )
        GetData (Err err) ->
            ({model | loading = False}, Cmd.none)
        IsActive title ->
            ({model | isActive = title} , Cmd.none)
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
    
    title = "함께해요"
    , content = 
       
    if model.check then
       div [] [
           if model.loading then
            div [class "spinnerBack"] [
                spinner
                ]
            else 
            div [] []
            , 
            app model
       ]
       else 
            web model   
    }

justData item = 
    case item of
        Just val ->
            val
    
        Nothing ->
            "Guest"



web model = 
    div [
        ] [
                div [class "container",
                scrollEvent ScrollEvent][
                    if model.need2login then
                    need2loginAppDetailRoute Route.Together
                     else
                    div [class "notification yf_workout"] [
                        div [] [
                            commonHeader "../image/icon_together.png" "함께해요",
                        div [class "yf_yfworkout_search_wrap together"] [
                            -- tabbox model,
                            lazy contentsBody model
                        ]
                        
                            ]
                        ]
                    ]
            ]

app model =
     
    div [class "container "] [
        justappHeader "함께해요" "togetherHeader",
        div [scrollEvent ScrollEvent, id "scrollE" ] [
            if model.need2login then
            need2loginAppDetailRoute Route.Together
            else
            div [ class "scroll", scrollEvent ScrollEvent] [
                -- appTab model,
                appStartBtn,
                div [] (List.indexedMap (
                        \idx x -> appContentsItem idx x model ) model.appData )
                ,if model.infiniteLoading then
                div [class "loadingPosition"] [
                infiniteSpinner
                ]
                else
                span [] []
            ]
        ]
    ]

appTab model = 
        div [ class "m_to_menubox" ]
                  [ div [ classList [
                    ("m_together_yf_active" , model.isActive == "All")
                ], onClick (IsActive "All") ] 
                [ i [class "fab fa-amilia m_to_menubox_icon" ]
                    [], text "전체" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "people")
                ], onClick (IsActive "people") ] 
                [ i [class "fas fa-users m_to_menubox_icon" ]
                    [], text "피플" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "recipe")
                ], onClick (IsActive "recipe") ] 
                [ i [class "fas fa-utensils m_to_menubox_icon" ]
                    [], text "레시피" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "news")
                ], onClick (IsActive "news") ] 
                [ i [class "fas fa-file-alt m_to_menubox_icon" ]
                    [], text "뉴스" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "fitness")
                ], onClick (IsActive "fitness") ] 
                [ i [class "fas fa-dumbbell m_to_menubox_icon" ]
                    [], text "피트니스" 
                ]
            ]

            

appStartBtn = 
    div [ class "m_to_mediabox" ]
        [ div [ class "media-content m_to_yf_content" ]
            [ 
                text "내가 만든 운동을 공유하는 공간입니다."
                -- text "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요." 
            -- , p []
            --     [ a [ class "button is-dark m_to_edit_btn", Route.href Route.TogetherW ]
            --         [ i [ class "fas fa-edit" ]
            --             []
            --         ]
            --     ]
            ]
        ]

justListData item = 
    case item of
        Just a ->
            a
    
        Nothing ->
            []
appContentsItem idx item model=
    if item.is_delete then
     span [] []
    else
    let
        pairing = 
            List.head
                (
                    List.map(\x ->
                        x.pairing    
                    )(justListData item.detail)
                )  
        thumbnail = 
            List.head
                (
                    List.map(\x ->
                        x.thembnail   
                    )(justListData item.detail)
                ) 

    in
    -- <i class="fas fa-dumbbell"></i>
    div [ class "m_to_mediabox2" ]
        [   
            div [ class "m_to_yf_boxtop" ]
            [
            div [ class "m_to_yf_id" ]
                [ case item.profile of
                    Just image ->
                        img [src image] []
                
                    Nothing ->
                        i [ class "fas fa-user profile" ]
                        []
                , div [class "profileText"] [
                    strong []
                    [  text (justData item.nickname) ]
                    , div [class"togetherData"] [
                        text (String.dropRight 10 item.inserted_at)
                    ]
                ]
                ]
            ], 
            case thumbnail of
            Just thumb ->
                div [class"m_to_video"][
                    div ([class "thumbTest_m"]
                    )[
                        div [class "clickThis",onClick (VideoCall ((
                        case pairing of
                            Just a ->
                                a
                            Nothing ->
                                []
                    ), idx))] [
                        i [ class "far fa-play-circle m_to_video_circle",onClick (VideoCall ((
                        case pairing of
                            Just a ->
                                a
                            Nothing ->
                                []
                    ), idx)) ][]
                        , img [src thumb, onLoad OnLoad] []
                ],
                 div [id ("myElement" ++ String.fromInt(idx)) ]  [] ] 
            -- , div [ class "m_to_yf_more" ]
            --     [ div [ ]
            --         [ strong []
            --             [ text "자세히보기" ]
            --         ]
            --     ]
                ]
            Nothing ->
                div [][]
            , if String.length (justData item.content) > 100 then
                   div [class "togetherArticle"][
                        if model.showAllText == item.id then
                            text (justData item.content)
                        else
                            text (String.dropRight ((String.length(justData item.content) - 100)) (justData item.content) )
                            
                   , div [] [
                    ul [] [
                        li [] (List.map (\x-> detailData x model item.id) (justListData item.detail))
                    ]
                ]
                ]
                else
                   div [class "togetherArticle"]
                   [ text (justData item.content)
                   , div [] 
                    (List.map (\x-> detailData x model item.id) (justListData item.detail))
                ]
                -- ]
        -- , div [ class "m_to_yf_text" ]
        --     [ text (justData item.content )]
        --  , text (String.fromInt(String.length (justData item.content)))
        , div [ class "level-left m_to_yf_like", onClick (IsLike (item.id, item.recommend_cnt)) ]
            [ text( "좋아요" ++ String.fromInt (item.recommend_cnt) ++"개" ) 
            , 
            if List.member item.id model.likeList  then
                i [ class "fas fa-heart together_heart" ]
                []
            else 
                 i [ class "far fa-heart together_heart"]
                []
                
            ]

        -- --  div [ class "level-left m_to_yf_scrap", onClick (Scrap item.id) ]
        --     [
        --     --      text( "스크랩" ++ String.fromInt(item.isLike) ++"개" ) 
        --     -- , 
        --     -- i [ class "fas fa-bookmark together_bookmark" ]
        --     --     []
        --     ]
        
        ]
   
tabbox model=
    div [ class "tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large" ]
            [ ul []
                [ li [ classList [
                    ("together_yf_active" , model.isActive == "All")
                ], onClick (IsActive "All") ]
                    [ p []
                        [ span []
                            [ text "전체" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "people")
                ], onClick (IsActive "people")]
                    [ p []
                        [ span []
                            [ text "피플" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "recipe")
                ], onClick (IsActive "recipe")]
                    [ p []
                        [ span []
                            [ text "레시피" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "news")
                ], onClick (IsActive "news")]
                    [ p []
                        [ span []
                            [ text "뉴스" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "fitness")
                ], onClick (IsActive "fitness")]
                    [ p []
                        [ span []
                            [ text "피트니스" ]
                        ]
                    ]
                ]
            ]
        ]


contentsBody model=
    div [ class "together_searchbox_wrap" ]
        [ div [ class "together_searchbox" ]
            [ div [ class "together_mediabox" ]
                [ div [ class "media-content together_yf_content"]
                [ img [ src "image/takeimage.png", alt "takeimage" ]
                []
                ],
                   h1 [ class "to_yf_h2" ]
                   [ text "내가 만든 운동을 공유하는 공간입니다." ,
                     p []
                        [ 
                            -- a [ class "button is-dark together_edit_btn" , Route.href Route.TogetherW]
                            -- [ i [ class "fas fa-edit" ]
                            --     []
                            -- ]
                        ]
                    ]
                                 ]
            , if model.showDetail then
                div [class "togetherdetail"] [
                       div [class "togetherdetailItem"] (
                            List.indexedMap (\idx x ->
                            detailItem idx x model
                            ) model.oneOfdata
                       )
                ]
                else
                div [] []
            ,if List.length model.togetherData.data > 0 then
             div [] [
                 div [](List.indexedMap (
                    \idx x -> userItem idx x model
                ) model.togetherData.data)
                ,pagination 
                    PageBtn
                    model.togetherData.paginate
                    model.pageNum
             ]
            else
            div [class "noResult"] [text "게시물이 없습니다."]
            ]
        
        ]
detailItem idx item model =
    let
        pairing = 
            List.head
                (
                    List.map(\x ->
                        x.pairing    
                    )(justListData item.detail)
                )  
        thumbnail = 
            List.head
                (
                    List.map(\x ->
                        x.thembnail   
                    )(justListData item.detail)
                ) 
    in
    div [] [
        div [ class "together_yf_text togetherVideo" ]
                 
                [    div [class "button is-danger together_closebtn", onClick (TogetherDetail 0)] [text "닫기"],
                case thumbnail of
                    Just thumb ->
                        div ([class "thumbTest"]
                        )[  
                            div [class "clickThis",onClick (VideoCall ((
                            case pairing of
                                Just a ->
                                    a
                                Nothing ->
                                    []
                        ), item.id))] [
                            i [ class "fas fa-play-circle",onClick (VideoCall ((
                                case pairing of
                                    Just a ->
                                        a
                                    Nothing ->
                                        []
                            ), item.id)) ][]
                            , img [class "toImg2", src (justData thumbnail)] []
                        ],
                            div [id ("myElement" ++ String.fromInt(item.id)) ]  [] ]
                    Nothing ->
                        div [] [] 
                    , div [id ("together" ++ (String.fromInt(idx)))] [] 
                    , if String.length (justData item.content) > 100 then
                    div [][
                            if model.showAllText == item.id then
                                text (justData item.content)
                            else
                                text (String.dropRight ((String.length(justData item.content) - 100)) (justData item.content) )
                                
                    , div [] [
                        ul [] [
                            li [] (List.map (\x-> detailData x model item.id) (justListData item.detail))
                        ]
                    ]
                    ]
                    else
                    div [class"together_list"]
                    [ text (justData item.content)
                    , div [] 
                        (List.map (\x-> detailData x model item.id) (justListData item.detail))
                    ]
                    ]
                    
                
                    -- , text (String.fromInt(String.length (justData item.content)))
                    
                    -- , div [] [text "더보기"]
                    --  ]
            , div [class"together_nav"]
                [
                    div [class "level-left together_yf_like", onClick (IsLike (item.id, item.recommend_cnt))] [
                        text( "좋아요"++ String.fromInt (item.recommend_cnt) ++"개")  
                , if List.member item.id model.likeList  then
                i [ class "fas fa-heart together_heart" ]
                    []
                else  
                    i [ class "far fa-heart together_heart"]
                    []
                    
                    ]
                -- ,div [ class "level-left together_yf_scrap", onClick (Scrap item.id) ]
                -- [ 
                --     -- text( "스크랩"++ String.fromInt (item.isLike) ++"개")  , br []
                --     -- []
                -- -- ,
                -- i [ class "far fa-bookmark together_bookmark" ]
                --     []
                -- ]
                ]
                -- , div [class "button is-dark together_close", onClick (TogetherDetail 0)] [text "닫기"]
            ]
    
userItem idx item model =
    let
         thumbnail = 
            List.head
                (
                    List.map(\x ->
                        x.thembnail   
                    )(justListData item.detail)
                ) 
    in
    
    if item.is_delete then
    span [] []
    else
    
    div [class"together_grid"] [
         div [ class "together_mediabox2", onClick (TogetherDetail item.id) ]
        [ div [ class "together_yf_boxtop" ]
            [ p [ class "image is-64x64 together_imgbox" ]
                [ 
                    case item.profile of
                        Just a ->
                            img [src a ] []
                        Nothing ->
                            img [ src "../image/profile.png" ]
                            []
                ]
            , div [ class "together_yf_id" ]
                [ strong []
                    [ text (justData item.nickname) ]
                , div [] [text (String.dropRight 10 item.inserted_at)]
                ]
            ]
        , img [class "toImg", src (justData thumbnail)] []
        ]
        
    ]
   

detailData item model id= 
    div[class "detailData"] [
   
        -- if List.length item.exercise_items > 3 then
        --  ul[] [
        --     li [] (List.indexedMap (\idx x->  exerciseItems idx x model id) (List.sortBy .sort item.exercise_items))
        --     , li [] [
        --         if model.showAllText == id then
        --             div [onClick (ShowAllText id), class "moreText cursor" ][text "닫기"] 
        --         else
        --             div [onClick (ShowAllText id), class "moreText cursor" ][text " ...자세히보기"] 
        --         ]
        --     ]
        -- else 
        ul [] [
         li [] 
         (List.indexedMap (\idx x->  exerciseItems idx x model id) (List.sortBy .sort item.exercise_items))]
     ]
   
exerciseItems idx item model id=
    li [
        -- classList 
        -- [ ("hideList", (idx >= 3))
        -- , ("allShow", model.showAllText == id)
        -- ]
    ] [
        text ((String.fromInt (item.sort)) ++ ". " ++ item.title ++ " x " ++ (
            if item.is_rest == False then
            String.fromInt (item.value) ++ "세트"
            else 
            String.fromInt (item.value) ++ "분"
        ))
    ]