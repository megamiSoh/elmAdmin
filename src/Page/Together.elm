module Page.Together exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing (..)
import Port as P
import Json.Encode as Encode
import Json.Decode as Decode
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Html.Lazy exposing (lazy, lazy2)
import Page as Page
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
        , need2login : Bool
        , videoStart : String
    }
type alias TogetherDataWrap = 
    { data : List TogetherData 
    , paginate : Paginate
    }

type alias TogetherData = 
    { content : Maybe String
    , detail : List DetailTogether
    , id : Int
    , inserted_at : String
    , is_delete : Bool
    , link_code : String
    , recommend_cnt : Int
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
        , infiniteLoading = False
        , appData = []
        , count = 1
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
        [ P.checkMobile ()
        , dataEncoder 1 10 session]
    )

dataEncoder page perpage session=
    let
        list = 
            Encode.object 
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int perpage)]
                |> Http.jsonBody

    in
    Api.post Endpoint.togetherList (Session.cred session) GetData list (Decoder.togetherdatawrap  TogetherDataWrap TogetherData DetailTogether Paginate TogetherItems Pairing)

loadingEncoder page perpage session = 
    let
        list = 
            Encode.object 
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int perpage)]
                |> Http.jsonBody

    in
    Api.post Endpoint.togetherList (Session.cred session) LoadingGetData list (Decoder.togetherdatawrap  TogetherDataWrap TogetherData DetailTogether Paginate TogetherItems Pairing)

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)    
onLoad msg =
    on "load" (Decode.succeed msg)

type Msg 
    = IsActive String
    | CheckDevice Encode.Value
    | GetData (Result Http.Error TogetherDataWrap)
    -- | Loading Encode.Value
    | IsLike String
    | LikeComplete (Result Http.Error Like)
    | PageBtn (Int, String)
    | ScrollEvent ScreenInfo
    | LoadingGetData (Result Http.Error TogetherDataWrap)
    | ScrapComplete (Result Http.Error Decoder.Success)
    | Scrap Int
    | VideoCall ((List Pairing) ,Int)
    | OnLoad
    

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Sub.none
    -- Api.videoSuccess Loading

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLoad ->
            if model.count <= List.length (model.togetherData.data) then
            ({model | loading = False}, Cmd.none)
            else
            ({model | count = model.count + 1}, Cmd.none)
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
            ({model| infiniteLoading = False}, Cmd.none)
            else
            ({model | appData = model.appData ++ ok.data, page = model.page + 1, infiniteLoading = False}, Cmd.none
            -- , Api.togetherDataList page 
            )
        LoadingGetData (Err err) ->
            (model, Cmd.none)
        
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                ({model | infiniteLoading = True}, loadingEncoder (model.page + 1)  model.per_page model.session)
                
            else
                (model, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    ({model | page = idx}, dataEncoder idx model.per_page model.session)
                "next" ->
                    ({model | page = idx}, dataEncoder idx model.per_page model.session)
                "go" -> 
                    ({model | page = idx}, dataEncoder idx model.per_page model.session)
                _ ->
                    ({model | page = idx}, Cmd.none)
        LikeComplete (Ok ok) ->
            ({model | like = ok.data.count}
            , dataEncoder model.page model.per_page model.session)
        LikeComplete (Err err) ->
            let
                serverErrors = Api.decodeErrors err
            in
                if serverErrors == "401" then 
                ({model | need2login = True}, Cmd.none )
            else
                (model, Cmd.none)
        IsLike id->
            (model, Api.get LikeComplete (Endpoint.togetherLike id) (Session.cred model.session) (Decoder.togetherLike Like LikeData))
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
            ({model | togetherData = ok, appData = ok.data} , Cmd.none )
        GetData (Err err) ->
            (model, Cmd.none)
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
            , app model
       ]
       else 
            web model   
    }

justData item = 
    case item of
        Just val ->
            val
    
        Nothing ->
            ""

web model = 
    div [class "containerwrap"] [
                div [class "container"][
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
                         ,pagination 
                                PageBtn
                                model.togetherData.paginate
                            ]
                        ]
                    ]
            ]

app model =
     
    div [class "container "] [
        div [] [
            appHeaderSearch "함께해요" "together",
            if model.need2login then
            need2loginAppDetailRoute Route.Together
            else
            div [ class "scroll" , scrollEvent ScrollEvent, style "height" "80vh"] [
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

appContentsItem idx item model=
    let
        pairing = 
            List.head
                (
                    List.map(\x ->
                        x.pairing    
                    )item.detail
                )  
        thumbnail = 
            List.head
                (
                    List.map(\x ->
                        x.thembnail   
                    )item.detail
                ) 

    in
    
    div [ class "m_to_mediabox2" ]
        [   
            div [ class "m_to_yf_boxtop" ]
            [
            div [ class "m_to_yf_id" ]
                [ strong []
                    [  text "닉네임" ]
                ]
            ], 
            div [class"m_to_video"][
             div ([class "thumbTest"]
             )[
                 div [class "clickThis",onClick (VideoCall ((
                 case pairing of
                    Just a ->
                        a
                    Nothing ->
                        []
             ), idx))] [
                 img [src (justData thumbnail), onLoad OnLoad] []
             ],
                 div [id ("myElement" ++ String.fromInt(idx)) ]  [] ] 
            , div [ class "m_to_yf_more" ]
                [ div [ ]
                    [ strong []
                        [ text "자세히보기" ]
                    ]
                ]
            ]
        , div [ class "m_to_yf_text" ]
            [ text (justData item.content )]
        , div [ class "level-left m_to_yf_like", onClick (IsLike (String.fromInt(item.id))) ]
            [ text( "좋아요" ++ String.fromInt (item.recommend_cnt) ++"개" ) 
            , 
            if model.like == 1 then
            i [ class "far fa-heart together_heart"]
                []
            else 
                i [ class "fas fa-heart together_heart" ]
                []
            ]

        , div [ class "level-left m_to_yf_scrap", onClick (Scrap item.id) ]
            [
            --      text( "스크랩" ++ String.fromInt(item.isLike) ++"개" ) 
            -- , 
            i [ class "far fa-bookmark together_bookmark" ]
                []
            ]
        
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
            , div [](List.indexedMap (
                \idx x -> userItem idx x model
            ) model.togetherData.data)
            ]
        ]
userItem idx item model =
    let
        pairing = 
            List.head
                (
                    List.map(\x ->
                        x.pairing    
                    )item.detail
                )  
        thumbnail = 
            List.head
                (
                    List.map(\x ->
                        x.thembnail   
                    )item.detail
                ) 
    in
    div [ class "together_mediabox2" ]
        [ div [ class "together_yf_boxtop" ]
            [ p [ class "image is-64x64 together_imgbox" ]
                [ img [ src "https://bulma.io/images/placeholders/128x128.png" ]
                    []
                ]
            , div [ class "together_yf_id" ]
                [ strong []
                    [ text "닉네임" ]
                ]
            ]
        , div [ class "together_yf_text togetherVideo" ]
            [ div ([class "thumbTest", style "width" "54%", style "height" "200px"]
             )[
                 div [class "clickThis",onClick (VideoCall ((
                 case pairing of
                    Just a ->
                        a
                    Nothing ->
                        []
             ), idx))] [
                 img [class "toImg", src (justData thumbnail), onLoad OnLoad] []
             ],
                 div [id ("myElement" ++ String.fromInt(idx)) ]  [] ] 
                , div [id ("together" ++ (String.fromInt(idx)))] [] ,
                div [][text (justData item.content)]
                , div [] [text "더보기"]
                 ]
        , div [ class "level-left together_yf_like", onClick (IsLike (String.fromInt(item.id))) ]
            [ text( "좋아요"++ String.fromInt (item.recommend_cnt) ++"개")  , br []
                []
            , if model.like == 1 then
                i [ class "far fa-heart together_heart"]
                []
            else 
                i [ class "fas fa-heart together_heart" ]
                []
            ]
        , div [ class "level-left together_yf_scrap", onClick (Scrap item.id) ]
            [ 
                -- text( "스크랩"++ String.fromInt (item.isLike) ++"개")  , br []
                -- []
            -- ,
             i [ class "far fa-bookmark together_bookmark" ]
                []
            ]
        ]
