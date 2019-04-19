module Page.Detail.MakeExerSearch exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Html.Attributes as Attr
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing(..)
import Port as P
import Json.Encode as Encode
import Json.Decode as Decode
import Http as Http exposing(..)
import Api as Api
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
type alias Model = 
    { session : Session
    , checkDevice : String
    , page : Int
    , per_page : Int
    , title : String
    , getlistData : GetListData
    , check : Bool
    , loading : Bool
    , saveCheckVal : String
    , sumCount : Int
    , screenInfo : ScreenInfo
    , infiniteLoading : Bool
    , newList : List ListData
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias GetListData = 
    { data : List ListData
    , paginate: Paginate }

type alias ListData = 
    { difficulty_name : Maybe String
    , duration : String
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , is_use : Bool
    , mediaid : String
    , thembnail : String
    , title : String
    }

type alias Paginate = 
    { difficulty_code : String
    , end_date : String
    , exercise_part_code : String
    , inserted_id : Int
    , make_code : String
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    }

bodyEncode page perpage title session= 
    let
        list = 
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int perpage)
                , ("title" , Encode.string title)]
        body =
            list
                |> Http.jsonBody
    in
    (Decoder.makeExerList GetListData ListData Paginate)
    |> Api.post Endpoint.makeExerList (Session.cred session) GetData body 
    
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
    let
        listmodel = 
            { page = 1
            , per_page = 10
            , title = ""}
    in
    (
        { session = session
        , checkDevice = ""
        , page = 1
        , per_page = 10
        , infiniteLoading = False
        , sumCount = 1
        , title = ""
        , check = mobile
        , saveCheckVal = ""
        , loading = True
        , newList = []
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , getlistData = 
            { data = []
            , paginate =
                { difficulty_code = ""
                , end_date = ""
                , exercise_part_code = ""
                , inserted_id = 0
                , make_code = ""
                , page = 0
                , per_page = 0
                , start_date = ""
                , title = ""
                , total_count = 0
                }
            }
        }, 
        Cmd.none
    )

type Msg 
    =  GetData (Result Http.Error GetListData)
    | CheckId Int String
    | SaveIdComplete Encode.Value
    | SessionCheck Encode.Value
    | GotSession Session
    | Delete Int
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | PageBtn (Int, String)
    | OnLoad
    | KeyDown Int
    | ScrollEvent ScreenInfo
    | SearchExercise String


toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int) 


subscriptions : Model -> Sub Msg
subscriptions model=
    Sub.batch [
        Session.changes GotSession (Session.navKey model.session)
        , Api.successId SaveIdComplete
    ]

onLoad msg =
    on "load" (Decode.succeed msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            if key == 13 then
                (model, bodyEncode model.page model.per_page model.title model.session)
            else
                (model, Cmd.none)
        SearchExercise str ->
            ({model | title = str}, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                ({model | infiniteLoading = True}, bodyEncode (model.page)  model.per_page model.title model.session)
                
            else
                (model, Cmd.none)
        OnLoad ->
            if model.sumCount >= List.length(model.getlistData.data) then
            ({model | loading = False}, Cmd.none)
            else
            ({model | sumCount = model.sumCount + 1}, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    (model, bodyEncode idx model.per_page model.title model.session)
                "next" ->
                    (model, bodyEncode idx model.per_page model.title model.session)
                "go" -> 
                    (model, bodyEncode idx model.per_page model.title model.session)
                _ ->
                    (model, Cmd.none)
        DeleteSuccess (Ok ok) ->
            (model, bodyEncode model.page model.per_page model.title model.session)
        DeleteSuccess (Err err) ->
            (model, Cmd.none)
        Delete id ->
            (model, 
            Decoder.resultD
                |> Api.get DeleteSuccess (Endpoint.makeDelete (String.fromInt (id)))(Session.cred model.session)  )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, bodyEncode model.page model.per_page model.title model.session)
                    Err _ ->
                        (model, Cmd.none)
        SaveIdComplete str ->
            if model.saveCheckVal == "" then
            (model, 
            -- -- Api.historyUpdate (Encode.string "makeExerciseDetail")
            Route.pushUrl (Session.navKey model.session) Route.MakeDetail
            )
            else 
            (model,
            --  -- Api.historyUpdate (Encode.string "togetherWrite")
            Route.pushUrl (Session.navKey model.session) Route.TogetherW
            )
        CheckId id str->
            let
                save = Encode.int id
            in
            ({model | saveCheckVal = str},Api.saveId save)
        GetData (Ok ok) -> 
            if ok.data == [] then
            ({model | getlistData = ok, newList = model.newList, infiniteLoading = False}, Cmd.none)
            else
            ({model | getlistData = ok, newList = model.newList ++ ok.data, infiniteLoading = False}, Cmd.none)
        GetData (Err err) -> 
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
        
onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = div [] [
        div [ class "cus_topbox" ]
            [ a [ class "backbtn", Route.href Route.MakeExer ]
                [ i [ class "fas fa-angle-left" ]
                    []
                ]
            , div [ class "topboxtitle" ]
                [ p [ class "control has-icons-left top_input" ]
                    [ input [ class "input", type_ "text", placeholder "운동을 검색하세요", onInput SearchExercise, onKeyDown KeyDown ]
                        []
                    , span [ class "icon is-small is-left" ]
                        [ i [ class "fas fa-search" ]
                            []
                        ]
                    ]
                ]
            ]
        ,
        div[](List.map appItemContent model.getlistData.data)
--         , if model.infiniteLoading then
--             div [class "loadingPosition"] [
--             infiniteSpinner
--             ]
--         else
--             span [] []
        -- div [ class "container yf_container" ]
        --     [ div [ class "result_textbox" ]
        --         [ text "총 n건의 검색결과" ]
        --     , div [ class "yf_box" ]
        --         [ img [ src "../image/dummy_video_image.png" ]
        --             []
        --         , div [ class "text_wrap" ]
        --             [ div [ class "yf_box_title" ]
        --                 [ text "와이드 레그드 포워드 밴드 위드 핸즈 온 힙" ]
        --             , div [ class "yf_ul" ]
        --                 [ ul []
        --                     [ li []
        --                         [ text "2019-01-01" ]
        --                     , li [ class "yf_il" ]
        --                         [ i [ class "fas fa-share-square" ]
        --                             []
        --                         ]
        --                     , li []
        --                         [ text "공유하기" ]
        --                     ]
        --                 ]
        --             ]
        --         ]
        --     ]

    ]}
-- webOrApp model= 
--     -- if model.check then
--          a [Route.href Route.MSearch] [appHeaderSearch "맞춤운동" "makeExerHeader",
--             if model.loading then
--             div [class "spinnerBack"] [
--                 spinner
--                 ]
--             else 
--             div [] []
--             , app model
--             ]  
--     -- else 
--     --     web model

-- web model = 
--     div [ class "customContainerwrap" ]
--             [ div [ class "container" ]
--                 [ div [ class "notification yf_workout" ]
--                     [
--                         commonHeader "/image/icon_customworkout.png" "맞춤운동",
--                         bodyContentTitle,
--                         div [ class "customyf_box2"] [
--                             div [ class "make_box_title" ]
--                                 [ h1 [ class "make_yf_h2" ]
--                                     [ text "맞춤운동 리스트" ]
--                                 ],
--                             if model.loading then
--                                 spinner
--                             else
--                                     if List.length model.getlistData.data > 0 then
--                                     div [ class "make_boxwrap" ]
--                                     (List.map bodyItem model.getlistData.data)
--                                 else
--                                     div [] [text "맞춤영상이 없습니다."]

--                         ]
--                     ]
--                 ]
--                 ,pagination 
--                 PageBtn
--                 model.getlistData.paginate
--             ]
-- app model =
--     div [ class "container", class "scroll", scrollEvent ScrollEvent, style "height" "85vh" ][
--          appStartBox
--         ,listTitle
--         ,div[](List.map appItemContent model.newList)
--         , if model.infiniteLoading then
--             div [class "loadingPosition"] [
--             infiniteSpinner
--             ]
--         else
--             span [] []
--     ]

-- appStartBox = 
--     div [ class "make_m_yf_box" ]
--         [ h1 [ class "m_make_yf_h1" ]
--             [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
--         , a [ class "button is-dark m_make_yf_darkbut", Route.href Route.Filter ]
--             [ text "시작하기" ]
--         , br []
--             []
--         ]
-- listTitle = 
--     div [ class "m_make_box_title" ]
--         [ h1 [ class "m_make_yf_h2" ]
--             [ text "맞춤운동 리스트" ]
--         ]

appItemContent item=
        div [ class "m_make_yf_box2" ]
            [ div [ class "m_make_videoimg", onClick (CheckId item.id "") ]
                [ img [ src item.thembnail, onLoad OnLoad ]
                    []
                ]
      
            , div [ class "m_make_yf_box_title", onClick (CheckId item.id "") ]
                [ text item.title ]
            , div [ class "make_yf_ul" ]
                [ ul []
                    [ li []
                        [ text (String.dropRight 10 (item.inserted_at)) ]
                    , li [] [
                        i [ class "fas fa-stopwatch" ]
                        []
                        , text " "
                        , text item.duration
                    ]
                    ]
                ]
            , div [ class "button is-dark m_makeExercise_share"
            , onClick (CheckId item.id "share")
            ]
                [ i [ class "fas fa-share-square" ]
                [], text "공유하기" 
            ]

                , div [ class "button m_makeExercise_dete",onClick (Delete item.id) ]
                [ i [ class "far fa-trash-alt" ]
                [], text "삭제" 
            ]
            ]

-- bodyItem item=
--     div [ class "make_box_card_wrap" ]
--     [ div [ class "make_videoboxwrap"]
--         [ div [ class "video_image" , onClick (CheckId item.id "")]
--             [ img [ class "vpic1",src item.thembnail, alt "dummy_video_image" ]
--                 []
--             ]
--         , div [ class "Customtextbox"]
--             [ div [ class "m1"  , onClick (CheckId item.id "")]
--                 [ h1 [ class "make_yf_titlename" ]
--                     [ text item.title ]
--                 ]
--             , div [ class "m2" ]
--                 [ text (String.dropRight 10 (item.inserted_at)), 
--                 div [] [
--                     i [ class "fas fa-stopwatch" ]
--                         []
--                         , text " "
--                         , text item.duration
--                 ]
--                 , p [class "makebtn"]
--                     [ div  [ class "button is-dark darkbtn make_share"
--                     , onClick (CheckId item.id "share") ]
--                         [ i [ class "fas fa-share-square" ]
--                             [] , text "공유" 
--                         ]
--                     , div [ class "button" ,onClick (Delete item.id)]
--                         [ i [ class "far fa-trash-alt" ]
--                             [] , text "삭제" 
--                         ]
--                     ]
--                 ]
--             ]
--         ]
--     ]

-- pagenation = 
--     div [ class "customyf_Pagination" ]
--         [ nav [ class "pagination is-centered" ]
--             [ ul [ class "pagination-list" ]
--                 [ li [ class "" ]
--                     [ a [ class "pagination-link"]
--                         [ text "<" , text "<" ]
--                     ]
--                 , a [ class "pagination-link"]
--                     [ text "<" ]
--                 , li []
--                     [ a [ class "pagination-link is-current yf_cut" ]
--                         [ text "5" ]
--                     ]
--                 , li []
--                     [ a [ class "pagination-link" ]
--                         [ text ">" ]
--                     ]
--                 , a [ class "pagination-link" ]
--                     [ text ">>" ]
--                 ]
--             ]
--         ]


-- bodyContentTitle =
--           div [ class "make_yf_box" ] 
        
--                 [ 
--             img [ src "image/runimage.png", alt "runimage" ]
--                 []
--            ,
--                     h1 [ class "make_yf_h1" ]
--                 [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
--              , a [ class "button is-dark make_yf_darkbut", Route.href Route.Filter ]
--                 [ text "시작하기" ]
--             , br []
--                 []
--             ]
