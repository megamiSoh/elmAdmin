module Page.Edit.MakeExerEdit exposing(..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Route exposing(..)
import Json.Decode as Decode exposing (Decoder, Value, int, string, float)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Json.Encode as Encode
import Json.Decode as Decode
import Api.Endpoint as Endpoint
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Swiper

type alias Model 
    = {
        session : Session
        , workOut : List FilterData
        , addItem : List FilterData
        , break : FilterData
        , next : Bool
        , what : String
        , loading : Bool
        , swipingState : Swiper.SwipingState
        , swipeCode : String
        , filterData : List FilterData
        , check : Bool
        , getFilter : GetFilter
        , menuOpen : Bool
        , stopEvent :Bool
        , menuleft :Bool
        , resultCount : String
        , screenInfo : ScreenInfo
        , takeList : Int
        , infiniteLoading : Bool
        , offsetH : Int
        , page : Int
        , per_page : Int
        , title : String
        , videoId : String
    }

type alias DetailData =     
    { data : DetailItem}

type alias DetailItem = 
    { difficulty_name : Maybe String
    , duration : String
    , exercise_items : List ExItem
    , exercise_part_name :Maybe (List String)
    , id : Int
    , inserted_at : String
    , pairing : List Pair
    , title : String
    , description : Maybe String }

type alias ExItem = 
    { action_id : Maybe Int
    , difficulty_name : Maybe String
    , duration: String
    -- , exercise_id : Int
    , exercise_name : Maybe String
    , instrument_name : Maybe String
    -- , is_rest : Bool
    -- , mediaid : String
    , part_detail_name : List (Maybe String)
    -- , sort : Int
    , thembnail : String
    , title : String
    , value : Int
    }

type alias Pair = 
    { file: String
    , image : String
    , title : String }

type alias FilterResult = 
    { data : List FilterData 
    , paginate : FilterPaginate}

type alias FilterData =
    { difficulty_name: Maybe String
    , exercise_name: Maybe String
    , id: Int
    , instrument_name: Maybe String
    , part_detail_name: List String
    , title: Maybe String
    , value : Maybe Int
    , duration: Maybe String
    , thembnail : Maybe String}

type alias FilterPaginate = 
    { difficulty_code : List String
    , exercise_code : List String
    , instrument_code : List String
    , page : Int
    , part_detail_code : List String
    , per_page : Int
    , title : String
    , total_count : Int}

type alias GetFilter = 
    { page : Int
    , per_page : Int
    , difficulty_code: List String
    , exercise_code : List String
    , instrument_code : List String
    , part_detail_code : List String
    , title : String
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}


-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = 
    (
        { session = session
        , workOut = []
        , addItem = []
        , loading = True
        , swipeCode = ""
        , menuOpen = False
        , stopEvent = False
        , menuleft = False
        , takeList = 50
        , resultCount = ""
        , offsetH = 0
        , videoId = ""
        , page = 1
        , per_page = 10 
        , title = ""
        , infiniteLoading = False
        , swipingState = Swiper.initialSwipingState
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , getFilter =
            { difficulty_code= []
            , exercise_code = []
            , instrument_code = []
            , part_detail_code = []
            , title = ""
            , page = 1
            , per_page = 1
            }
        , break= 
            { difficulty_name = Nothing
            , exercise_name = Nothing
            , id = 0
            , instrument_name = Nothing
            , part_detail_name = []
            , title = Nothing
            , value = Just 1
            , duration = Nothing
            , thembnail = Nothing }
        , next = False
        , what = ""
        , filterData = []
        , check = mobile
        }
        , 
        Cmd.batch[
        Api.sendData ()
        , Api.getfilter ()
        , Api.getId ()
        , Api.scrollControl ()
        ]
    )


    

filterEncoder model session page per_page= 
    let
        list =  
            Encode.object
            [ ("difficulty_code", (Encode.list Encode.string) model.difficulty_code)
            , ("exercise_code", (Encode.list Encode.string) model.exercise_code)
            , ("instrument_code", (Encode.list Encode.string) model.instrument_code)
            , ("part_detail_code", (Encode.list Encode.string) model.part_detail_code)
            , ("title", Encode.string  model.title)
            , ("page", Encode.int page)
            , ("per_page", Encode.int per_page)
            ]
        body = 
            list
                |> Http.jsonBody
    in
    (Decoder.filterResult FilterResult FilterData FilterPaginate)
    |> Api.post Endpoint.filter (Session.cred session) GetFilterData body 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[
    Api.receiveFilter GetFilterValue
    , Api.receive Success
    , Api.receiveData ReceiveDataFromStorage
    , Session.changes GotSession (Session.navKey model.session)
    , Api.onSucceesSession SessionCheck
    , Api.receiveId GetId
    ]

type Msg 
    = AddItem Int
    | BackItem Int
    | AddBreak
    | SendData
    | ReceiveDataFromStorage Encode.Value
    | Test String
    | DeleteData 
    | GetFilterData (Result Http.Error FilterResult)
    | GetFilterValue Encode.Value
    | Success Encode.Value
    | SearchExercise String
    | GotSession Session
    | SessionCheck Encode.Value
    | Swiped Int Swiper.SwipeEvent
    | StartEvent 
    | SwipedLeft Swiper.SwipeEvent
    | ScrollEvent ScreenInfo
    | ScrInfo 
    | Search
    | GetId Encode.Value
    | KeyDown Int
    | GetList (Result Http.Error DetailData)
    | GoFilter

    -- | SearchExercise STring
toSession : Model -> Session
toSession model =
    model.session


toCheck : Model -> Bool
toCheck model =
    model.check

listEncoder model = 
    Encode.object   
        [
             ("difficulty_name", (Encode.string) (justokData model.difficulty_name))
            , ("exercise_name", (Encode.string) (justokData model.exercise_name))
            , ("id", Encode.int model.id)
            , ("instrument_name", (Encode.string) (justokData model.instrument_name))
            , ("part_detail_name", (Encode.list Encode.string) model.part_detail_name
            -- (case model.part_detail_name of
            --     Just ok ->
            --         ok

            --     Nothing ->
            --         [])
            )
            , ("title", (Encode.string) (justokData model.title))
            , ("value", Encode.int 
            ( case model.value of
                Just ok ->
                    ok

                Nothing ->
                    3
                    )
            )   
            , ("duration", Encode.string (
                case model.duration of
                    Just ok ->
                        ok
                    Nothing ->
                        ""
            ))
            , ("thembnail", Encode.string (
                case model.thembnail of
                    Just ok ->
                        ok
                    Nothing ->
                        ""
                        
            ))
        ]

sendDataEncoder model = 
    Encode.list listEncoder model


onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

justData item = 
    case item of
        Just a ->
            a
    
        Nothing ->
            []

justInt item = 
    case item of
        Just a ->
            a
    
        Nothing ->
            0

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoFilter ->
            let
                enc = sendDataEncoder model.addItem
            in
            (model, Cmd.batch[Route.pushUrl (Session.navKey model.session) Route.EditFilter
            , Api.toJs enc])
        GetList(Ok ok) ->  
            let
                old = model.break
                
                addData = 
                    List.map (\x ->
                        {old | difficulty_name = x.difficulty_name
                        , exercise_name = x.exercise_name
                        , id = (justInt x.action_id)
                        , instrument_name = x.instrument_name
                        , part_detail_name = []
                        , title = Just x.title
                        , value = Just x.value
                        , duration = Just x.duration
                        , thembnail = Just x.thembnail }
                    ) ok.data.exercise_items
            in
            ({model | addItem = addData}, Cmd.none)
            -- update GoVideo {model | getData = ok.data}
        GetList(Err err) ->
            let
                serverErrors = 
                    Api.decodeErrors err  
            in
             (model,(Session.changeInterCeptor (Just serverErrors) model.session))
        GetId id ->
            let
                result = Decode.decodeValue Decode.string id
            in
            case result of
                Ok string ->
                    ({model | videoId = string}, 
                    (Decoder.makeEdit DetailData DetailItem ExItem Pair)
                    |>Api.get GetList (Endpoint.makeDetail string) (Session.cred model.session) 
                    )
            
                Err _ ->
                    (model,Cmd.none)
        
        KeyDown key ->
            if key == 13 then
                update Search model
            else
                (model, Cmd.none)
        Search ->
            (model , Cmd.batch[filterEncoder model.getFilter model.session model.page model.per_page, Api.blur () ])
        ScrInfo ->
             (model, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
            let 
                toInt = String.toInt(model.resultCount)
                resultpage = 
                    justIntData (String.toInt model.resultCount)
                endOfPage =  resultpage // model.per_page 
            in
             if (scrollHeight - scrollTop) <= offsetHeight then
                if model.page < (endOfPage + 1) then
                ({model | page = model.page + 1, infiniteLoading = True}, filterEncoder model.getFilter model.session (model.page + 1) model.per_page)
                else
                (model, Cmd.none)
            else
                (model, Cmd.none)
        AddItem idx->
            let
                add = 
                    filterItem (
                        idx

                    ) model.filterData
            in
           
            if List.length model.addItem < 20 then
            ( {model | addItem = model.addItem ++ add}, Cmd.none)
            else
            (model, Cmd.none)
        SwipedLeft evt ->
            let
                ( oldState, swipedLeft ) =
                    Swiper.hasSwipedLeft evt model.swipingState
                
            in
            if swipedLeft then
                ( { model | menuleft = swipedLeft,menuOpen = False, swipingState = oldState}, Cmd.none )
            else
                ({model | swipingState = oldState},Cmd.none)
        Swiped idx evt ->
            let
                (newState, swipedRight ) = 
                    Swiper.hasSwipedRight evt model.swipingState
            in
                if swipedRight then
                ( { model | menuOpen = swipedRight,
                menuleft= False, swipingState = newState}, Cmd.none )
                else
                ({model | swipingState = newState}, Cmd.none)
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, filterEncoder model.getFilter model.session model.page model.per_page)
                    Err _ ->
                        (model, Cmd.none)
        GotSession session ->
            ({model | session = session}
            , filterEncoder model.getFilter session model.page model.per_page
            )
        SearchExercise str ->
            let
                old = model.getFilter
                new = {old | title = str}
            in
            ({model | getFilter = new}, Cmd.none)
        Success str ->
            let 
                sucData = Decode.decodeValue Decode.string str
            in
            case sucData of
                Ok ok ->
                    
                    (model,
                    -- -- Api.historyUpdate (Encode.string "makeEditStepLast")
                    Route.pushUrl (Session.navKey model.session) Route.MakeEditLast
                    )
            
                Err err ->
                    (model,Cmd.none)
            
        GetFilterValue val ->
            let
                valdecode = 
                    Decode.decodeValue (Decoder.getFilterDecoder GetFilter) val
            in
            case valdecode of
                Ok ok ->
                    ({model | getFilter = ok}, Cmd.batch[
                        filterEncoder ok model.session model.page model.per_page
                    ])
            
                Err err -> 
                    (model, Cmd.none)
                    
        GetFilterData (Ok ok)->
            let 
                count = String.fromInt(List.length (ok.data))
            in
                if ok.data == [] then
                ({model | filterData = ok.data , resultCount = count, loading = False, infiniteLoading = False},Cmd.none)
                else
                    ({model | filterData = model.filterData ++ ok.data  , resultCount = (String.fromInt ok.paginate.total_count), loading = False, infiniteLoading = False},Cmd.none)
        GetFilterData (Err err)->
            let
                serverErrors = 
                    Api.decodeErrors err    
            in
            (model,(Session.changeInterCeptor (Just serverErrors) model.session))
        AddBreak ->
            if List.length model.addItem < 20 then
            ({model | addItem = model.addItem ++ [model.break]}, Cmd.none)
            else 
            (model, Cmd.none)
        
        StartEvent ->
            ({model | stopEvent = False}, Cmd.none)
                        
        BackItem idx ->
            let
                before =
                    List.take (idx) model.addItem 

                after = 
                    List.drop (idx+1) model.addItem
                result =
                    before ++ after
            in
            
            ({model | addItem = result} , Cmd.none)

        SendData ->
            let
                enc = sendDataEncoder model.addItem
            in
            
           (model, 
           
           Cmd.batch [
               Api.toJs enc
            ]
           )
        
        ReceiveDataFromStorage data ->
            let 
                last = Decode.decodeValue (Decode.list(Decoder.filterStep2 FilterData)) data
            in
            case last of
                Ok val ->
                    ( {model | addItem = val, what = "페이지 로드가 완료 되었습니다."}, Cmd.none )
                Err _ -> 
                    (model, Cmd.none)
           
               
        Test data -> 
            ({model | what = data}, Cmd.none)
        
        DeleteData ->
            (model,
            Cmd.batch [ 
                Api.deleteData (),
                 Route.pushUrl (Session.navKey model.session) Route.Filter
                -- -- Api.historyUpdate (Encode.string "filter")
            ]
            )






filterItem item list =
    List.filter(\x -> x.id == item) list


justokData data = 
    case data of
        Just ok ->
            ok
    
        Nothing ->
            ""

justIntData data = 
    case data of
        Just ok ->
            ok
    
        Nothing ->
            0

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
        if model.loading then
        { title = "맞춤운동 필터 Step 1"
        , content = 
                div [] [
                    div [class "spinnerBack"] [spinner]
                ]
            }
        else
        { title = "맞춤운동 필터 Step 1"
        , content = 
                div [] [
                    appHeader model
                    , appitemContainer model
                ]
            }
    else
    { title = "맞춤운동 필터 Step 1"
    , content = 
            div [] [
                web model
            ]
        }

app model = 
    div [] [
        if model.loading then
        div [class "spinnerBack"] [spinner]
        else
        div [] []
        , appHeader model , 
        if model.loading then
        div [] []
        else
        appitemContainer model
    ]

appHeader model = 
    div [class "appheadermakeExer"] [
        ul [ class "commonHeaderBoth makeExerHeader"]
        [ li [ class "m_backbtn" ]
            [a [Route.href Route.Filter][ i [ class "fas fa-angle-left" ]
                []
            ]
            ]
        , li [ class "m3_topboxtitle" ]
            [ text "운동추가" ]
        ,   if List.length (model.addItem) == 0 then
                li  [ class "m2_nextbtn"]
                [ text "다음" ]
            else
                li  [ class "m2_nextbtn", onClick SendData]
                [ text "다음" ]
        ]
        
        
    ]

-- 
web model = 
        div [ class "container" ]
            [
                commonHeader2 "/image/icon_customworkout.png" "맞춤운동 수정"
                ,search model,
            if model.loading then
                spinner
            else
             itemContainer model
            , goBtn model 
    ]

itemContainer model = 
     div [class"filter_box"] [
            div[class "filterStep1_listbox"] [
                div [] [stringresultCount model.resultCount "searchlistCount" "검색",
                 breakTime "fas fa-plus-circle" AddBreak]
                , div [class "filterStep1_listsrollbox", scrollEvent ScrollEvent]
                 [
                    if List.length model.filterData > 0 then
                    div [ class "loadlistbox" , scrollEvent ScrollEvent]
                        (List.indexedMap (
                            \idx x ->
                            workoutItem idx x "fas fa-plus-circle" 
                            ) model.filterData)
                    else 
                    div [] [text "검색 된 운동이 없습니다. "]
                ]
            ]
            , 
            if List.length (model.addItem) == 0 then
            div [class "filterStep1_listbox2"] [
                resultCount model.addItem "select_listresult" "선택",
                div [class "filterStep1_listsrollbox2 warningText"] [
                            text "운동을 선택 해 주세요."
                ]
            ]
            else
            div [class "filterStep1_listbox2"] [
                resultCount model.addItem "select_listresult" "선택",
                div [class "filterStep1_listsrollboxAdd"] [
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            workoutItem idx x "fas fa-minus-circle"  
                        )model.addItem )
                ] 
            ]
            
            -- , text model.what
        ]

appitemContainer model = 
    div [] [
        div ([class "m_filterStep1_filter_box"]
    -- ++ [id "infinite"]
    ++ [style "left"
                 (if model.menuleft then
                     "0"
                    else
                    "-15.5rem"
                 ), 
        style "left"
                 (if model.menuOpen then
                   
                    "-15.5rem"
                    else
                    "0"
                 ) 
                 , style "transition" "left 0.5s"
                 , style "max-height" "100%"
                 ]
                 ++ [scrollEvent ScrollEvent]
            )
            [
            div([class "m_filterStep1_listbox"]
            -- ++ [ style "height"  "85vh"] 
            ++ Swiper.onSwipeEvents SwipedLeft
            
            ) [
                div [class "togetherheaderSpace"] [
            div [ class "control has-icons-left m_top_input  makeExerHeader" ]
                [ 
                p [ class "iconFixed"] [
                    input [id "keyboardBlur",onKeyDown KeyDown, class "input m_filterinput", type_ "text", onInput SearchExercise, placeholder "운동을 직접 검색하세요", value model.getFilter.title, style "width" "100%"]
                    []
                    , span [ class "icon is-small is-left m_filtersearch" ]
                        [ i [ class "fas fa-search " ]
                            []
                        ]
                    , div [ class "filterbtn m_fa-filter", onClick GoFilter ]
                    [ i [ class "fas fa-filter" ]
                        []
                    ]
                ]
                
                ]
                ]
                , div [style "height" "118px"][stringresultCount model.resultCount "m_searchlistCount" "검색",
                appbreakTime "fas fa-plus-circle" AddBreak],
                    if List.length model.filterData > 0 then
                    div ([class "m_filterStep1_listsrollbox"]
                    ++ [scrollEvent ScrollEvent])
                    [
                            div [ class "m_loadlistbox" ]
                                (List.indexedMap (
                                    \idx x ->
                                    appworkoutItem idx x "fas fa-plus-circle" BackItem 
                                    ) model.filterData)
                    ]
                    else 
                        div [class "noResult"] [text "검색 된 운동이 없습니다."]
                , 
                    if model.infiniteLoading then
                    div [class "loadingPosition"] [
                    spinner
                    ]
                    else
                    span [] []
                
            ]
            , 
            if List.length (model.addItem) == 0 then
            div ([class "m_filterStep1_listbox2"]
             ++ Swiper.onSwipeEvents (Swiped -1)
              ++ [ style "height" "100vh"] 
            ) [
                div [class "togetherheaderSpace"] [
                div [ class "control has-icons-left m_top_input  makeExerHeader" ]
                    []
                    ]
                , resultCount model.addItem "m_select_listresult" "선택",
                div [class "filterStep1_listsrollbox2 m_warningText"] [
                            text "운동을 선택 해 주세요."
                ]
            ]
            else
            div ([class "m_filterStep1_listbox2"]
             ++ Swiper.onSwipeEvents (Swiped -1)
             ++ [ style "height" "100vh"] 
            ) [
                div [class "togetherheaderSpace"] [
                div [ class "control has-icons-left m_top_input  makeExerHeader" ]
                    [  ] ]
                , resultCount model.addItem "m_select_listresult" "선택",
                 div [class "m_filterStep1_listsrollboxAdd"] [
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            appworkoutItem idx x "fas fa-minus-circle" BackItem 
                        )model.addItem )
                ]
            ]
            
        ]
    ]

search model = 
        div [ class "filterstep1_yf_box" ]
            [ div [ class "filterstep1_yf_full" ]
                [ div [ class "field is-grouped" ]
                    [ p [ class "control is-expanded" ]
                        [ input [ class "input", type_ "text", placeholder "운동을 검색하세요", onInput SearchExercise, value model.getFilter.title ]
                            []
                        ]
                    , p [ class "control yf_con", onClick Search ]
                        [ div [ class "button yf_infor" ]
                            [ text "검색" ]
                        ]
                    , p [ class "control yf_con" ]
                        [ a [ class "button yf_infor", Route.href Route.Filter ]
                            [ text "필터" ]
                        ]
                    ]
                ]
            ]

resultCount model style title =  
    div [ class style ]
        [ text ("총 " ++ String.fromInt(List.length model) ++ "건의 " ++ title ++"결과") ]          

stringresultCount model style title =  
    div [ class style ]
        [ text ("총 " ++ model ++ "건의 " ++ title ++"결과") ]   

    
workoutItem idx item style=
            if justokData item.title == "" then
            breakTime "fas fa-minus-circle" (BackItem idx)
            else
            div [ class "worklistbox" ]
                [ div [ class "filterStep1_iconbox" ]
                    [ img [ src (justokData item.thembnail) ]
                        []
                    ]
                , div [ class "filtertextbox" ]
                    [ ul [class "filtertextbox_area"]
                        [ li [ class "filter_work1" ]
                            [ text (justokData item.title) ]
                        , li [ class "filter_work2" ]
                            [ text (justokData item.difficulty_name ++ "-" ++ justokData item.instrument_name ++"-" ++justokData item.exercise_name) ]
                        , li [ class "filter_work3" ]
                            [ text (justokData item.duration) ]
                        ]
                    ]
                , div [ class "filter_addbox" ]
                    [ i [ class style, 
                        if style ==  "fas fa-minus-circle" then
                        onClick (BackItem idx)
                        else
                        onClick (AddItem ( item.id)) 
                    ]
                        []
                    ]
                ]
appworkoutItem idx item style addItem =
            if justokData item.title == "" then
            appbreakTime "fas fa-minus-circle" (BackItem idx)
            else
            div [ class "m_worklistbox" ]
                [ 
                    div [ class "m_filterStep1_iconbox" ]
                    [ img [ src (justokData item.thembnail) ]
                        []
                    ]
                , div [ class "filtertextbox" ]
                    [ ul [class "appfiltertextbox_area"]
                        [ li [ class "m_filter_work1" ]
                            [ text (justokData item.title) ]
                        , li [ class "m_filter_work2" ]
                            [ text (justokData item.difficulty_name ++ "-" ++ justokData item.instrument_name ++"-" ++justokData item.exercise_name)]
                        , li [ class "m_filter_work3" ]
                            [ text (justokData item.duration) ]
                        ]
                    ]
                , div [ class "m_filter_addbox",
                    if style ==  "fas fa-minus-circle" then
                        onClick (addItem idx)
                        else
                        onClick (AddItem ( item.id))
                 ]
                    [ i [ class style ]
                        []
                    ]
                ]

breakTime style addBreak=
        div [ class "breaktimetbox" , onClick addBreak]
        [ div [ class "filterStep1_iconbox" ]
            [ img [ src "/image/m_timeicon.png" ]
                []
            ]
        , div [ class "filterStep1_filtertextbox" ]
            [ ul [class"filtertextbox_area"]
                [ li [ class "filter_work1" ]
                    [ text "휴식하기" ]
                , li [ class "filter_work3" ]
                    [ text "1분" ]
                ]
            ]
        , div [ class "filter_addbox" ]
            [ i [ class style ]
                []
            ]
        ]

appbreakTime style addBreak=
        div [ class "m_breaktimetbox" , onClick addBreak]
        [ div [ class "m_filterStep1_iconbox" ]
            [ img [ src "/image/m_timeicon.png" ]
                []
            ]
        , div [ class "m_filterStep1_filtertextbox" ]
            [ ul [class"m_filtertextbox_area"]
                [ li [ class "m_filter_work1" ]
                    [ text "휴식하기" ]
                , li [ class "m_filter_work3" ]
                    [ text "1분" ]
                ]
            ]
        , div [ class "m_filter_addbox" ]
            [ i [ class style ]
                []
            ]
        ]


goBtn model=
    div [ class "make_yf_butbox" ]
        [ div [ class "yf_backbtm" ]
            [ div [ class "button is-middle", onClick DeleteData]
                [ text "뒤로" ]
            ]
        , div [ class "yf_nextbtm" ]
            [  
                if List.length (model.addItem) == 0 then
                    div [ class "button is-dark is-middle next_btn"]
                    [ text "다음" ]
                else
                    div [ class "button is-dark is-middle next_btn", onClick SendData]
                        [ text "다음" ]
            ]
        ]
