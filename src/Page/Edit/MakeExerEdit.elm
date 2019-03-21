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

type alias Model 
    = {
        session : Session
        , workOut : List FilterData
        , addItem : List FilterData
        , break : FilterData
        , next : Bool
        , what : String
        , loading : Bool
        , filterData : List FilterData
        , check : Bool
        , getFilter : GetFilter
    }
type alias FilterResult = 
    {data : List FilterData }

type alias FilterData =
    { difficulty_name: Maybe String
    , exercise_name: Maybe String
    , id: Int
    , instrument_name: Maybe String
    , part_detail_name: Maybe (List String)
    , title: Maybe String
    , value : Maybe Int}

type alias GetFilter = 
    { difficulty_code: List String
    , exercise_code : List String
    , instrument_code : List String
    , part_detail_code : List String
    }
type alias MakeEdit = 
    { title : String
    , items : List MakeEditData }

type alias MakeEditData =   
    { action_id : Maybe Int
    , is_rest : Bool
    , value : Int}

filterInit = 
    { difficulty_code= []
    , exercise_code = []
    , instrument_code = []
    , part_detail_code = []
    }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = 
    (
        { session = session
        , workOut = []
        , addItem = []
        , loading = True
        , getFilter = filterInit
        , break= 
            { difficulty_name = Nothing
            , exercise_name = Nothing
            , id = 0
            , instrument_name = Nothing
            , part_detail_name = Nothing
            , title = Nothing
            , value = Nothing }
        , next = False
        , what = ""
        , filterData = []
        , check = mobile
        }
        , 
        Cmd.batch[
        Api.getId ()
        , filterEncoder filterInit session
        ]
    )


filterEncoder model session= 
    let
        list =  
            Encode.object
            [("difficulty_code", (Encode.list Encode.string) model.difficulty_code)
            , ("exercise_code", (Encode.list Encode.string) model.exercise_code)
            , ("instrument_code", (Encode.list Encode.string) model.instrument_code)
            , ("part_detail_code", (Encode.list Encode.string) model.part_detail_code)
            ]
        body = 
            list
                |> Http.jsonBody
    in
    Api.post Endpoint.filter (Session.cred session) GetFilterData body (Decoder.filterResult FilterResult FilterData)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[ Api.receiveId Success
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
    | GetList (Result Http.Error MakeEdit)

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
            , ("part_detail_name", (Encode.list Encode.string)  
            (case model.part_detail_name of
                Just ok ->
                    ok

                Nothing ->
                    [])
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
        ]

sendDataEncoder model = 
    Encode.list listEncoder model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetList (Ok ok)->
            (model, Cmd.none)
        GetList (Err err)->
            (model, Cmd.none)
        SearchExercise str ->
            (model, Cmd.none)
        Success str ->
            let 
                sucData = Decode.decodeValue Decode.string str
            in
            case sucData of
                Ok ok ->
                    (model, 
                    Api.get GetList (Endpoint.makeEdit ok) (Session.cred model.session) (Decoder.makeedit MakeEdit MakeEditData) )
                Err err ->
                    (model,Cmd.none)
            
        GetFilterValue val ->
            let _ = Debug.log "filter" val
                valdecode = 
                    Decode.decodeValue (Decoder.getFilterDecoder GetFilter) val
            in
            case valdecode of
                Ok ok ->
                    ({model | getFilter = ok}, Cmd.batch[
                        filterEncoder ok model.session
                    ])
            
                Err err -> 
                    (model, Cmd.none)
                    
        GetFilterData (Ok ok)->
            ({model | filterData = ok.data, loading = False},Cmd.none)
        GetFilterData (Err err)->
            (model,Cmd.none)
        AddBreak ->
            ({model | addItem = model.addItem ++ [model.break]}, Cmd.none)
        AddItem idx->
            let
                add = 
                    filterItem (idx) model.filterData
            in
            
            ( {model | addItem = model.addItem ++ add} , Cmd.none )

        BackItem idx ->
            let _ = Debug.log "idx" idx
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
                    let _ = Debug.log "dat" val
                        
                    in
                    
                    ( {model | addItem = val, what = "페이지 로드가 완료 되었습니다."}, Cmd.none )
                Err _ -> 
                    (model, Cmd.none)
           
               
        Test data -> 
            ({model | what = data}, Cmd.none)
        
        DeleteData ->
            (model,
            Cmd.batch [ 
                Api.deleteData (),
                 Route.pushUrl (Session.navKey model.session) Route.Filter])






filterItem item list =
    List.filter(\x -> x.id == item) list


justokData data = 
    case data of
        Just ok ->
            ok
    
        Nothing ->
            ""

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "맞춤운동 필터 Step 1"
    , content = 
            web model
        }

app model = 
    div [] [
        if model.loading then
        spinner
        else
        appHeader model
        , appitemContainer model
    ]

appHeader model = 
    div [ class "filterstepHeader2"]
        [ div [ class "m_backbtn" ]
            [ i [ class "fas fa-angle-left" ]
                []
            ]
        , div [ class "m3_topboxtitle" ]
            [ text "운동추가" ]
        ,   if List.length (model.addItem) == 0 then
                div [ class "m2_nextbtn"]
                [ text "다음" ]
            else
                div [ class "m2_nextbtn", onClick SendData]
                [ text "다음" ]
        , div [ class "control has-icons-left m_top_input" ]
            [ input [ class "input m_filterinput", type_ "text", placeholder "운동을 직접 검색하세요" ]
                []
            , span [ class "icon is-small is-left m_filtersearch" ]
                [ i [ class "fas fa-search " ]
                    []
                ]
            , div [ class "filterbtn m_fa-filter" ]
                [ i [ class "fas fa-filter" ]
                    []
                ]
            ]
        ]
web model = 
        div [ class "container" ]
            [
                commonHeader2 "/image/icon_customworkout.png" "맞춤운동"
                ,search,
            if model.loading then
                spinner
            else
             itemContainer model
            , goBtn model 
    ]

itemContainer model = 
     div [class"filter_box"] [
            div[class "filterStep1_listbox"] [
                resultCount model.filterData "searchlistCount" "검색",
                div [class "filterStep1_listsrollbox"] [
                    breakTime "fas fa-plus-circle" AddBreak,
                    if List.length model.filterData > 0 then
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            workoutItem idx x "fas fa-plus-circle" AddItem 
                            ) model.filterData)
                    else 
                    div [] [text "검색 된 운동이 없습니다. 필터설정을 다시 해 주세요."]
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
                div [class "filterStep1_listsrollbox"] [
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            workoutItem idx x "fas fa-minus-circle" BackItem 
                        )model.addItem )
                ]
            ]
            -- , text model.what
        ]

appitemContainer model = 
     div [class"m_filterStep1_filter_box"] [
            div[class "m_filterStep1_listbox"] [
                resultCount model.workOut "m_searchlistCount" "검색",
                div [class "m_filterStep1_listsrollbox"] [
                    appbreakTime "fas fa-plus-circle" AddBreak,
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            appworkoutItem idx x "fas fa-plus-circle" AddItem 
                            ) model.filterData)
                ]
            ]
            , 
            if List.length (model.addItem) == 0 then
            div [class "m_filterStep1_listbox2"] [
                resultCount model.addItem "m_select_listresult" "선택",
                div [class "filterStep1_listsrollbox2 m_warningText"] [
                            text "운동을 선택 해 주세요."
                ]
            ]
            else
            div [class "m_filterStep1_listbox2"] [
                resultCount model.addItem "m_select_listresult" "선택",
                div [class "m_filterStep1_listsrollbox"] [
                    div [ class "loadlistbox" ]
                        (List.indexedMap (
                            \idx x ->
                            appworkoutItem idx x "fas fa-minus-circle" BackItem 
                        )model.addItem )
                ]
            ]
            , text model.what
        ]

search = 
        div [ class "filterstep1_yf_box" ]
            [ div [ class "filterstep1_yf_full" ]
                [ div [ class "field is-grouped" ]
                    [ p [ class "control is-expanded" ]
                        [ input [ class "input", type_ "text", placeholder "운동을 검색하세요", onInput SearchExercise ]
                            []
                        ]
                    , p [ class "control yf_con" ]
                        [ a [ class "button yf_infor" ]
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


    
workoutItem idx item style addItem =
            if justokData item.title == "" then
            breakTime "fas fa-minus-circle" (addItem item.id)
            else
            div [ class "worklistbox" ]
                [ div [ class "filterStep1_iconbox" ]
                    [ img [ src "/image/m_workicon.png" ]
                        []
                    ]
                , div [ class "filtertextbox" ]
                    [ ul [class "filtertextbox_area"]
                        [ li [ class "filter_work1" ]
                            [ text (justokData item.title) ]
                        , li [ class "filter_work2" ]
                            [ text (justokData item.difficulty_name ++ "-" ++ justokData item.instrument_name ++"-" ++justokData item.exercise_name) ]
                        , li [ class "filter_work3" ]
                            [ text " ㅡ " ]
                        ]
                    ]
                , div [ class "filter_addbox" ]
                    [ i [ class style, 
                        if style ==  "fas fa-minus-circle" then
                        onClick (addItem idx)
                        else
                        onClick (addItem item.id) 
                    ]
                        []
                    ]
                ]
appworkoutItem idx item style addItem =
            if justokData item.title == "" then
            appbreakTime "fas fa-minus-circle" (addItem idx)
            else
            div [ class "m_worklistbox" ]
                [ div [ class "m_filterStep1_iconbox" ]
                    [ img [ src "/image/m_workicon.png" ]
                        []
                    ]
                , div [ class "filtertextbox" ]
                    [ ul [class "filtertextbox_area"]
                        [ li [ class "m_filter_work1" ]
                            [ text (justokData item.title) ]
                        , li [ class "m_filter_work2" ]
                            [ text (justokData item.difficulty_name ++ "-" ++ justokData item.instrument_name ++"-" ++justokData item.exercise_name)]
                        , li [ class "m_filter_work3" ]
                            [ text " ㅡ " ]
                        ]
                    ]
                , div [ class "m_filter_addbox" ]
                    [ i [ class style, 
                        if style ==  "fas fa-minus-circle" then
                        onClick (addItem idx)
                        else
                        onClick (addItem item.id) 
                     ]
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
