module Page.MakeExer exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Html.Attributes as Attr
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing(..)
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
    , pageNum : Int
    , show : String
    , contentsId : Int
    , isActive : String
    , askSelected : Bool
    , askyours : AskYourData
    , askIndex : Int
    , aksYoursPoint : AskYourDataPoint
    , categoryPaperWeight : String
    , axerCode : String
    , askSearchData : List AskSearchData
    , askSearchItem : AskSearchData
    , etcAsk : List EtcDataItem 
    , currentEtcAsk : EtcDataItem
    , idxSearch : Int
    , trialShow : Bool
    , yesformat: 
        { ask_id : Int
        , content : String
        , is_yes : Bool
        , exercise_part_code : String
        }
    , resultData : AskResult
    , askExerList : List AskExer
    }

type alias AskExerData = 
    { data : List AskExer }

type alias AskExer = 
    { difficulty_name :  String
    , duration :  String 
    , exercise_part_name :  String
    , id : Int
    , mediaid :  String 
    , thembnail :  String
    , title :  String }

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

type alias AskYours = 
    { data : AskYourData }

type alias AskYourData =
    { content : String
    , default : Maybe Bool
    , items : List AskItems
    , name : String }

type alias AskItems =
    { text : String
    , value : Bool}

type alias AskYoursPoint = 
    { data : AskYourDataPoint }

type alias AskYourDataPoint =
    { content : String
    , default : Maybe Bool
    , items : List AskItemsPoint
    , name : String }

type alias AskItemsPoint =
    { code : String
    , name : String}

type alias AskSearch = 
    { data : List AskSearchData}

type alias AskSearchData = 
    { content : String
    , exercise_part_code : String
    , id : Int }

type alias EtcDataItem = 
    { ask_id : Int
    , content : String
    , is_yes : String 
    , exercise_part_code : String}

type alias AskResultData = 
    { data : AskResult }

type alias AskResult = 
    { ask_no : Int
    , result : AskResultResult 
    }

type alias AskResultResult = 
    { content : String
    , detail : List AskResultDetail
    , part : String
    , target : String  }

type alias AskResultDetail = 
    { content : String
    , name : String
    , sort : Int }

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
    

askAsnwer point male answers session format=
    let 
        format_change = format
        is_yes_to_bool = 
            List.map (\x -> 
                { format_change | content = x.content
                , exercise_part_code = x.exercise_part_code
                , ask_id = x.ask_id
                , is_yes = 
                    if x.is_yes == "true" then
                        True
                    else
                        False
                }
            ) (List.reverse answers)
        answerList answer = 
            Encode.object
                [ ("content", Encode.string answer.content)
                , ("exercise_part_code", Encode.string answer.exercise_part_code)
                , ("ask_id", Encode.int answer.ask_id)
                , ("is_yes", Encode.bool answer.is_yes)]
        body = 
            Encode.object   
                [ ("exercise_point_code", Encode.string point)
                , ("is_male", Encode.bool male)
                , ("answers", Encode.list answerList is_yes_to_bool)
                ] |> Http.jsonBody    
    in
    Api.post Endpoint.askAnswer (Session.cred session) AskAnswerComplete body Decoder.resultD

askExerData session = 
    Api.get GetListComplete Endpoint.askExer (Session.cred session) (Decoder.askExer AskExerData AskExer)

init : Session -> Bool ->(Model, Cmd Msg)
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
        , contentsId = 0
        , pageNum = 1
        , show = ""
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
        , isActive = "paperweight"
        , askSelected = False
        , askyours = 
            { content = ""
            , default = Nothing
            , items = []
            , name = ""}
        , askIndex = 1
        , aksYoursPoint = 
            { content = ""
            , default = Nothing
            , items = []
            , name = ""}
        , categoryPaperWeight = ""
        , axerCode = ""
        , askSearchData = []
        , askSearchItem = 
            { content = ""
            , exercise_part_code = ""
            , id = 0 }
        , etcAsk = []
        , currentEtcAsk = 
            { ask_id = 0
            , content = ""
            , is_yes = ""
            , exercise_part_code = ""}
        , yesformat = 
            { ask_id = 0
            , content = ""
            , is_yes = False
            , exercise_part_code = ""}
        , idxSearch = 1
        , trialShow = False
        , resultData = 
            { ask_no = 0 
            , result = 
                { content = ""
                , detail = [] 
                , part = ""
                , target = ""
                }
            }
        , askExerList = []
        }, 
        Cmd.batch 
        [ bodyEncode 1 10 "" session
        , Api.removeJw ()
        , scrollToTop NoOp
        , askExerData session 
        ]
    )

type Msg 
    =  GetData (Result Http.Error GetListData)
    | CheckId Int String
    | SaveIdComplete Encode.Value
    | SessionCheck Encode.Value
    | GotSession Session
    | Delete
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | PageBtn (Int, String)
    | OnLoad
    | ScrollEvent ScreenInfo
    | NoOp
    | DeleteConfirm Int
    | IsActive String
    | SelectedAnswer String String
    | GetQuestions (Result Http.Error AskYours)
    | GetQuestions2 (Result Http.Error AskYoursPoint)
    | AskYourDataPost (Result Http.Error AskSearch)
    | EtcAsk EtcDataItem String
    | CompletePaperWeight
    | ProgressComplete Encode.Value
    | CloseTrial
    | AskAnswerComplete (Result Http.Error Decoder.Success)
    | AskResultComplete (Result Http.Error AskResultData)
    | GetListComplete (Result Http.Error AskExerData)
    | AskRecommendComplete (Result Http.Error Decoder.Success)

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
        , Api.progressComplete ProgressComplete
    ]

onLoad msg =
    on "load" (Decode.succeed msg)

askYourDataPost model = 
    let
        body =
            Encode.object 
                [("is_male", Encode.bool model.askSelected)
                ,("exercise_point_code",  Encode.string model.axerCode)]
                    |> Http.jsonBody
    in
    
    Api.post Endpoint.askSearch (Session.cred model.session) AskYourDataPost body (Decoder.askSearch AskSearch AskSearchData)

askYourData model msg endpoint decoder= 
    Api.get msg endpoint (Session.cred model.session) decoder

indexItem idx item = 
    case List.head(List.drop (idx - 1)  item) of
        Just a ->
            a
    
        Nothing ->
            { content = ""
            , exercise_part_code = ""
            , id = 0 }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AskRecommendComplete(Ok ok) ->
            (model, askExerData model.session)
        AskRecommendComplete(Err err) ->
            (model, Cmd.none)
        GetListComplete (Ok ok)->
            ({model | askExerList = ok.data }, Cmd.none)
        GetListComplete (Err err)->
            (model, Cmd.none)
        AskResultComplete (Ok ok) ->
            ({model | resultData = ok.data},scrollToTop NoOp)
        AskResultComplete (Err err) ->
            (model, Cmd.none)
        AskAnswerComplete (Ok ok) ->
            (model, Cmd.none)
        AskAnswerComplete (Err err) ->
            (model, Cmd.none)
        CloseTrial ->   
            ({model | trialShow = not model.trialShow}, Cmd.none)
        ProgressComplete complete ->
            ( {model | categoryPaperWeight = "paperweightResult"}, 
             Api.get AskResultComplete Endpoint.askResult (Session.cred model.session ) (Decoder.askResultData AskResultData AskResult AskResultResult AskResultDetail) )
        CompletePaperWeight ->
            (model, Cmd.batch[Api.progressGo ()
            , askAsnwer model.axerCode model.askSelected model.etcAsk model.session model.yesformat])
        EtcAsk etcData direction->
            let
                initValue = 
                    { ask_id = 0
                    , content = ""
                    , is_yes = "" 
                    , exercise_part_code = ""}
                f =
                    List.member etcData.ask_id (List.map (\x -> x.ask_id) model.etcAsk)
                m = 
                    List.map(\x -> 
                                if x.ask_id == etcData.ask_id then
                                    {x | is_yes = etcData.is_yes}
                                else
                                    x
                        ) model.etcAsk
                idValueNext = indexItem (model.idxSearch + 1) model.askSearchData
                idValueBefore = indexItem (model.idxSearch - 1) model.askSearchData
                nextR = 
                    case List.head (List.filter(\x -> x.ask_id == idValueNext.id ) model.etcAsk) of
                        Just a ->
                            a
                    
                        Nothing ->
                            initValue
                beforeR = 
                    case List.head (List.filter(\x -> x.ask_id == idValueBefore.id ) model.etcAsk) of
                    Just a ->
                        a
                
                    Nothing ->
                        initValue
            in
            case direction of
                "saveNnext" ->
                    if f then
                    ({model | etcAsk = m,   askSearchItem = indexItem (model.idxSearch + 1) model.askSearchData , idxSearch = model.idxSearch + 1, currentEtcAsk = nextR},Cmd.none) 
                    else
                    ({model | etcAsk =  etcData :: model.etcAsk,   askSearchItem = indexItem (model.idxSearch + 1) model.askSearchData , idxSearch = model.idxSearch + 1, currentEtcAsk = nextR},Cmd.none)   
                "saveNComplete" ->
                    if f then
                    update (SelectedAnswer "completePaperWeight" "") {model | etcAsk = m,   askSearchItem = indexItem (model.idxSearch + 1) model.askSearchData , idxSearch = model.idxSearch + 1, currentEtcAsk = nextR} 
                    else
                    update (SelectedAnswer "completePaperWeight" "") {model | etcAsk =  etcData :: model.etcAsk,   askSearchItem = indexItem (model.idxSearch + 1) model.askSearchData , idxSearch = model.idxSearch + 1, currentEtcAsk = nextR}  
                "saveNbefore" ->
                        if model.idxSearch <= 1 then
                        ({model | categoryPaperWeight = "exerpoint", etcAsk = [], currentEtcAsk = initValue}, Cmd.none)
                        else
                            if f then
                            ({model | etcAsk = m,   askSearchItem = indexItem (model.idxSearch - 1) model.askSearchData , idxSearch = model.idxSearch - 1, currentEtcAsk = beforeR},Cmd.none) 
                            else
                            ({model |  etcAsk =  etcData :: model.etcAsk,   askSearchItem = indexItem (model.idxSearch - 1) model.askSearchData , idxSearch = model.idxSearch - 1, currentEtcAsk = beforeR},Cmd.none) 
                _ ->
                    ({model | currentEtcAsk = etcData}, Cmd.none)
        AskYourDataPost (Ok ok)->
            ({model | askSearchData = ok.data, askSearchItem = indexItem 1 ok.data }, Cmd.none)
        AskYourDataPost (Err err)->
            (model, Cmd.none)
        GetQuestions2 (Ok ok) ->
            ({model | aksYoursPoint = ok.data }, Cmd.none)
        GetQuestions2 (Err err) ->
            (model, Cmd.none)
        GetQuestions (Ok ok) ->
            ({model | askyours = ok.data, askSelected = caseItem ok.data.default True}, Cmd.none)
        GetQuestions (Err err) ->
            (model, Cmd.none)
        SelectedAnswer category answer -> 
                case category of
                "sex" ->
                 ({model | askSelected = if answer == "true" then True else False, categoryPaperWeight = category, askIndex = 1, etcAsk = []}, Cmd.none)   
                "exerpoint" ->
                    ({model | categoryPaperWeight = category, askIndex = 2, axerCode = answer, etcAsk = []} , askYourData model GetQuestions2 Endpoint.askExercise_point (Decoder.askyoursPoint AskYoursPoint AskYourDataPoint AskItemsPoint))
                "etcStart" ->
                    ({model | askIndex = 2, categoryPaperWeight = category, etcAsk = [], idxSearch = 1}, askYourDataPost model )
                "completePaperWeight" ->
                    update CompletePaperWeight {model | categoryPaperWeight = category}
                _ ->
                 (model, Cmd.none)   
        IsActive category ->
            case category of
                "paperweight" ->
                    ({model | isActive = category, etcAsk = [], askIndex = 1, categoryPaperWeight = "", axerCode = "", askSelected = caseItem model.askyours.default True} , Cmd.none )
            
                "makeExer" ->
                    ({model | isActive = category, etcAsk = [], askIndex = 1, categoryPaperWeight = "", axerCode = "", askSelected = caseItem model.askyours.default True} , Cmd.none)
                "paperweightStart" ->
                    ({model | isActive = category, categoryPaperWeight = "sex", axerCode = "", askSelected = caseItem model.askyours.default True} , Cmd.batch[askYourData model  GetQuestions Endpoint.askgender (Decoder.askyours AskYours AskYourData AskItems)
                    , Api.hideFooter ()])
                "paperWeightConfirm" ->
                    -- if model.check then
                    -- ({model | isActive = "reset"}, Cmd.none)
                    -- else
                    ({model | isActive = "reset"}, scrollToTop NoOp)
                "newRecommend" ->
                    ({model | isActive = "recommend"}, Cmd.none)
                "paperweightRecommend" ->
                    ({model | isActive = "paperweight"}, Cmd.batch[Api.post Endpoint.askRecommend (Session.cred model.session) AskRecommendComplete Http.emptyBody Decoder.resultD]) 
                "paperweightComplete" ->
                    ({model | isActive = "paperweight", categoryPaperWeight = ""}, askExerData model.session )
                _ ->
                    (model, Cmd.none)
        DeleteConfirm id ->
            if id == 0 then
            ({model | show = ""}, Cmd.none)
            else
            ({model | contentsId = id, show = "logoutShow"}, Cmd.none)
        NoOp ->
            (model, Cmd.none)
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
                    ({model | page = idx, pageNum = model.pageNum - 1}, bodyEncode idx model.per_page model.title model.session)
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, bodyEncode idx model.per_page model.title model.session)
                "go" -> 
                    ({model | page = idx}, bodyEncode idx model.per_page model.title model.session)
                _ ->
                    (model, Cmd.none)
        DeleteSuccess (Ok ok) ->
            ({model | show = ""}, Cmd.batch [
                bodyEncode model.page model.per_page model.title model.session
                , Api.showToast (Encode.string "삭제되었습니다.")
            ])
        DeleteSuccess (Err err) ->
            ({model | show = ""}, Api.showToast (Encode.string "삭제할 수 없는 게시물입니다."))
        Delete ->
            (model, 
            Decoder.resultD
            |> Api.get DeleteSuccess (Endpoint.makeDelete (String.fromInt (model.contentsId)))(Session.cred model.session)  )
        GotSession session ->
            ({model | session = session}
            , bodyEncode model.page model.per_page model.title session
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
            Route.pushUrl (Session.navKey model.session) Route.MakeDetail
            -- Api.historyUpdate (Encode.string "makeExerciseDetail")
            )
            else 
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.TogetherW
            -- Api.historyUpdate (Encode.string "togetherWrite")
            )
        CheckId id str->
            let
                save = Encode.int id
            in
            ({model | saveCheckVal = str},Api.saveId save)
        GetData (Ok ok) -> 
            if model.check then
                if ok.data == [] then
                ({model | getlistData = ok, newList = model.newList ++ ok.data, infiniteLoading = False, loading =False}, Cmd.none)
                else
                ({model | getlistData = ok, page = model.page + 1, newList = model.newList ++ ok.data, infiniteLoading = False, loading = False}, (scrollToTop NoOp))
            else 
                ({model | getlistData = ok, loading =False}, Cmd.none)
        GetData (Err err) -> 
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, Cmd.batch[(Session.changeInterCeptor (Just serverErrors) model.session)
            ])
            
            

view : Model -> {title : String , content : Html Msg}
view model =
    case model.check of
        True ->
            case model.loading of
                True ->
                    { title = "맞춤운동"
                    , content =
                        div [] [
                        div [Route.href Route.MSearch]
                        [appHeaderSearch "맞춤운동" "makeExerHeader"]
                        , activeTab model
                        , case model.isActive of
                            "paperweight" ->
                                paperWeightStartApp model  
                        
                            "makeExer" ->
                                app model
                            _ ->
                                paperWeightStartApp model       
                        
                        , appdeltelayer model
                        , paperweightStartMobile model
                        , resetLayer "yf_popup" model
                        ]
                    }
                False ->
                    { title = "맞춤운동"
                    , content =
                        div [] [
                        div [Route.href Route.MSearch]
                        [appHeaderSearch "맞춤운동" "makeExerHeader"]
                        , case model.isActive of
                            "paperweight" ->
                                paperWeightStartApp model  
                        
                            "makeExer" ->
                                app model
                            _ ->
                                paperWeightStartApp model       
                        
                        , appdeltelayer model
                        , paperweightStartMobile model
                        , resetLayer "yf_popup" model
                        ]
                    
                    }
    
        False ->
            { title = "맞춤운동"
            , content =
                div [ class "customContainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_customworkout.png" "맞춤운동"
                        , activeTab model
                        , case model.isActive of
                            "paperweight" ->
                                div [][
                                paperWeightBody model 
                                , if List.isEmpty model.askExerList then
                                div [][]
                                else
                                div [class "button mj_new_recommend", onClick (IsActive "newRecommend")][text "새로운 추천"]
                                ] 
                            "makeExer" ->
                                makeExerBody model
                                
                            _ ->
                                paperWeightBody model            
                    ]
                     
                ]
                , paperweightStart model
                , selectedItem model
                , resetLayer "yf_popup" model
            ]
            }
            
        


makeExerBody model = 
    div [ class "customContainerwrap" ]
            [ bodyContentTitle,deltelayer model,
                div [ class "customyf_box2"] [
                    div [ class "make_box_title" ]
                        [ h1 [ class "make_yf_h2" ]
                            [ text "맞춤운동 리스트" ]
                        ],
                    div [] [
                        if List.isEmpty model.getlistData.data then
                            div [class "nopaperWeightResult"] [
                            text "맞춤운동이 없습니다."
                            ]
                        else
                            div[][
                                div [ class "make_boxwrap" ]
                                (List.map bodyItem model.getlistData.data)
                                ,pagination
                                    PageBtn
                                    model.getlistData.paginate
                                    model.pageNum]
                        ]

                ]
            ]
app model =
    div [ class "container", class "scroll", scrollEvent ScrollEvent, style "height" "85vh" ][
        activeTab model
        , appStartBox
        , listTitle
            ,div [] [
                div[](List.map appItemContent model.newList) ,
                if model.infiniteLoading then
                    div [class "loadingPosition"] [
                    infiniteSpinner
                    ]
                else
                    span [] []
            ]
    ]

appStartBox = 
    div [ class "make_m_yf_box" ]
        [ h1 [ class "m_make_yf_h1" ]
            [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
        , a [ class "button is-dark m_make_yf_darkbut", Route.href Route.Filter ]
            [ text "시작하기" ]
        , br []
            []
        ]
paperWeightStartApp model = 
     div [ class "container", class "scroll", scrollEvent ScrollEvent, style "height" "85vh" ][
         activeTab model ,
         div [ class "make_m_yf_box" ]
        [ h1 [ class "m_make_yf_h1" ]
            [ text "유어핏 문진을 통해서 나만의 운동을 만들어보세요!" ]
        , div [ class "button is-dark m_make_yf_darkbut", onClick (
            if List.isEmpty model.askExerList then
            IsActive "paperweightStart"
            else
            IsActive "paperWeightConfirm"
        ) ]
            [ text "시작하기" ]
        , br []
            []
        ]
        , listTitle
            , if List.isEmpty model.askExerList then
                div [class "nopaperWeightResult"] [
                    text "문진운동이 없습니다." ]
            else
                div [] (List.map videoItemApp model.askExerList) 
            , div [class "button reset_mj_exer", onClick (IsActive "newRecommend")] [text "운동 새로 받기"]
    ]
    

listTitle = 
    div [ class "m_make_box_title" ]
        [ h1 [ class "m_make_yf_h2" ]
            [ text "맞춤운동 리스트" ]
        ]

appItemContent item=
    let
        titleReplace = 
            item.title 
                |> String.replace "%26" "&"
                |> String.replace "%25" "%"    
    in
    
        div [ class "m_make_yf_box2" ]
            [ div [ class "m_make_videoimg", onClick (CheckId item.id "") ]
                [ img [ src item.thembnail, onLoad OnLoad ]
                    []
                ]
      
            , div [ class "m_make_yf_box_title", onClick (CheckId item.id "") ]
                [ text (
                    if String.length titleReplace > 10 then
                    String.left 10 titleReplace ++ "..."
                    else
                    titleReplace
                ) ]
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

                , div [ class "button m_makeExercise_dete",onClick (DeleteConfirm item.id) ]
                [ i [ class "far fa-trash-alt" ]
                [], text "삭제" 
            ]
            ]

bodyItem item=
    let
        titleReplace = 
            item.title 
                |> String.replace "%26" "&"
                |> String.replace "%25" "%"    
    in
    div [ class "make_box_card_wrap" ]

    [div [ class "make_videoboxwrap cursor", onClick (CheckId item.id "")]

      [div [class"make_overlay"]
     [i [ class "fas fa-play overlay_makeplay" ][]],

         div [ class "video_image" , onClick (CheckId item.id "")]
            [ img [ class "vpic1",src item.thembnail, alt "dummy_video_image" ]
                []
            ]
        , div [ class "Customtextbox"]
            [ div [ class "m1"  , onClick (CheckId item.id "")]
                [ h1 [ class "make_yf_titlename" ]
                    [ text  (
                    if String.length titleReplace > 10 then
                    String.left 10 titleReplace ++ "..."
                    else
                    titleReplace
                ) ]
                ]
            , div [ class "m2" ]
                [ text (String.dropRight 10 (item.inserted_at)), 
                div [] [
                    i [ class "fas fa-stopwatch" ]
                        []
                        , text " "
                        , text item.duration
                ]
                , p [class "makebtn"]
                    [ div  [ class "button is-dark darkbtn make_share"
                    , onClick (CheckId item.id "share") ]
                        [ i [ class "fas fa-share-square" ]
                            [] , text "공유" 
                        ]
                    , div [ class "button" ,onClick (DeleteConfirm item.id)]
                        [ i [ class "far fa-trash-alt" ]
                            [] , text "삭제" 
                        ]
                    ]
                ]
            ]
        ]
    ]




bodyContentTitle =
          div [ class "make_yf_box" ] 
        
                [ 
            img [ src "image/makeimage.png", alt "makeimage" ]
                []
           ,
                    h1 [ class "make_yf_h1" ]
                [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
             , a [ class "button is-dark make_yf_darkbut", Route.href Route.Filter ]
                [ text "시작하기" ]
            , br []
                []
            ]

appdeltelayer model =
    div [class ("m_delete_post " ++ model.show)] [
         div [ class "yf_delete_popup" ]
            [ h1 [ class "popup_yf" ]
                [ text "게시물을 삭제하시겠습니까?" ]
            , p [ class "yf_logout_butbox" ]
                [ div [ class "button is-light logout_danger2", onClick (DeleteConfirm 0) ]
                    [ text "취소" ]
                , div [ class "button is-danger logout_cencel2", onClick Delete ]
                    [ text "삭제" ]
                ]
            ]
    ]

deltelayer model =
    div [class ("delete_post " ++ model.show)] [
         div [ class "yf_delete_popup" ]
            [ h1 [ class "popup_yf" ]
                [ text "게시물을 삭제하시겠습니까?" ]
            , p [ class "yf_logout_butbox" ]
                [ div [ class "button is-light logout_danger2", onClick (DeleteConfirm 0) ]
                    [ text "취소" ]
                , div [ class "button is-danger logout_cencel2", onClick Delete ]
                    [ text "삭제" ]
                ]
            ]
    ]


paperWeight model = 
    div [ class "make_yf_box" ] 
        
                [ 
            img [ src "image/mj_image.png", alt "makeimage" ]
                []
           ,
                    h1 [ class "make_yf_h1" ]
                [ text "유어핏 문진을 통해서 나만의 운동을 만들어보세요!" ]
             , div [ class "button is-dark make_yf_darkbut",  onClick (
                    if List.isEmpty model.askExerList then
                    IsActive "paperweightStart"
                    else
                    IsActive "paperWeightConfirm"
                )]
                [ text "시작하기" ]
            , br []
                []
            ]

activeTab model =
    ul [class "tabs is-toggle is-fullwidth is-large make_tag" ]
        [ li [ classList [
            ("make_tag_li" , True)
            , ("is-active ", model.isActive == "paperweight" ||model.isActive ==  "reset" ||model.isActive ==  "recommend")
        ], onClick (IsActive "paperweight") ]
            [  text "문진 맞춤운동" 
                
            ]
        , li [ classList [
            ("make_tag_li" , True)
            , ("is-active ", model.isActive == "makeExer")
        ], onClick (IsActive "makeExer") ]
            [  text "직접 만들기" 
            ]
        ]
paperWeightBody model =
    div [ class "customContainerwrap" ]
            [ paperWeight model 
            , deltelayer model,
                div [ class "mj_yf_box make_boxwrap"] [
                    div [ class "mj_box_title" ]
                        [ h1 [ class "mj_yf_title" ]
                            [ text "문진운동 리스트" ]
                        ]
                    , if List.isEmpty model.askExerList then
                        div [class "nopaperWeightResult"] [
                            text "문진운동이 없습니다."
                        ]
                    else
                        div [class "makeExerMjBoxWrap_Container"] ( List.map videoItem model.askExerList)
                ]
            ]

stopPaperWeight = 
    div [ class "button is-link is-medium mj_backbtn" , onClick (IsActive "paperweight")]
            [ text "닫기" ]

paperweightStart model = 
    div [ classList 
        [ ( "mj_yf_box" , model.categoryPaperWeight /= "")
        , ( "paperweightLayer", model.isActive == "paperweightStart") ] ]
    [ 
    case model.categoryPaperWeight of
        "sex" ->
            div [class "paperweightStartItem" ]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text ("문진 맞춤 운동 ( "++ String.fromInt (model.askIndex) ++  " / 2 )") ]   
            ]
            , paperweightSex model "mj_text_web" "mj_movebtn" "mj_boxwrap_web"
            , stopPaperWeight
            ]
    
        "exerpoint" ->
            div [class "paperweightStartItem" ]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text ("문진 맞춤 운동 ( "++ String.fromInt (model.askIndex) ++  " / 2 )") ]
            ]
             , paperweightPoint model "mj_text_web" "mj_movebtn" "mj_boxwrap_web"
             , stopPaperWeight
            ]
        "etcStart" ->
            div [class "paperweightStartItem" ]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text ("문진 맞춤 운동 ( "++ String.fromInt (model.idxSearch) ++  " / " ++ String.fromInt (List.length model.askSearchData )++ " )") ]
            ]
            , etcAsk model "mj_text_web" "mj_movebtn" "mj_boxwrap_web"
            , stopPaperWeight
            ]
        "completePaperWeight" ->
            div [class "paperweightStartItem" ]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text "문진 결과" ]
            ]
            , div [class "progressTextWrap"] [
                div [class "progressText"][
                    ul [class "progressTextul", id "progressText"]
                    [li [] [ text "작성한 문진 데이터를 입력 중입니다."]
                    ,li [] [ text "문진 데이터에 맞춰 체형을 분석 중입니다."]
                    ,li [] [ text "분석데이터를 통해 맞춤운동을 생성합니다."]
                    ,li [] [ text "유어핏 운동 서버에 접근하고 있습니다."]
                    ,li [] [ text "운동데이터를 모아 간추리고 있습니다."]
                    ,li [] [ text "개인별 맞춤 운동을 생성하고 있습니다."]
                ]
                ]
            ]
            , progress [ class "progress is-large is-info", Attr.max "100" , id "paperWeightProgress"]
                [ text "60%" ]
            ]
        "paperweightResult" ->
            div [class "paperweightStartItem" ]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text "문진 맞춤 운동결과" ]
            ]
            , paperweightAnswer model.resultData
            ]
        _ ->
            div [][
            ]
    ]


paperweightStartMobile model = 
    div [ 
        class ("container myaccountStyle " ++ (if model.isActive == "paperweightStart" then "account" else "") ++ (if model.categoryPaperWeight == "paperweightResult" then " paperWeightResultStyle" else ""))
        , id (if model.isActive == "paperweightStart" && model.categoryPaperWeight /= "paperweightResult" then "noScrInput" else "")
        ]
    [ 
    case model.categoryPaperWeight of
        "sex" ->
            div [class "inheritHeight"]
            [ 
            appHeaderRDetailClick  ("문진 맞춤 운동 ( "++ String.fromInt (model.askIndex) ++  " / 2 )") "makeExerHeader paperweightmobileFontsize" (IsActive "paperweight") "fas fa-times"
            , paperweightSex model "mj_text" "m_mj_move_btn" "mj_boxwrap"
            ]
    
        "exerpoint" ->
            div [class "inheritHeight"]
            [ appHeaderRDetailClick  ("문진 맞춤 운동 ( "++ String.fromInt (model.askIndex) ++  " / 2 )") "makeExerHeader paperweightmobileFontsize" (IsActive "paperweight") "fas fa-times"
             , paperweightPoint model  "mj_text" "m_mj_move_btn" "mj_boxwrap"
            ]
        "etcStart" ->
            div [class "inheritHeight"]
            [ appHeaderRDetailClick  ("문진 맞춤 운동 ( "++ String.fromInt (model.idxSearch) ++  " / " ++ String.fromInt (List.length model.askSearchData )++ " )") "makeExerHeader paperweightmobileFontsize" (IsActive "paperweight") "fas fa-times"
            , etcAsk model "mj_text" "m_mj_move_btn" "mj_boxwrap"
            ]
        "completePaperWeight" ->
            div [class "inheritHeight"]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text "문진 결과" ]
            ]
            , div [class "progressTextWrap"] [
                div [class "progressText"][
                    ul [class "progressTextul", id "progressText"]
                    [li [] [ text "작성한 문진 데이터를 입력 중입니다."]
                    ,li [] [ text "문진 데이터에 맞춰 체형을 분석 중입니다."]
                    ,li [] [ text "분석데이터를 통해 맞춤운동을 생성합니다."]
                    ,li [] [ text "유어핏 운동 서버에 접근하고 있습니다."]
                    ,li [] [ text "운동데이터를 모아 간추리고 있습니다."]
                    ,li [] [ text "개인별 맞춤 운동을 생성하고 있습니다."]
                ]
                ]
            ]
            , progress [ class "progress is-large is-info", Attr.max "100" , id "paperWeightProgress"]
                [ text "60%" ]
            ]
        "paperweightResult" ->
            div [class "individual_paperweightResult"]
            [ div [ class "mj_box_title" ]
            [ h1 [ class "mj_yf_title" ]
                [ text "문진 맞춤 운동결과" ]
            ]
            , paperweightAnswer model.resultData
            ]
        _ ->
            div [][
            ]
    ]

paperweightAnswer item = 
    div [ class "mj_boxwrap2"]
    [ div [ class "mj_result_box1" ]
        [ p [ class "mj_result_text1" ]
            [ text item.result.target
            ]
        ]
    , div [ class "mj_result_box1" ]
        [ p [ class "mj_result_text1" ]
            [ text item.result.part
            ]
        ]
    , div [ class "mj_result_box1" ]
        [ p [ class "mj_result_text1" ]
            [ text item.result.content ]
        ]
    , div [ class "mj_result_box1" ]
        [ div [ class "columns mj_columns" ]
            (List.map (\x -> paperweightAnswerDetail x) (List.sortBy .sort item.result.detail) )
        ]
    , div [ class "mj_result_box1" ]
        [ p [ class "mj_result_text3" ]
            [ text "회원님의 문진데이터를 분석하여 유어핏 맞춤형 운동이 부위와 난이도 맞게 자동추천합니다." ]
        , div [ class "button is-link is-medium mj_gobtn" , onClick (IsActive "paperweightComplete")]
            [ text "맞춤 운동 보러가기" ]
        ]
    ]

paperweightAnswerDetail item = 
            div [ class "column" ]
                [ text item.name , div [ class "mj_columns_icon" ]
                [ 
                case item.name of
                    "상체" ->
                        img [ src "../image/mj_icon1.png", alt "logo" ]
                        []
                
                    "하체" ->
                        img [ src "../image/mj_icon3.png", alt "logo" ]
                        []
                    "복부" ->
                        img [ src "../image/mj_icon2.png", alt "logo" ]
                        []
                    _ ->
                        img [ src "../image/mj_icon1.png", alt "logo" ]
                        []
                ]
                ,  case item.content of
                        "기본" ->
                            p [ class "mj_customtext2" ]
                            [ text item.content ]
                    
                        "최고" ->
                            p [ class "mj_customtext3" ]
                            [ text item.content ]
                        "노력" ->  
                            p [ class "mj_customtext1" ]
                                [ text item.content ]
                            
                        _ ->
                            p [ class "mj_customtext2" ]
                            [ text item.content ]
        
                ]

etcAsk model textStyle moveBtn boxStyle= 
    div [ class boxStyle ]
        [ p [ class  textStyle ]
            [ text model.askSearchItem.content ]

        , p [class "control answer_box"][
            label [ classList 
                [(("answer_btn"), True)
                , ("etcAnswerSelected" , model.currentEtcAsk.is_yes == "true")
                ] , onClick (EtcAsk {ask_id = model.askSearchItem.id, content = model.askSearchItem.content , is_yes = "true" , exercise_part_code = model.askSearchItem.exercise_part_code} "" )
                 ]
                [  text "예"
                ]
            , label [ classList 
                [(("answer_btn"), True)
                , ("etcAnswerSelected" , model.currentEtcAsk.is_yes == "false")
                ] , onClick (EtcAsk {ask_id = model.askSearchItem.id, content = model.askSearchItem.content , is_yes = "false" , exercise_part_code = model.askSearchItem.exercise_part_code} "")
                 ]
                [  text "아니오"
                ]
        ]
        , p [ class moveBtn ]
            [ div [ class "button  mj_before" , onClick (EtcAsk model.currentEtcAsk "saveNbefore") ]
                [ text "이전" ]
            , if List.length model.askSearchData == model.idxSearch then
                if model.currentEtcAsk.is_yes == "" then
                     div [ class "button mj_disabled mj_next" ]
                    [ text "완료" ]
                else
                     div [ class "button is-dark mj_next" ,  onClick  (EtcAsk model.currentEtcAsk "saveNComplete") ]
                    [ text "완료" ]
            else
                if model.currentEtcAsk.is_yes == "" then
                div [ class "button mj_disabled mj_next" ]
                [ text "다음" ]
                else
                div [ class "button is-dark mj_next" ,  onClick (EtcAsk model.currentEtcAsk "saveNnext") ]
                [ text "다음" ]
            ]
        ]

etcExample idx model item =
                label [ classList 
                [(("answer_btn"++(String.fromInt (idx + 1))), True)
                , ("sexSelected" , model.askSelected == item.is_yes)
                ] 
                , onClick (SelectedAnswer "sex" (if item.is_yes == True then "true" else "false"))
                 ]
                [  text item.text
                ]
paperweightSex model textStyle moveBtn boxStyle=
    div [ class boxStyle ]
        [ p [ class textStyle ]
            [ text model.askyours.content ]
        , p [class "control answer_box"](List.indexedMap (\idx x -> answerExample idx model x )model.askyours.items)
        , p [ class moveBtn ]
            [ div [ class "button is-dark mj_next" ,  onClick (SelectedAnswer "exerpoint" model.axerCode) ]
                [ text "다음" ]
            ]
        ]
paperweightPoint model textStyle moveBtn boxStyle =
    div [ class boxStyle  ]
        [ p [ class textStyle ]
            [ text model.aksYoursPoint.content ]
        , p [class "control answer_box"](List.indexedMap (\idx x -> answerExamplePoint idx model x )model.aksYoursPoint.items)
        , p [ class moveBtn ]
            [ div [ class "button  mj_before" , onClick (SelectedAnswer "sex" (if model.askSelected == True then "true" else "false"))]
                [ text "이전" ]
            , 
            if model.axerCode == "" then
            div [ class "button mj_next  mj_disabled" ]
                [ text "다음" ]
            else
            div [ class "button is-dark mj_next" , onClick (SelectedAnswer "etcStart" "")]
                [ text "다음" ]
            ]
        ]
answerExample idx model item =
                label [ classList 
                [(("answer_btn"++(String.fromInt (idx + 1))), True)
                , ("sexSelected" , model.askSelected == item.value)
                ] 
                , onClick (SelectedAnswer "sex" (if item.value == True then "true" else "false"))
                 ]
                [  text item.text
                ]


answerExamplePoint idx model item =
                label [ classList 
                [(("answer_btn"++(String.fromInt (idx + 1))), True)
                , ("sexSelected" , model.axerCode == item.code)
                ] 
                , onClick (SelectedAnswer "exerpoint" item.code)
                 ]
                [  text item.name
                ]



caseItem item charaterType =
    case item of
        Just i ->
            i
    
        Nothing ->
            charaterType


videoItem item = 
    div [ class "yf_workoutvideoboxwrap makeExerMjboxWrap" , onClick CloseTrial]
        [ div [class"list_overlay"]
        [i [ class "fas fa-play overlayplay_list" ][]],

             div [  ]
                [ 
                    img [ src item.thembnail ]
                    []
                ]
            , div [ class "yf_workoutvideo_lavel_bg" ]
                [ div [ class "level" ]
                    [ text item.difficulty_name ]
                ]
            , div [ class "yf_workoutworkout_title" ]
                [ text (item.title ++ " - " ++ item.exercise_part_name) ]
            , div [ class "m_timebox" ]
                [
                    i [ class "fas fa-stopwatch" ]
                    []
                    , text " "
                    , text item.duration ]
        ]

videoItemApp item = 
    div [ class "mjList_container" ]
        [div [class "mj_wrap"][
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
            , div [class "mjList_title_makeTab"][
            div [ class "yf_workoutworkout_title" ]
                [ text (item.title ++ " - " ++ item.exercise_part_name) ]
            , div [ class "m_timebox" ]
                [
                    i [ class "fas fa-stopwatch" ]
                    []
                    , text " "
                    , text item.duration
                    ]
                    
            ]
        ]


selectedItem model = 
    div [class "paperweightLayer", style "display" 
        (if model.trialShow then "flex" else "none")
        ]
        [
        div [class "paperweightStartItem paperweightSelectedItem_container"]
        [
            div[class "paperweightSelectedItem_first"][
            img [src "image/dummy_video_image.png"][]
            , div []
            [ div [class "mj_title"][text "복근 만들기 운동"
            , span [class "mj_title_part"][text "복부 - 상"]
            ]
            , span [class "mj_title_duration"]
            [ i [ class "fas fa-stopwatch" ] []
            , text " "
            , text "duration"
            ]
            , ul [class "mj_description"]
            [ li [][text "1. helloworld"]
            , li [][text "2. helloworld"]
            ]
            ]
        ]
        , div [class "paperweightSelectedItem_second"][
            h3 [][text "운동설명"]
            , div [class "description"][
                text "운동설명 123123123"
            ]
        ]
        , div [class "paperweightSelectedItem_third"]
        [ div [class "button is-link"][text "1주일 무료 체험"]
        , div [class "button is-danger", onClick CloseTrial][text "닫기"]
        ]
        ]
    ]

-- "myf_popup" 
-- "yf_popup"
resetLayer layerStyle model =
    div [ class "layerStyleWarn", style "display" ( if model.isActive == "reset" || model.isActive == "recommend" then "flex" else "none"), id ( if model.isActive == "reset" || model.isActive == "recommend" then "noScrInput" else "") ] [
    div [ class layerStyle ]
    [ img [ src "/image/setting_icon2.png", style "width" "20%" ]
        []
    , h1 [ class "popup_yf_h1", style "font-size" "1rem" ]
        [ text (
            if model.isActive == "reset" then
                "새로운 문진을 진행하시면 기존의 문진 리스트는 초기화 됩니다."
            else
                "새로운 추천을 받으면 기존의 문진 리스트는 초기화 됩니다."
        ) ]
    , p [ class "yf_logoout_butbox" ]
        [ div [ class "button is-danger logout_danger" 
        , onClick (
            if model.isActive == "reset" then
            IsActive "paperweightStart"
            else
            IsActive "paperweightRecommend"
        ) ]
            [ text "확인"]
        , div [ class "button is-light logout_cencel" , onClick (IsActive "paperweight")]
            [ text "취소" ]
        ]
    ]
    ]

