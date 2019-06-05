module Page.MyPageMenu.MyStatistical exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Json.Encode as E
import Json.Decode as Decode
import Route exposing (..)
import Page.MyPageMenu.MyPageInfoLayout exposing (..)
import Page.Common exposing(..)
import Api as Api
import Http as Http
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import LineChart
import LineChart.Colors as Colors exposing (..)
import Time exposing (Month(..))
import Task exposing (Task)
import Date exposing (..)
import LineChart as LineChart
import LineChart.Junk as Junk exposing (..)
import LineChart.Dots as Dots
import LineChart.Container as Container
import LineChart.Junk as Junk
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis as Axis
import LineChart.Line as Line
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Legends as Legends
import LineChart.Area as Area
import Color
import LineChart.Axis.Range as Range
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Title as Title

type alias Model 
    = {
        session : Session,
        activeTab : String,
        checkDevice : String
        , check : Bool
        , statistics : List  Statistical
        , maxTime : Float
        , minTime : Float
        , weightData : List Float
        , hovering : Maybe Point
        , barChartForm : List Info
        , date : String
        , hover : String
        , dateList : List String
        , todayWeight : Float
        , exerciseChartForm : List String
        , exerciseData : List Float
        , maxTimeString : String
        , calData : List String
        , maxCal : Float
        , minCal : Float
        , maxCalString : String
        , calFloatData : List Float
        , hovered : Maybe Info
        , dateToInt : List Int
        , monthExerLine : List Info
        , monthCalLine : List Info
        , weightBarChart : List Float
        , weightBarChartString : List String
        , maxWeight : Float
        , minWeight : Float
        , showInfo : Bool
        , getWeightInfo : String
        , getWeightInfoDate : String
        , getExerciseInfo: String
        , getExerciseInfoDate : String
        , getCalInfo : String
        , getCalInfoDate : String
        , selectedChart : String
        , currentDay : Date
        , currentMonth : String
        , today : String
        , hoverWeight : Maybe Info
        , hoverExercise : Maybe Info
        , hoverCal : Maybe Info
    }
type alias BarChartForm = 
    { label : String
    , heights : List Float }

type alias StatisticalData = 
    { data : List Statistical }

type alias Statistical = 
    { date : String
    , exercise : Maybe String
    , food : Maybe String
    , weight : Maybe String }

weekApi session = 
    Decoder.statisticalWeek StatisticalData Statistical
    |>Api.get GetList (Endpoint.statisticalweek) (Session.cred session) 

monthApi session date = 
    Decoder.statisticalWeek StatisticalData Statistical
    |> Api.get GetMonthList (Endpoint.statisticalMonth date )
    (Session.cred session)

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , activeTab = "weekly"
        , check = mobile
        , checkDevice = ""
        , statistics = []
        , hovering = Nothing
        , maxTime = 1
        , minTime = 0
        , barChartForm = []
        , date = ""
        , weightData = [] 
        , todayWeight = 0
        , hovered = Nothing
        , hover = ""
        , dateToInt = []
        , exerciseChartForm = []
        , dateList = []
        , exerciseData = []
        , maxTimeString = ""
        , calData = []
        , maxCal = 0
        , minCal = 0
        , maxCalString = ""
        , calFloatData = []
        , monthExerLine = []
        , monthCalLine = []
        , weightBarChartString = []
        , weightBarChart = []
        , maxWeight = 0
        , minWeight = 0
        , showInfo = False
        , getWeightInfo = "" 
        , getWeightInfoDate = "" 
        , getExerciseInfo = ""
        , getExerciseInfoDate = ""
        , getCalInfo = ""
        , getCalInfoDate = ""
        , selectedChart = ""
        , currentDay = Date.fromCalendarDate 2019 Jan 1
        , currentMonth = ""
        , today = ""
        , hoverWeight = Nothing
        , hoverExercise = Nothing
        , hoverCal = Nothing
        }
        , Cmd.batch[ Date.today |> Task.perform ReceiveDate]
    )



type Msg 
    = ActiveTab String
    | CheckDevice E.Value
    | BackBtn
    | GetList (Result Http.Error StatisticalData)
    | Hover String (Maybe Info)
    | ReceiveDate Date
    | GetMonthList (Result Http.Error StatisticalData)
    | ShowInfo String String String
    | ChangeDate String
    | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    Session.changes GotSession (Session.navKey model.session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ({model | session = session}, 
            case model.activeTab of
                "weekly" ->
                    weekApi model.session
            
                "monthly" ->
                    monthApi model.session model.today
                _ ->
                     weekApi model.session
                    )
        ChangeDate when ->
            let
                formatDate month = 
                    Date.add Date.Months month model.currentDay 
                formatDateString month = 
                    getFormattedDate Nothing (Just (formatDate month))   
            in
            case when of
                "next" ->
                    ({model | date = formatDateString 1, currentDay = formatDate 1}, monthApi model.session (formatDateString 1))
                "before" ->
                    ({model | date = formatDateString -1, currentDay = formatDate -1}, monthApi model.session (formatDateString -1))
                _ ->
                    (model, Cmd.none)
        ShowInfo date data category ->
            case category of
                "cal" ->
                    ({model | getCalInfo = data, getCalInfoDate = date,  selectedChart = "selectedChart"}, Cmd.none)
                "exercise" ->
                    ({model | getExerciseInfo = data, getExerciseInfoDate = date,  selectedChart = "selectedChart"}, Cmd.none)
                "weight" ->
                    ({model | getWeightInfo = data, getWeightInfoDate = date,  selectedChart = "selectedChart"}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        GetMonthList (Ok ok) ->
            let 
                weightChartData = List.indexedMap (\i x -> 
                    {x = 
                        (case String.toFloat (String.dropLeft 6 (String.replace "-" "" (x.date))) of
                            Just int ->
                                int
                        
                            Nothing ->
                                0)
                        , y = (justString x.weight (listHeaderweight todayweight))}
                     ) ok.data
                weightBarChartString = 
                    List.indexedMap (\i x -> 
                    case x.weight of
                        Just w ->
                            w
                    
                        Nothing ->
                            "기록없음"
                    ) ok.data
                weightBarChart = 
                    List.map (\x ->
                        case x.weight of
                            Just w ->
                                case String.toFloat w of
                                    Just f ->
                                        f
                                
                                    Nothing ->
                                        0
                        
                            Nothing ->
                                0
                        ) ok.data
                monthExerLine = 
                    List.indexedMap (\i x -> 
                        {x = 
                            (case String.toFloat(String.dropLeft 6 (String.replace "-" "" (x.date))) of
                                Just int ->
                                    int
                            
                                Nothing ->
                                    0)
                        , y = 
                            (Basics.toFloat  (exerciseCase x.exercise 0 2) * 60) + Basics.toFloat (exerciseCase x.exercise 3 5) + (Basics.toFloat (exerciseCase x.exercise 6 8) / 100)
                        }
                     ) ok.data
                monthCalLine = 
                    List.indexedMap (\i x -> 
                        {x = 
                            (case String.toFloat (String.dropLeft 6 (String.replace "-" "" (x.date))) of
                                Just int ->
                                    int
                            
                                Nothing ->
                                    0)
                        , y = 
                            case x.food of
                            Just f ->
                                case String.toFloat f of
                                    Just float ->
                                        float
                                
                                    Nothing ->
                                        0
                                        
                        
                            Nothing ->
                                0
                        }
                     ) ok.data
                todayweight = 
                    List.filter (\x ->
                    x.date == model.date
                    ) ok.data
                exerciseChartData = List.indexedMap (\i x -> 
                    case x.exercise of
                        Just exer ->
                            exer
                    
                        Nothing ->
                            "기록없음"
                    ) ok.data

                exerciseChartMinMaxData = 
                    List.indexedMap (\i x ->
                       (Basics.toFloat  (exerciseCase x.exercise 0 2) * 60) + Basics.toFloat (exerciseCase x.exercise 3 5) + (Basics.toFloat (exerciseCase x.exercise 6 8) / 100)
                    ) ok.data
                
                maxstring = 
                    List.filter (\x -> 
                         (Basics.toFloat  (exerciseCase x.exercise 0 2) * 60) + Basics.toFloat (exerciseCase x.exercise 3 5) + (Basics.toFloat (exerciseCase x.exercise 6 8) / 100) == exerMax exerciseChartMinMaxData
                    )ok.data
                dateInt = 
                    List.map (\x -> 
                        case String.toFloat (String.replace "-" "" x) of
                            Just int ->
                                int
                        
                            Nothing ->
                                0
                    )dateData
                
                exerMin item = justCase (List.minimum item)
                exerMax item  = justCase (List.maximum item)
                dateData = 
                    List.map (\x -> x.date) ok.data
                caloriesData = 
                    List.map (\x -> 
                        case x.food of
                            Just f ->
                                f
                        
                            Nothing ->
                                "기록없음"
                    ) ok.data

                calMinMaxData = 
                    List.map (\x ->
                        case String.toFloat x of
                            Just float ->
                                float
                        
                            Nothing ->
                                0
                    ) caloriesData
                
            in
            ({model | statistics = ok.data, barChartForm = weightChartData, exerciseChartForm = exerciseChartData, dateList = dateData, minTime = exerMin  exerciseChartMinMaxData, maxTime = exerMax exerciseChartMinMaxData, exerciseData = exerciseChartMinMaxData
            , maxTimeString = 
                case List.head maxstring of
                    Just exer ->
                        case exer.exercise of
                            Just a ->
                                a
                        
                            Nothing ->
                                ""
                
                    Nothing ->
                        ""
            , calData = caloriesData
            , minCal = exerMin calMinMaxData
            , maxCal = exerMax calMinMaxData
            , maxCalString = String.fromFloat (exerMax calMinMaxData)
            , maxWeight = exerMax weightBarChart
            , minWeight = exerMin weightBarChart
            , calFloatData = calMinMaxData
            , monthExerLine = monthExerLine
            , monthCalLine = monthCalLine
            , weightBarChart = weightBarChart
            , weightBarChartString = weightBarChartString
            , selectedChart = ""
            , getCalInfo = ""
            , getExerciseInfo = ""
            , getWeightInfo = ""
             }, Cmd.none)
        GetMonthList (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Cmd.none)
        ReceiveDate today ->
            ({model | date = getFormattedDate Nothing (Just today),  currentDay = today, today = getFormattedDate Nothing (Just today)} , 
            case model.activeTab of
                "weekly" ->
                    weekApi model.session
            
                "monthly" ->
                    Cmd.none
                _ ->
                    Cmd.none
            )
        Hover string hovered ->
            case string of
                "운동" ->
                   ({model | hoverExercise = hovered }, Cmd.none) 
            
                "칼로리" ->
                    ({model | hoverCal = hovered }, Cmd.none)
                "체중" ->
                    ({model | hoverWeight = hovered }, Cmd.none)
                _ ->
                    ({model | hovered = hovered }, Cmd.none)
        GetList (Ok ok) ->
            let
                weightBarChartString = 
                    List.indexedMap (\i x -> 
                    case x.weight of
                        Just w ->
                            w
                    
                        Nothing ->
                            "기록없음"
                    ) ok.data
                weightBarChart = 
                    List.map (\x ->
                        case x.weight of
                            Just w ->
                                case String.toFloat w of
                                    Just f ->
                                        f
                                
                                    Nothing ->
                                        0
                        
                            Nothing ->
                                0
                        ) ok.data
                weightChartData = List.indexedMap (\i x -> 
                    {x = 
                        (case String.toFloat (String.replace "-" "" x.date) of
                            Just int ->
                                int
                        
                            Nothing ->
                                0)
                        , y = (justString x.weight (listHeaderweight todayweight))}
                     ) ok.data
                todayweight = 
                    List.filter (\x ->
                    x.date == model.date
                    ) ok.data
                exerciseChartData = List.indexedMap (\i x -> 
                    case x.exercise of
                        Just exer ->
                            exer
                    
                        Nothing ->
                            "기록없음"
                    ) ok.data

                exerciseChartMinMaxData = 
                    List.indexedMap (\i x ->
                       (Basics.toFloat  (exerciseCase x.exercise 0 2) * 60) + Basics.toFloat (exerciseCase x.exercise 3 5) + (Basics.toFloat (exerciseCase x.exercise 6 8) / 100)
                    ) ok.data
                
                maxstring = 
                    List.filter (\x -> 
                         (Basics.toFloat  (exerciseCase x.exercise 0 2) * 60) + Basics.toFloat (exerciseCase x.exercise 3 5) + (Basics.toFloat (exerciseCase x.exercise 6 8) / 100) == exerMax exerciseChartMinMaxData
                    )ok.data
                dateInt = 
                    List.map (\x -> 
                        case String.toFloat (String.replace "-" "" x) of
                            Just int ->
                                int
                        
                            Nothing ->
                                0
                    )dateData
                
                exerMin item = justCase (List.minimum item)
                exerMax item  = justCase (List.maximum item)
                dateData = 
                    List.map (\x -> x.date) ok.data
                caloriesData = 
                    List.map (\x -> 
                        case x.food of
                            Just f ->
                                f
                        
                            Nothing ->
                                "기록없음"
                    ) ok.data

                calMinMaxData = 
                    List.map (\x ->
                        case String.toFloat x of
                            Just float ->
                                float
                        
                            Nothing ->
                                0
                    ) caloriesData
                
            in
            ({model | statistics = ok.data, barChartForm = weightChartData, exerciseChartForm = exerciseChartData, dateList = dateData, minTime = exerMin  exerciseChartMinMaxData, maxTime = exerMax exerciseChartMinMaxData, exerciseData = exerciseChartMinMaxData
            , maxTimeString = 
                case List.head maxstring of
                    Just exer ->
                        case exer.exercise of
                            Just a ->
                                a
                        
                            Nothing ->
                                ""
                
                    Nothing ->
                        ""
            , calData = caloriesData
            , minCal = exerMin calMinMaxData
            , maxCal = exerMax calMinMaxData
            , maxCalString = String.fromFloat (exerMax calMinMaxData)
            , calFloatData = calMinMaxData
            , weightBarChart = weightBarChart
            , weightBarChartString = weightBarChartString
            , maxWeight = exerMax weightBarChart
            , minWeight = exerMin weightBarChart
            , selectedChart = ""
            , getCalInfo = ""
            , getExerciseInfo = ""
            , getWeightInfo = ""
             }, Cmd.none)
        GetList (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Cmd.none)    
        ActiveTab item ->
            case item of
                "weekly" ->
                    ( {model | activeTab = item, getCalInfo ="", getExerciseInfo ="" , getWeightInfo ="", getCalInfoDate ="", getExerciseInfoDate ="" , getWeightInfoDate =""
                    , selectedChart = "", date = model.today}, Cmd.batch[weekApi model.session
                    , Date.today |> Task.perform ReceiveDate] )
            
                "monthly" ->
                    ( {model | activeTab = item, getCalInfo ="", getExerciseInfo ="" , getWeightInfo =""
                    , getCalInfoDate ="", getExerciseInfoDate ="" , getWeightInfoDate ="", selectedChart = "", date = model.today}, 
                    Cmd.batch[monthApi model.session model.today
                    , Date.today |> Task.perform ReceiveDate] )
                _ ->
                    ( {model | activeTab = item, getCalInfo ="", getExerciseInfo ="" , getCalInfoDate ="", getExerciseInfoDate ="" , getWeightInfoDate ="", getWeightInfo =""
                    , selectedChart = ""}, Date.today |> Task.perform ReceiveDate )
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
        BackBtn ->
            (model, Route.backUrl(Session.navKey model.session) 1)



type alias Point =
    { x : Float, y : Float }

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
        div [] [
            if model.check then
                app model BackBtn
            else
                web model
               
                
        ]
    }


chart : Model -> String -> String -> List Info -> Maybe Info -> Html.Html Msg
chart model xAcc yAcc data hover=
  LineChart.viewCustom
    { y = Axis.default 600 "" .y
    , x = 
        case model.activeTab of
            "weekly" ->
                Axis.custom
                { title = Title.default ""
                , variable = Just << .x
                , pixels = 1270
                , range = Range.padded 20 20
                , axisLine = AxisLine.full Colors.gray
                , ticks =Ticks.intCustom 7 ticksConfig
                }   
        
            "monthly" ->
                Axis.default 1270 "" .x
            _ ->    
               Axis.default 1270 "" .x 
          
    , container = 
    Container.styled "line-chart-1" [  ("height" , "100%"), ("width" , "100%") ]
    , interpolation = Interpolation.default
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverOne (Hover xAcc)
    , junk =
        -- Junk.default
        if model.activeTab == "weekly" then
        Junk.hoverOne hover
          [ ( xAcc, String.fromFloat << .y )
          ]
        else
        Junk.hoverOne hover
          [ ( xAcc, String.fromFloat << .y )
          , ( yAcc, dateDecoder model << .x)
          ]
    , grid = Grid.default
    , area = Area.default
    , line = Line.default
    , dots = Dots.hoverOne hover
    }
    [ LineChart.line Color.blue Dots.circle "" data
    ]


xAxisConfig : Model -> Axis.Config Info msg
xAxisConfig model =
  Axis.custom
    { title = Title.default "Weight"
    , variable = Just  << .x
    , pixels = 700
    , range = Range.padded 20 20
    , axisLine = AxisLine.rangeFrame Colors.gray
    -- , ticks = ticksConfig model.hovered model
    , ticks = Ticks.intCustom 7 ticksConfig
    }

-- ticksConfig : Maybe Data -> Ticks.Config msg
ticksConfig num =
    let
        s = String.fromInt num
    in
    
    Tick.custom
    { position = toFloat num
    , color = Colors.black
    , width = 1
    , length = 7
    , grid = True
    , direction = Tick.negative
    , label = Just (Junk.label Colors.black (String.dropRight 4 s ++ "-"++ String.slice  4 6 s  ++ "-" ++ (String.dropLeft 6 s))
    )      
    }
someCustomTick number model = 
  Tick.custom
    { position = toFloat number
    , color = Colors.black
    , width = 2
    , length = 2
    , grid = True
    , direction = Tick.negative
    , label = Just (Junk.label Colors.black (String.fromInt number))
    }
type alias Info =
  { x : Float
  , y : Float
  }


listHeader item = 
    case List.head item of
        Just a ->
            a.date
        Nothing ->
            ""



innerItem item data max date unit = 
    --  tr [ class "qtr", id "q1" ]
    --         [
            td [ class ("bar qtr " ++ 
            ( if (String.fromFloat (percentage data max)) == "0" then "" else " sent" ))
            , style "height" (String.fromFloat (percentage data max) ++ "%") , style "min-height" "25%" , style "width" "15%",  id "q1"]
                [
                         text (
                        if item == "기록없음" then
                        item
                        else
                        item ++ unit
                    ) 
                ]
            
appinnerItem item data max date unit model what= 
    --  tr [ class "qtr", id "q1" ]
    --         [
            td [ class ((
            case what of
                "cal" ->
                    if model.getCalInfoDate == date then model.selectedChart else ""
                "exercise" ->
                    if model.getExerciseInfoDate == date then model.selectedChart else ""
                "weight" ->
                    if model.getWeightInfoDate == date then model.selectedChart else ""
                _ ->
                    ""
                    ) ++" bar qtr "  ++ 
            ( if (String.fromFloat (percentage data max)) == "0" then "" else " sent" ))
            , style "width" (if model.activeTab == "monthly" then  "3%" else "100%")
            , style "height" (String.fromFloat (percentage data max) ++ "%") , style "min-height" (if item =="기록없음" then "0" else "20%" )  , id "q1"
            , onClick (ShowInfo date item what) ]
                [
                        --  text (
                        if item == "기록없음" then
                            div [] [text ""]
                        else
                            if model.activeTab == "monthly" then
                            div [style "display" "none"][text item]
                            else
                            div[] [
                            text  item 
                                , div [][text unit]
                            ]
                ]

axisY item =
    div [ class "tick", style"height" "59px" ]
            [ p []
                [ text (String.fromFloat item.sortNum) ]
            ]

dateFooter item =
     
                td []
                    [  text item ]
            
dateIdxFooter idx item =
                td []
                    [  if Basics.modBy 2 idx == 0 then
                    text (String.fromInt (idx + 1))
                    else
                    text ""  ]

calBarChart model = 
    div [class "webbarChartcontainer"][
        div [ id "ticks" ]
        [
            div [ class "tick"]
            [ p []
                [ text (if model.maxCalString == "1" then "" else model.maxCalString ) ]
            ]
            , div [ class "tick"]
            [ p []
                [ text "0.00" ]
            ]
        ]
        , table [ id "q-graph" ]
        [ tbody []
           [tr [style "width" "100%"] (List.map3 (\x y z-> innerItem x y model.maxCal z "Kcal") model.calData model.calFloatData model.dateList)] 
            , tfoot [][
                tr []
            (List.map dateFooter model.dateList)]
        ]
        
    ]
appcalBarChart model = 
    div [class "barChartcontainer", style "padding" "0"][
        table [ id "q-graph" ]
        [ tbody []
           [tr [style "width" "100%"] (List.map3 (\x y z-> appinnerItem x y model.maxCal z "Kcal" model "cal") model.calData model.calFloatData model.dateList)
        --    , tr [] [
        --        td [] []
        --        ]
            ] 
            ,tfoot [][
              case model.activeTab of
                    "weekly" ->
                        tr []
                        (List.map dateFooter appDate)
                
                    "monthly" ->
                        tr []
                            (List.indexedMap dateIdxFooter model.dateList)
                    _ ->
                        tr []
                        (List.indexedMap dateIdxFooter model.dateList)
                    , tr [class "graphGrid"] [
                        td [] []
                        ]
            ]
        ]
        , div [ class "statisticInfo" , style "display" (if model.getCalInfo == "" then "none" else "flex")] [
         div [][
             text "칼로리 : "
            ,text ( model.getCalInfo ++ " Kcal")
            ]   
            , div [] [
                text "날짜 : "
                , text model.getCalInfoDate
            ]
        ]
        , div [style "display" (if model.getCalInfo == "" then "flex" else "none"), class "graphInfoUsageText"][
            text "그래프를 선택하시면,"
            , br [] []
            , text " 칼로리 기록을 확인 할 수 있습니다."
        ]
    ]

appweightBarChart model = 
    div [class "barChartcontainer", style "padding" "0"][
        table [ id "q-graph" ]
        [ tbody []
            [
                tr [style "width" "100%"](List.map3 (\x y z -> appinnerItem x y model.maxWeight z "" model "weight") model.weightBarChartString model.weightBarChart model.dateList)  ] 
         , tfoot [][
              case model.activeTab of
                    "weekly" ->
                        tr []
                        (List.map dateFooter appDate)
                
                    "monthly" ->
                        tr []
                            (List.indexedMap dateIdxFooter model.dateList)
                    _ ->
                        tr []
                        (List.indexedMap dateIdxFooter model.dateList)
                    , tr [class "graphGrid"] [
                        td [] []
                        ]
                    
                    ]
            ]
            , div [ class "statisticInfo", style "display" (if model.getWeightInfo == "" then "none" else "flex") ] [
             div [][
             text "체중 : "
            ,text (model.getWeightInfo ++ " Kg")
            ]   
            , div [] [
                text "날짜 : "
                , text model.getWeightInfoDate
            ]
        ]      
        , div [style "display" (if model.getWeightInfo == "" then "flex" else "none"), class "graphInfoUsageText"][
            text "그래프를 선택하시면,"
            , br [] []
            , text "체중 기록을 확인 할 수 있습니다."
        ] 
    ]

appexerciseBarChart model = 
    div [class "barChartcontainer", style "padding" "0"][
        table [ id "q-graph" ]
        [ tbody []
            [
                tr [style "width" "100%"](List.map3 (\x y z -> appinnerItem x y model.maxTime z "" model "exercise") model.exerciseChartForm model.exerciseData model.dateList)  ] 
         , tfoot [][
              case model.activeTab of
                    "weekly" ->
                        tr []
                        (List.map dateFooter appDate)
                
                    "monthly" ->
                        tr []
                            (List.indexedMap dateIdxFooter model.dateList)
                    _ ->
                        tr []
                        (List.indexedMap dateIdxFooter model.dateList)
                    , tr [class "graphGrid"] [
                        td [] []
                        ]
            ]
        ]      
        , div [ class "statisticInfo" , style "display" ( if model.getExerciseInfo == "" then "none" else "flex")] [
            div [][
             text "운동 시간 : "
            ,text model.getExerciseInfo
            ]   
        , div [] [
                text "날짜 : "
                , text model.getExerciseInfoDate
            ] 
        ]
        , div [style "display" (if model.getExerciseInfo == "" then "flex" else "none"), class "graphInfoUsageText"][
            text "그래프를 선택하시면,"
            , br [] []
            , text "운동기록을 확인 할 수 있습니다."
        ] 
    ]



appDate = 
    ["일", "월", "화", "수", "목", "금" ,"토"]

exerciseBarChart model = 
    div [class "webbarChartcontainer"][
        div [ id "ticks" ]
        [
            div [ class "tick"]
            [ p []
                [ text model.maxTimeString ]
            ]
            , div [ class "tick"]
            [ p []
                [ text "00:00:00" ]
            ]
        ]
        , table [ id "q-graph" ]
        [ tbody []
            [
                tr [style "width" "100%"](List.map3 (\x y z -> innerItem x y model.maxTime z "") model.exerciseChartForm model.exerciseData model.dateList)  ] 
         , tfoot [][
                tr []
            (List.map dateFooter model.dateList)]
        ]
    ]

listHeaderweight item = 
    case List.head item of
        Just a ->
            case a.weight of
                Just weight ->
                    weight
            
                Nothing ->
                    ""
        Nothing ->
            ""

takeItem idx item = 
    let
        takeResult = List.take idx item
        dropResult = List.drop (idx - 1) takeResult
    in
    listHeader dropResult

dateDecoder model float = 
            case String.fromFloat float of
                "1" ->
                    takeItem (Basics.round float) model.statistics
                "2" ->
                    takeItem (Basics.round float) model.statistics
                "3" ->
                    takeItem (Basics.round float) model.statistics
                "4" ->
                    takeItem (Basics.round float) model.statistics
                "5" ->
                    takeItem (Basics.round float) model.statistics
                "6" ->
                    takeItem (Basics.round float) model.statistics
                "7" ->
                    takeItem (Basics.round float) model.statistics
                _ ->
                    takeItem (Basics.round float) model.statistics

web model = 
        div [ class "container" ]
            [
                commonJustHeader "/image/icon_stats.png" "나의 통계" ,
                div [ class "yf_yfworkout_search_wrap" ]
                [
                    tabBox model,
                    bodyContents model
                ]
            ]

exerciseCase item a b  = 
    case item of
        Just ok ->
            justInt (String.slice a b  ok)
    
        Nothing ->
            0

justInt int = 
    case String.toInt int of
        Just i ->
            i
    
        Nothing ->
            0



app model btn = 
    div [class "container"] [
        appHeaderConfirmDetailR "나의 통계" "myPageHeader" btn btn "확인",
        div [ class "yf_yfworkout_search_wrap" ]
                [
                    apptabBox model,
                    appbodyContents model
                ]
    ]
tabBox model = 
    div [ class "tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large " ]
            [ ul []
                [ li [ classList [
                    ("myStatistical_yf_active", model.activeTab == "weekly")
                ], onClick (ActiveTab "weekly") ]
                    [ 
                         text "주간별" 
                    ]
                , li [ classList [
                    ("myStatistical_yf_active", model.activeTab == "monthly")
                ], onClick (ActiveTab "monthly")]
                    [ 
                         text "월별"
                    ]
                ]
            ]
        ]
apptabBox model = 
    div [ class "m_tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth m_myStatistical_tabs" ]
            [ ul []
                [ li [ classList [
                    ("m_myStatistical_yf_active", model.activeTab == "weekly")
                ], onClick (ActiveTab "weekly") ]
                    [ 
                         text "주간별" 
                    ]
                , li [ classList [
                    ("m_myStatistical_yf_active", model.activeTab == "monthly")
                ], onClick (ActiveTab "monthly")]
                    [ 
                         text "월별"
                    ]
                ]
            ]
        ]
appbodyContents model =
    case model.activeTab of
        "weekly" ->
            appbodyItem model
    
        "monthly" ->
            div [] [
            div [ class "statisticalMonth"] [
                i [ class "fas fa-chevron-left", onClick (ChangeDate "before") ][]
                , div [] [text (String.dropRight 3 model.date)]
                , i [ class "fas fa-chevron-right", onClick (ChangeDate "next")  ]
                []
                ]
            , appbodyItem model
            ]
        _ ->
            appbodyItem model

appbodyItem model = 
    div [ class "myStatistical_searchbox" ]
        [ div [  ]
            [ div [  ]
                [ div [class "appTitleStatic"] [text "체중 기록"]
                   , appweightBarChart model
                ]
            , div [ class "myStatistical_inbox columnStatisc", style "padding" "0" ]
                [ div [class "appTitleStatic"] [text "운동 기록"]
                , appexerciseBarChart model
                ]
            , div [ class "myStatistical_inbox columnStatisc" , style "padding" "0"]
                [ div [class "appTitleStatic"] [text "칼로리 기록"]
                , 
                        appcalBarChart model
                ]
            ]
        ]

bodyContents model =
    case model.activeTab of
        "weekly" ->
            webBodyContentsItem model
    
        "monthly" ->
            div [] [
            div [ class "statisticalMonth"] [
                i [ class "fas fa-chevron-left", onClick (ChangeDate "before") ][]
                , div [] [text (String.dropRight 3 model.date)]
                , i [ class "fas fa-chevron-right", onClick (ChangeDate "next")  ]
                []
                ]
            , webBodyContentsItem model
            ]
        _ ->
            webBodyContentsItem model

webBodyContentsItem model =
    div [ class "myStatistical_searchbox" ]
        [ div [ class "myStatistical_mediabox" ]
            [ div [ class "myStatistical_inbox web_inbox" ]
                [ div [class "staticsTitle"] [text "체중" ]
                    , chart model "체중" "date" model.barChartForm model.hoverWeight
                ]
            , div [ class "myStatistical_inbox web_inbox" ]
                [ div [class "staticsTitle"][text "운동" ]
                , case model.activeTab of
                    "weekly" ->
                        exerciseBarChart model 
                
                    "monthly" ->
                        
                        chart model "운동" "date" model.monthExerLine model.hoverExercise
                    _ ->
                        exerciseBarChart model
                ]
            , div [ class "myStatistical_inbox web_inbox" ]
                [ div [class "staticsTitle"] [text "섭취칼로리" ]
                , case model.activeTab of
                    "weekly" ->
                        calBarChart model 
                    "monthly" ->
                        chart model "칼로리" "date" model.monthCalLine model.hoverCal
                    _ ->
                        calBarChart model
                ]
            ]
        ]
stringToFloat item today = 
    case item of
        Just float ->
            float
    
        Nothing ->
           case String.toFloat today of
                Just td ->
                    td
            
                Nothing ->
                    0

justCase item = 
    case item of
        Just ok ->
            if ok <= 0 then
                1
            else 
                ok
    
        Nothing ->
            1


justString item today = 
    case item of
        Just str ->
            stringToFloat (String.toFloat str) ( today)
    
        Nothing ->
            case String.toFloat today of
                Just td ->
                    td
            
                Nothing ->
                    0
percentage part max  =
    part / max * 100
