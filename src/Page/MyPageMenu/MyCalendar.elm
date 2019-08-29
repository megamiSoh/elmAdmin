module Page.MyPageMenu.MyCalendar exposing(..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes  as Attr exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Json.Encode as E
import Json.Decode as Decode
import Route exposing (..)
import Page.Common exposing(..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import File as Files
import Date exposing (Date, Interval(..), Unit(..), add, fromCalendarDate)
import Task exposing (Task)
import Time exposing(Month(..))
import Page.MyPageMenu.MyPageInfoLayout exposing (..)


type alias Model = 
    { session : Session
    , checkDevice : String
    , check : Bool
    , data : MealData
    , date : String
    , beforeImg : List Files.File
    , afterImg : List Files.File
    , currentDay : Date
    , today : String
    , exerPage : Int
    , exerPer_page : Int
    , completeExerciseList : List ExerciseList
    , showMenu : Bool
    , currentPage : String
    , preview : List String
    , errType : String
    , beforeOrAfter : String
    }

type alias Meal = 
    { data : MealData }

type alias MealData = 
    { body : Body
    , date_exercise : String
    , date_kcal : String
    , kcal : List Kcal
    , photo : Photo }

type alias Body = 
    { age : Int
    , bmi : Bmi
    , bmr : Float
    , body_fat_percentage : Float
    , change_weight: String
    , goal_weight: String
    , height: String
    , is_male : Maybe Bool
    , remain_weight : String
    , weight: String }

type alias Bmi = 
    { division : String
    , value : Float }

type alias Kcal = 
    { food_code : String
    , kcal : String }

type alias Photo = 
    { after : Maybe String
    , before : Maybe String }

type alias ImgData = 
    { data : BodyImg }

type alias BodyImg = 
    { content_length : Int
    , content_type : String
    , extension : String
    , name : String
    , origin_name : String
    , path : String }

type alias CompleteExerciseList = 
    { data : List ExerciseList 
    , paginate : ExercisePaginate }

type alias ExerciseList = 
    { exercise_no : Int
    , exercise_id : Int
    , mediaid : String
    , thembnail : String
    , title : String }

type alias ExercisePaginate = 
    { date : String
    , exercise_date : String
    , page : Int
    , per_page : Int
    , total_count : Int
    , user_id : Int }

diaryApi date session = 
    Api.get GetData (Endpoint.diary date) (Session.cred session) (Decoder.diaryData Meal MealData Body Kcal Photo Bmi)

imgEncoder img date session whatKindOf endpoint= 
    let
        body = (List.map (Http.filePart whatKindOf)img)
            |> Http.multipartBody 
    in
    Api.post (endpoint date) (Session.cred session) ImgUploadComplete body (Decoder.myBodyImg ImgData BodyImg)

exerciseCompleteList page per_page date session = 
    let
        body = 
            E.object
                [ ("page", E.int page)
                , ("per_page", E.int per_page)]
                    |> Http.jsonBody
    in
    Api.post (Endpoint.exerciseCompleteList date) (Session.cred session) GetExerciseCompleteList body (Decoder.exerciseCompleteList CompleteExerciseList ExerciseList ExercisePaginate)
    

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , date = ""
        , beforeImg = []
        , afterImg = []
        , exerPage = 1
        , exerPer_page = 10
        , showMenu = False
        , preview = []
        , currentPage = ""
        , data = 
            { body = 
                { age = 0
                , bmi = 
                    { division = ""
                    , value = 0 }
                , bmr = 0
                , body_fat_percentage = 0
                , change_weight = ""
                , goal_weight = ""
                , height = ""
                , is_male = Nothing
                , remain_weight = ""
                , weight = ""
                }
            , date_exercise = ""
            , date_kcal = ""
            , kcal = []
            , photo = 
                { after = Nothing
                , before = Nothing }
            }
        , currentDay = Date.fromCalendarDate 2019 Jan 1
        , today = ""
        , completeExerciseList = []
        , errType = ""
        , beforeOrAfter = ""
        }
        , Cmd.batch[Date.today |> Task.perform ReceiveDate
        , scrollToTop NoOp
        , Api.mypageMenu (E.bool False)]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Api.successSave SaveKey
    , Session.changes GotSession (Session.navKey model.session)]

type Msg 
    = BackBtn
    | GetData (Result Http.Error Meal)
    | ImgUploadComplete (Result Http.Error ImgData)
    | ImgUploadBefore (List Files.File)
    | ImgUploadAfter (List Files.File)
    | GoMealRecord String
    | SaveKey E.Value
    | ReceiveDate Date
    | ChangeDate String
    | GetExerciseCompleteList (Result Http.Error CompleteExerciseList)
    | GotSession Session
    | NoOp
    | ClickRight
    | ClickLeft
    | GoAnotherPage
    | ShowMenu
    | GotPreviews (List String)
    | ChangePage String
    | ChangeProfile 

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

justToint int = 
    case String.toInt int of
        Just ok ->
            ok
    
        Nothing ->
            0
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ({model | currentPage = page}, Cmd.none)
        ChangeProfile ->
            (model, Cmd.none)
        GotPreviews urls ->
            ({model | preview = urls}, Cmd.none)
        ShowMenu ->
            ({model | showMenu = not model.showMenu}, Cmd.none)
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (E.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        NoOp -> 
            (model, Cmd.none)
        GotSession session ->
            ({model | session = session}, 
            case model.errType of
                "getExer" ->
                    exerciseCompleteList model.exerPage model.exerPer_page model.today session
                "imgupload" ->
                    case model.beforeOrAfter of
                        "after" ->
                            imgEncoder model.afterImg model.date session "after" Endpoint.afterImg
                        "before" ->
                            imgEncoder model.afterImg model.date session "before" Endpoint.afterImg
                        _ ->
                            Cmd.none
                "getData" ->
                    diaryApi model.date session
                _ ->
                    diaryApi model.date session)
        GetExerciseCompleteList (Ok ok) ->
            ({model | completeExerciseList = ok.data}, Cmd.none)
        GetExerciseCompleteList (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "getExer"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        ChangeDate when ->
            let
                formatDate day =
                    Date.add Date.Days day model.currentDay

                formatDateString day = 
                    getFormattedDate Nothing (Just (formatDate day))
            in
            
            case when of
                "next" ->
                    let
                        date = String.dropLeft 8 model.date
                        today = String.dropLeft 8 model.today
                    in
                    ({model | date = formatDateString 1, currentDay = formatDate 1}, 
                    Cmd.batch[diaryApi (formatDateString 1) model.session
                    , exerciseCompleteList model.exerPage model.exerPer_page (formatDateString 1) model.session
                    , 
                    if model.check then
                    Cmd.none
                    else
                    Api.valueReset (E.string (formatDateString 1))]
                    )
                "before" ->
                    ({model | date = formatDateString -1, currentDay = formatDate -1}, 
                    Cmd.batch [
                        diaryApi (formatDateString -1) model.session
                    , exerciseCompleteList model.exerPage model.exerPer_page (formatDateString -1) model.session
                    , if model.check then
                    Cmd.none
                    else
                    Api.valueReset (E.string (formatDateString -1))
                    ]
                    )
                _ ->
                    (model, Cmd.none)
                    
        ReceiveDate date ->
            let
                dateString = getFormattedDate Nothing (Just date)
            in
            
            ({model | date = getFormattedDate Nothing (Just date), currentDay = date, today = dateString}, 
            Cmd.batch[diaryApi dateString model.session
            ,exerciseCompleteList model.exerPage model.exerPer_page dateString model.session]
            )
        SaveKey success ->
            if model.check then
            (model, Route.pushUrl (Session.navKey model.session) Route.MealRM)
            else
            (model, Route.pushUrl (Session.navKey model.session )Route.MealR)
        GoMealRecord code ->
            
            (model,Api.saveKey (E.string (code ++ "," ++model.date)))
        ImgUploadAfter img ->
            ({model | afterImg = img , beforeOrAfter = "after"}, imgEncoder img model.date model.session "after" Endpoint.afterImg)
        ImgUploadBefore img ->
            ({model | beforeImg = img, beforeOrAfter = "before"}, imgEncoder img model.date model.session "before" Endpoint.beforeImg) 
        ImgUploadComplete (Ok ok) ->
            (model, Cmd.batch[Api.showToast (E.string "등록 되었습니다."), diaryApi model.date model.session]) 
        ImgUploadComplete (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "imgupload"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        GetData (Ok ok)->
            ({model | data = ok.data}, Cmd.none)
        GetData (Err err)->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "getData" }, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        BackBtn ->
            (model, Route.backUrl (Session.navKey model.session) 1)


view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
    { title = "YourFitExer"
    , content =
       div [] [
           app model
        --    , div [] (List.map (\x -> previewLayout x model) model.preview )
       ]
    }
    else
    { title = "YourFitExer"
    , content =
       div [] [
           div [class "mypageHiddenMenu", onClick ShowMenu] [
               div [][]
               , div [][]
           ]
            , div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage model.showMenu]
            , web model
       ]
    }
previewLayout item model = 
    div [class ("control has-icons-right myaccountStyle "  ++ (if model.currentPage == "image" then "calendarImg" else "")), id "calendarImg"] [
        ul [class "accountHeader"] 
                [ li[onClick (ChangePage "")]
                    [ span [class "fas fa-times"][] ]
                , li[][text "프로필 사진"]
                , li[onClick ChangeProfile][
                   text "확인"
                ]
                ],
        img [src item] []
    ]
web model= 
        div [ class "container" ]
            [
                commonJustHeader "/image/icon_calendar.png" "캘린더"
                , div [ class "yf_yfworkout_search_wrap" ]
                    [
                        calendarDate model,
                        dietRecords model,
                        weightResult model,
                        bodyPhoto model,
                        bodyInfo model ,
                        workoutSum model,
                        doneWorkOut model
                    ]
            ]

app model =
    div [class ("container topSearch_container " ++ if model.currentPage == "image" then "fadeContainer" else "")] [
        appHeaderRDetail "캘린더" "myPageHeader  whiteColor" Route.MyPage "fas fa-angle-left",
        appcalendarDate model,
        appdietRecords model,
        appweightResult model ,
        appbodyPhoto model ,
        appbodyInfo model,
        appworkoutSum model,
        appdoneWorkOut model
    ]
calendarDate model = 
    div [ class "myCalendar_tapbox" ]
        [ div [ class "myCalendar_datebox" ]
            [ i [ class "fas fa-angle-left myCalendar_yf_left", onClick (ChangeDate "before") ]
                [], 
                if model.date == model.today then 
                div [class "date_container"] [text model.date, span [class"today"] [text "today"] ]
                else 
                div [class "date_container"] [text model.date, span [class"today"] [] ]
            , 
                if model.date == model.today then
                i [ class "fas fa-angle-right myCalendar_yf_right", style "color" "#d2caca"]
                    []
                else
                i [ class "fas fa-angle-right myCalendar_yf_right", onClick (ChangeDate "next") , style "color" "#000" ]
                    []
            ]
        ]

appcalendarDate model  = 
    div [ class "m_myCalendar_tapbox" ]
        [ div [ class "m_myCalendar_datebox" ]
            [ i [ class "fas fa-angle-left m_myCalendar_yf_left", onClick (ChangeDate "before") ]
                [],if model.date == model.today then 
                div [class "date_container"] [text model.date, span [class"today"] [text "today"] ]
                else 
                div [class "date_container"] [text model.date, span [class"today"] [] ]
            , if model.date == model.today then
            i [ class "fas fa-angle-right m_myCalendar_yf_right", style "color" "#d2caca" ]
                []
            else
            i [ class "fas fa-angle-right m_myCalendar_yf_right", onClick (ChangeDate "next") , style "color" "#000"  ]
                []
            ]
        ]
foodKcalLayout foodImg time kcal style code =
    div [ class (style ++ " cursor"), onClick (GoMealRecord code )]
            [ img [ src foodImg ] []
            , ul [class"daytime"]
                [ li[]
                    [ text time ]
                , li[]
                    [ 
                    if Basics.round (stringToFloat kcal) == 0 then
                        span [class "RecordMeal"] [text "식단기록"]
                    else
                        span [class "RecordMeal"] [text kcal
                        , div [class"kcal_text"] [text ("단위 / Kcal")]]
                    ]
                ]
            ]
foodKcal item class = 
    case item.food_code of
        "10" ->
            foodKcalLayout "/image/dite_morning.png" "아침" item.kcal class "10"
        "20" ->
            foodKcalLayout "/image/dite_lunch.png" "점심" item.kcal class "20"
        "30" ->
            foodKcalLayout "/image/dite_dinner.png" "저녁" item.kcal class "30"
        "40" ->
            foodKcalLayout "/image/dite_snack.png" "간식" item.kcal class "40"
        _ ->
            foodKcalLayout "" "" "" "" ""

stringToFloat item = 
    case String.toFloat item of
        Just ok ->
            ok
    
        Nothing ->
            0

roundPercent model = 
    Basics.round((stringToFloat model.data.date_kcal) / model.data.body.bmr * 100)

dietRecords model = 
    div [ class "myCalendar_ditebox" ]
        [ div [ class "myCalendar_goal" ]
            [ text ("목표체중" ++ model.data.body.goal_weight ++ "Kg") ]
        , div [class "myCalendar_food_container"] (List.map (\x -> foodKcal x "myCalendar_food") model.data.kcal)
        , div [ class "myCalendar_kcalbox" ]
            [ text ("총 섭취 칼로리 "++ model.data.date_kcal ++ " Kcal / 기초대사량 "++ (if model.data.body.bmr < 0 then "0" else String.fromFloat model.data.body.bmr) ++  " Kcal"), 
            if roundPercent model >= 100 then
                progress [ class "progress is-medium is-danger yf_progress", value model.data.date_kcal , Attr.max (String.fromInt(Basics.round model.data.body.bmr))]
                []
            else
                progress [ class "progress is-medium is-primary yf_progress", value model.data.date_kcal , Attr.max (String.fromInt(Basics.round model.data.body.bmr))]
                []
            , if roundPercent model == 0 then
                div [class"put_menu"] [text "식단을 기록 해 주세요."]
            else
                div [class"in_menu"] [
                 text ("일일 섭취 칼로리 " ++ String.fromInt(Basics.round((stringToFloat model.data.date_kcal) / model.data.body.bmr * 100)) ++ " %") 
            ]
            ]
        ]

appdietRecords model  = 
    div [ class "myCalendar_ditebox" ]
        [ div [ class "m_myCalendar_goal" ]
            [ text ("목표체중" ++ model.data.body.goal_weight ++ "Kg") ]
        , div [class "m_myCalendar_food_container"] (List.map (\x -> foodKcal x "m_myCalendar_food")model.data.kcal)
        , div [ class "m_myCalendar_kcalbox" ]
            [ text ("일일 섭취 칼로리 " ++ model.data.date_kcal ++ "Kcal ")
            , div [] [
                text (" 기초대사량 "++  (if model.data.body.bmr < 0 then "0" else String.fromFloat model.data.body.bmr) ++  " Kcal")
            ]
            ,if roundPercent model >= 100 then
                progress [ class "progress is-medium is-danger yf_progress", value model.data.date_kcal , Attr.max (String.fromInt(Basics.round model.data.body.bmr))]
                []
            else
                progress [ class "progress is-medium is-primary yf_progress", value model.data.date_kcal , Attr.max (String.fromInt(Basics.round model.data.body.bmr))]
                []
                , if roundPercent model == 0 then
                div [] [text "식단을 기록 해 주세요."]
                else
                div [] [
                 text ("일일 섭취 칼로리 " ++ String.fromInt(Basics.round((stringToFloat model.data.date_kcal) / model.data.body.bmr * 100)) ++ " %") 
            ]
            ]
        ]

weightResult model = 
    div [ class "myCalendar_weightbox" ]
        [ i [ class "fas fa-weight" ]
            [], text (" 현재 체중 "++ model.data.body.weight ++"KG") 
        , p[class"gloa_weight"][text ("목표 체중까지 " ++ String.fromFloat(Basics.negate (stringToFloat model.data.body.remain_weight)) ++ "KG 남았습니다.") ]
        ]

appweightResult model = 
    div [ class "m_myCalendar_weightbox" ]
        [ i [ class "fas fa-weight" ]
            [], text ("현재 체중 "++ model.data.body.weight ++"KG")
        , p[][text ("목표 체중까지 " ++ model.data.body.remain_weight ++ "KG 남았습니다.") ]
        ]

bodyPhoto model = 
    div [ class "myCalendar_phototbox" ]
        [ div [ class "boxtitle" ]
            [ text "신체변화기록" ]
        , div [ class "photobtnbox" ]
            [ div [ class "myCalendar_photo1" ]
                [ img [ src (
                    case model.data.photo.before of
                        Just before ->
                            before
                    
                        Nothing ->
                            "/image/photo.png"
                ) ]
                    []
               ,p[] [text "Before"] 
                ]
            , div [ class "myCalendar_photo1" ]
                [ img [ src (
                    case model.data.photo.after of
                        Just after ->
                            after
                    
                        Nothing ->
                            "/image/photo.png"
                ) ]
                    []
                ,p[] [text "After"]
                ]
            , div [ class "myCalendar_photobtnbox2" ]
                [ ul []
                    [ li []
                        [ label [] [
                            div [ class "button yf_is-dark myCalendar_btn" ]
                            [ text "비포사진 올리기" ]
                        , input [ type_ "file", style "display" "none" , id (model.date ++ "before"),  on "change" (Decode.map ImgUploadBefore targetFiles)] []
                        ]
                        ]
                    , li []
                        [ label [] [
                            div [ class "button is-dark yf_is-dark myCalendar_btn" ]
                            [ text "에프터사진 올리기" ]
                            , input [ type_ "file", style "display" "none", id (model.date ++ "after"),  on "change" (Decode.map ImgUploadAfter targetFiles)] []
                        ]
                        ]
                    ]
                ]
            ]
        ]

appbodyPhoto model = 
    div [ class "m_myCalendar_phototbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "신체변화기록" ]
        , div [ class "photobtnbox" ]
            [ label [ class "m_myCalendar_photo" ]
                [ img [ src (
                    case model.data.photo.before of
                        Just before ->
                            before
                    
                        Nothing ->
                            "/image/photo.png"
                ) ]
                    []
                    ,input [ class "appFile", type_ "file", id (model.date ++ "before"), accept "image/*" , style "display" "none"
                            , on "change" (Decode.map ImgUploadBefore targetFiles)  
                            ]
                                []
                ]
                
            , label [ class "m_myCalendar_photo1" ]
                [ img [ src (
                    case model.data.photo.after of
                        Just after ->
                            after
                    
                        Nothing ->
                            "/image/photo.png"
                ) ]
                    []
                  , input [ class "appFile", type_ "file", id (model.date ++ "after"), accept "image/*" , style "display" "none"
                            , on "change" (Decode.map ImgUploadAfter targetFiles)  
                            ]
                                []
                ]
            ]
            , div [class "photoText_container"] [
                p[] [text"Before"] 
                ,p[] [text "After"]
            ]
        ]

bodyInfo model = 
    if model.data.body.age <= 0 then
    div [ class "myCalendar_inforbox" ]
       
        [ div [ class "boxtitle" ]
            [ text "신체정보" ]

         , div [class"bodyInfo_boxs"][   
         ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "체지방율(%)" ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "기초대사량" ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "비만도(BMI)" ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "몸무게 변화" ]
            ]
        ]
        , div [] [
            text "신체 정보를 등록 해 주세요."
        ]
        ]
    else
    div [ class "myCalendar_inforbox" ]
       
        [ div [ class "boxtitle" ]
            [ text "신체정보" ]

         , div [class"bodyInfo_boxs"][   
         ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "체지방율(%)" ]
            , li [class"bodyInfo_li"]
                [ text (String.fromFloat model.data.body.body_fat_percentage ++ " %") ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "기초대사량" ]
            , li [class"bodyInfo_li"]
                [ text (String.fromFloat model.data.body.bmr ++ " Kcal") ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "비만도(BMI)" ]
            , li [class"bodyInfo_li"]
                [ text (String.fromFloat model.data.body.bmi.value ++ " ( " ++ model.data.body.bmi.division ++ " )") ]
            ]
        , ul [class"bodyInfo_ul"]
            [ li [class"myCalendar_infotitle"]
                [ text "몸무게 변화" ]
            , li [class""]
                [ 
                if changeWeight model < 0 then
                p[class"bodyInfo_li"][text (String.fromFloat(changeWeight model) ++ "Kg ")
                , i [ class "fas fa-caret-down" , style "font-size" "1.2em", style "color" "green"][]
                ]
                else if changeWeight model == 0 then
                p[class"bodyInfo_li"][text (String.fromFloat(changeWeight model) ++ "Kg ")
                , i [ class "fas fa-minus" , style "font-size" "1.2rem", style "color" "skyblue"] []
                ]
                else
                p[class"bodyInfo_li"][text (String.fromFloat(changeWeight model) ++ "Kg " )
                , i [ class "fas fa-caret-up" , style "font-size" "1.2rem", style "color" "red"] []
                ]
                
                ]
            ]
        ]
        ]

changeWeight model = 
    stringToFloat model.data.body.change_weight 
    
appbodyInfo model = 
    div [ class "m_myCalendar_inforbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "신체정보" ]

        , div [class"m_box_w"][   
         ul [class"m_boxs"]
            [ li [class"m_myCalendar_infotitle"]
                [ text "체지방율(%)" ]
            , li [class"m_myCalendar_infotitle"]
                [ text (String.fromFloat model.data.body.body_fat_percentage ++ " %") ]
            ]
        , ul [class"m_boxs"]
            [ li [class"m_myCalendar_infotitle"]
                [ text "기초대사량" ]
            , li [class"m_myCalendar_infotitle"]
                [ text (String.fromFloat model.data.body.bmr ++ " Kcal") ]
            ]
        , ul [class"m_boxs"]
            [ li [class"m_myCalendar_infotitle"]
                [ text "비만도(BMI)" ]
            , li [class"m_myCalendar_infotitle"]
                [ text (String.fromFloat model.data.body.bmi.value ++ " (" ++ model.data.body.bmi.division ++ ")") ]
            ]
        , ul [class"m_boxs"]
            [ li [class"m_myCalendar_infotitle"]
                [ text "몸무게 변화" ]
            , li [class"m_myCalendar_infotitle"]
                [ text (String.fromFloat(changeWeight model) ++ "Kg ") ]
            ]
        ]
        ]


workoutSum model =
        div [ class "myCalendar_timetbox" ]
            [ div [ class "boxtitle" ]
                [ text "총운동시간" ]
            , i [ class "fas fa-clock boxtitle_clock" ]
                [], text ( model.data.date_exercise ++ "") 
            ]

appworkoutSum model =
        div [ class "m_myCalendar_timetbox" ]
            [ div [ class "m_myCalendar_boxtitle" ]
                [ text "총운동시간" ]
            , i [ class "fas fa-clock m_myCalendar_infotitle" ]
              [], text ( model.data.date_exercise ++ "")
            ]

doneWorkOutItem item = 
    div [class"myCalendar_container_doneWork"] [
        div [ class "myCalendar_iconbox" ]
                [ img [ src item.thembnail ]
                    []
                ]
            , div [ class "myCalendar_textbox" ]
                [ text item.title ]
    ]

doneWorkOut model = 
    div [ class "myCalendar_finshbox" ]
        [ div [ class "boxtitle" ]
            [ text "완료한 운동목록" ]
        , div [style "display" ( if List.isEmpty model.completeExerciseList then "none" else "block")] [
         div [ class "myCalendar_workbox" ]
            (List.map doneWorkOutItem model.completeExerciseList)
        -- , div [ class "myCalendar_addbtn" ]
        --     [ a [ class "button is-dark" ]
        --         [ text "더보기" ]
        --     ]
        ]
        , div [style "display" (if List.isEmpty model.completeExerciseList then "block" else "none"), class "noExerciseResult"] [
                text "완료한 운동이 없습니다."
            ]
        ]

appdoneWorkOut model = 
    div [ class "m_myCalendar_finshbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "완료한 운동목록" ]
            , div [style "display" ( if List.isEmpty model.completeExerciseList then "none" else "block")] [
         div [ class "m_myCalendar_workbox" ]
            (List.map appdoneWorkOutItem model.completeExerciseList)
        -- , div [ class "m_myCalendar_addbtn" ]
        --     [ a [ class "button is-dark m_myCalendar_addbtnbtn" ]
        --         [ text "더보기" ]
        --     ]
            ]
            , div [style "display" (if List.isEmpty model.completeExerciseList then "block" else "none"), class "noExerciseResult"] [
                text "완료한 운동이 없습니다."
            ]
        ]

appdoneWorkOutItem item = 
    div [class "appdoneWorkOut_container"] [
         div [ class "m_myCalendar_iconbox" ]
            [ img [ src item.thembnail ]
                []
            ]
        , div [ class "m_myCalendar_textbox" ]
            [ text item.title ]
    ]

-- changePhoto = 
--     div [class ("noShowToast " ++ model.showbottomtoast)] [
--                     ul [] [
--                         li [onClick ResetProfileImg] [
--                             text "기본 이미지로 변경"
--                         ]
--                         , li [] [
--                             label [] [
--                             text "앨범에서 사진 선택"
--                             , div [] [
--                                 input [ class "appFile", type_ "file", multiple False, id "thumbFile", accept "image/*" 
--                             , on "change" (Decode.map ChangeProfileImage targetFiles)  
--                             ]
--                                 []
--                             ]
--                             ]
--                         ]
--                         , li [class "toastCancel", onClick (ChangeProfileImg False)] [
--                             text "취소"
--                         ]
--                     ]
--                 ]