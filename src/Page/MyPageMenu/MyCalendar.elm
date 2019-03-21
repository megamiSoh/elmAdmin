module Page.MyPageMenu.MyCalendar exposing(..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Route exposing (..)
import Page.Common exposing(..)
import Api as Api

type alias Model 
    = {
        session : Session
        ,checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = ""
        , check = mobile}
        , P.checkMobile ()
    )
subscriptions : Model -> Sub Msg
subscriptions model =
    P.check CheckDevice

type Msg 
    = CheckDevice E.Value
    | BackBtn

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            (model, Route.backUrl (Session.navKey model.session) 1)


view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
       if model.checkDevice == "pc" then
       web
       else
       app  
    }

web = 
        div [ class "container" ]
            [
                commonJustHeader "/image/icon_calendar.png" "캘린더"
                , div [ class "yf_yfworkout_search_wrap" ]
                    [
                        calendarDate,
                        dietRecords,
                        weightResult,
                        bodyPhoto,
                        bodyInfo,
                        workoutSum,
                        doneWorkOut
                    ]
            ]

app =
    div [class "container"] [
        appHeaderback "캘린더" "myPageHeader" BackBtn,
        appcalendarDate,
        appdietRecords,
        appweightResult,
        appbodyPhoto,
        appbodyInfo,
        appworkoutSum,
        appdoneWorkOut
    ]
calendarDate = 
    div [ class "myCalendar_tapbox" ]
        [ div [ class "myCalendar_datebox" ]
            [ i [ class "fas fa-angle-left myCalendar_yf_left" ]
                [], text "2019.01.01" 
            , i [ class "fas fa-angle-right myCalendar_yf_right" ]
                []
            ]
        ]

appcalendarDate = 
    div [ class "m_myCalendar_tapbox" ]
        [ div [ class "m_myCalendar_datebox" ]
            [ i [ class "fas fa-angle-left m_myCalendar_yf_left" ]
                [], text "2019.01.01" 
            , i [ class "fas fa-angle-right m_myCalendar_yf_right" ]
                []
            ]
        ]


dietRecords = 
    div [ class "myCalendar_ditebox" ]
        [ div [ class "myCalendar_goal" ]
            [ text "목표체중 nn Kg" ]
        , div [ class "myCalendar_food" ]
            [ img [ src "/image/dite_morning.png" ]
                []
            , ul []
                [li[]
                    [ text "아침" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "myCalendar_food" ]
            [ img [ src "/image/dite_lunch.png" ]
                []
            , ul []
                [li[]
                    [ text "점심" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "myCalendar_food" ]
            [ img [ src "/image/dite_dinner.png" ]
                []
            , ul []
                [li[]
                    [ text "저녁" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "myCalendar_food" ]
            [ img [ src "/image/dite_snack.png" ]
                []
            , ul []
                [li[]
                    [ text "간식" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "myCalendar_kcalbox" ]
            [ text "일일 섭취 칼로리 1000 Kcal / 기초대사량 4000 Kcal",   
            div [ class "progress is-medium is-primary yf_progress" ]
                [], text "일일 섭취 칼로리 80%" 
            ]
        ]

appdietRecords = 
    div [ class "myCalendar_ditebox" ]
        [ div [ class "m_myCalendar_goal" ]
            [ text "목표체중 nn Kg" ]
        , div [ class "m_myCalendar_food" ]
            [ img [ src "/image/dite_morning.png" ]
                []
            , ul []
                [li[]
                    [ text "아침" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "m_myCalendar_food" ]
            [ img [ src "/image/dite_lunch.png" ]
                []
            , ul []
                [li[]
                    [ text "점심" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "m_myCalendar_food_a" ]
            [ img [ src "/image/dite_dinner.png" ]
                []
            , ul []
                [li[]
                    [ text "저녁" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "m_myCalendar_food_a" ]
            [ img [ src "/image/dite_snack.png" ]
                []
            , ul []
                [li[]
                    [ text "간식" ]
                ,li[]
                    [ text "1000Kcal" ]
                ]
            ]
        , div [ class "m_myCalendar_kcalbox" ]
            [ text "일일 섭취 칼로리 1000 Kcal / 기초대사량 4000 Kcal",   
            div [ class "progress is-medium is-primary yf_progress" ]
                [], text "일일 섭취 칼로리 80%" 
            ]
        ]

weightResult = 
    div [ class "myCalendar_weightbox" ]
        [ i [ class "fas fa-weight" ]
            [], text "현재 체중 56.8KG" 
        , p[][text "목표 체중까지  3.2KG 남았습니다." ]
        ]

appweightResult = 
    div [ class "m_myCalendar_weightbox" ]
        [ i [ class "fas fa-weight" ]
            [], text "현재 체중 56.8KG" 
        , p[][text "목표 체중까지  3.2KG 남았습니다." ]
        ]

bodyPhoto = 
    div [ class "myCalendar_phototbox" ]
        [ div [ class "myCalendar_boxtitle" ]
            [ text "신체변화기록" ]
        , div [ class "photobtnbox" ]
            [ div [ class "myCalendar_photo1" ]
                [ img [ src "/image/photo.png" ]
                    []
               ,p[] [text"Before 2018-01-01"] 
                ]
            , div [ class "myCalendar_photo1" ]
                [ img [ src "/image/photo.png" ]
                    []
                ,p[] [text "After 2018-01-01"]
                ]
            , div [ class "myCalendar_photobtnbox2" ]
                [ ul []
                    [ li []
                        [ a [ class "button yf_is-dark myCalendar_btn" ]
                            [ text "비포사진 올리기" ]
                        ]
                    , li []
                        [ a [ class "button is-dark yf_is-dark myCalendar_btn" ]
                            [ text "에프터사진 올리기" ]
                        ]
                    ]
                ]
            ]
        ]

appbodyPhoto = 
    div [ class "m_myCalendar_phototbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "신체변화기록" ]
        , div [ class "photobtnbox" ]
            [ div [ class "m_myCalendar_photo" ]
                [ img [ src "/image/photo.png" ]
                    []
               ,p[] [text"Before 2018-01-01"] 
                ]
            , div [ class "m_myCalendar_photo1" ]
                [ img [ src "/image/photo.png" ]
                    []
                ,p[] [text "After 2018-01-01"]
                ]
            
            ]
        ]

bodyInfo = 
    div [ class "myCalendar_inforbox" ]
        [ div [ class "myCalendar_boxtitle" ]
            [ text "신체정보" ]
        , ul []
            [ li [class"myCalendar_infotitle"]
                [ text "체지방율(%)" ]
            , li []
                [ text "nn%" ]
            ]
        , ul []
            [ li [class"myCalendar_infotitle"]
                [ text "기초대사량" ]
            , li []
                [ text "nn Kcal" ]
            ]
        , ul []
            [ li [class"myCalendar_infotitle"]
                [ text "비만도(BMI)" ]
            , li []
                [ text "nn.n (고체중)" ]
            ]
        , ul []
            [ li [class"myCalendar_infotitle"]
                [ text "몸무게 변화" ]
            , li [class"myCalendar_infotitle"]
                [ text "+ 1.2Kg" ]
            ]
        ]

appbodyInfo = 
    div [ class "m_myCalendar_inforbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "신체정보" ]
        , ul []
            [ li [class"m_myCalendar_infotitle"]
                [ text "체지방율(%)" ]
            , li [class"m_myCalendar_infotitle"]
                [ text "nn%" ]
            ]
        , ul []
            [ li [class"m_myCalendar_infotitle"]
                [ text "기초대사량" ]
            , li [class"m_myCalendar_infotitle"]
                [ text "nn Kcal" ]
            ]
        , ul []
            [ li [class"m_myCalendar_infotitle"]
                [ text "비만도(BMI)" ]
            , li [class"m_myCalendar_infotitle"]
                [ text "nn.n (고체중)" ]
            ]
        , ul [class"m_myCalendar_infotitle"]
            [ li [class"m_myCalendar_infotitle"]
                [ text "몸무게 변화" ]
            , li [class"m_myCalendar_infotitle"]
                [ text "+ 1.2Kg" ]
            ]
        ]

workoutSum =
        div [ class "myCalendar_timetbox" ]
            [ div [ class "boxtitle" ]
                [ text "총운동시간" ]
            , i [ class "fas fa-clock" ]
                [], text "nn분" 
            ]

appworkoutSum =
        div [ class "m_myCalendar_timetbox" ]
            [ div [ class "m_myCalendar_boxtitle" ]
                [ text "총운동시간" ]
            , i [ class "fas fa-clock m_myCalendar_infotitle" ]
              [], text "nn분"
            ]

doneWorkOut = 
    div [ class "myCalendar_finshbox" ]
        [ div [ class "boxtitle" ]
            [ text "완료한 운동목록" ]
        , div [ class "workbox" ]
            [ div [ class "myCalendar_iconbox" ]
                [ img [ src "/image/m_workicon.png" ]
                    []
                ]
            , div [ class "myCalendar_textbox" ]
                [ ul []
                    [ li [ class "work1" ]
                        [ text "와이드 레그드 포워드 밴드 위드 핸즈 온 힙" ]
                    , li [ class "work2" ]
                        [ text "엉덩이 -하 -바벨 -요가" ]
                    , li [ class "work3" ]
                        [ text "30초" ]
                    ]
                ]
            ]
        , div [ class "myCalendar_workbox" ]
            [ div [ class "myCalendar_iconbox" ]
                [ img [ src "/image/m_workicon.png" ]
                    []
                ]
            , div [ class "myCalendar_textbox" ]
                [ ul []
                    [ li [ class "work1" ]
                        [ text "와이드 레그드 포워드 밴드 위드 핸즈 온 힙" ]
                    , li [ class "work2" ]
                        [ text "엉덩이 -하 -바벨 -요가" ]
                    , li [ class "work3" ]
                        [ text "30초" ]
                    ]
                ]
            ]
        , div [ class "myCalendar_addbtn" ]
            [ a [ class "button is-dark" ]
                [ text "더보기" ]
            ]
        ]

appdoneWorkOut = 
    div [ class "m_myCalendar_finshbox" ]
        [ div [ class "m_myCalendar_boxtitle" ]
            [ text "완료한 운동목록" ]

        , div [ class "m_myCalendar_workbox" ]
            [ div [ class "m_myCalendar_iconbox" ]
                [ img [ src "/image/m_workicon.png" ]
                    []
                ]
            , div [ class "m_myCalendar_textbox" ]
                [ ul []
                    [ li [ class "m_work1" ]
                        [ text "와이드 레그드 포워드 밴드 위드 핸즈 온 힙" ]
                    , li [ class "m_work2" ]
                        [ text "엉덩이 -하 -바벨 -요가" ]
                    , li [ class "m_work3" ]
                        [ text "30초" ]
                    ]
                ]
            ]
        , div [ class "m_myCalendar_workbox" ]
            [ div [ class "m_myCalendar_iconbox" ]
                [ img [ src "/image/m_workicon.png" ]
                    []
                ]
            , div [ class "m_myCalendar_textbox" ]
                [ ul []
                    [ li [ class "m_work1" ]
                        [ text "와이드 레그드 포워드 밴드 위드 핸즈 온 힙" ]
                    , li [ class "m_work2" ]
                        [ text "엉덩이 -하 -바벨 -요가" ]
                    , li [ class "m_work3" ]
                        [ text "30초" ]
                    ]
                ]
            ]
        , div [ class "m_myCalendar_addbtn" ]
            [ a [ class "button is-dark m_myCalendar_addbtnbtn" ]
                [ text "더보기" ]
            ]
        ]