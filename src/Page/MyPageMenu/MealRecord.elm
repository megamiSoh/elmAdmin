module Page.MyPageMenu.MealRecord exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Api as Api


type alias Model 
    = {
        session : Session,
        activeBtn : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        , activeBtn = "breakfast"
        , check = mobile}
        , Cmd.none
    )

type Msg 
    = ActiveTab String

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ActiveTab item ->
            ( {model | activeBtn = item}, Cmd.none )

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
        div [ class "container" ]
            [
                -- contentsHere
                tabMenu model,
                searchBox,
                registMeal,
                saveBtn


            ]
    }

 
tabMenu model =
    div [ class "mealRecord_tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large" ]
            [ ul []
                [ li [ classList [
                        (" mealRecord_yf_active", model.activeBtn == "breakfast")
                    ], onClick (ActiveTab "breakfast")]
                    [ 
                        text "아침식단" 
                    ]
                , li [ classList [
                        ("mealRecord_yf_active", model.activeBtn == "lunch")
                    ], onClick (ActiveTab "lunch")]
                    [  
                        text "점심식단" 
                    ]
                , li [classList [
                        ("mealRecord_yf_active", model.activeBtn == "dinner")
                    ], onClick (ActiveTab "dinner")]
                    [  
                        text "저녁식단" 
                    ]
                , li [classList [
                        ("mealRecord_yf_active", model.activeBtn == "snack")
                    ], onClick (ActiveTab "snack")]
                    [ 
                        text "간식식단" 
                    ]
                ]
            ]
        ]

searchBox = 
    div [ class "mealRecord_searchbox" ]
        [ div [ class "field mealRecord_yf_field" ]
            [ input [ class "input yf_food_input", type_ "text", placeholder "음식을 검색하세요" ]
                []
            , div [ class "control yf_food_con" ]
                [ a [ class "button is-info yf_food_searchwindow" ]
                    [ text "검색" ]
                ]
            , a [ class "button yf_food_searchwindow", href "yf_mypage_dite2.html" ]
                [ text "음식 칼로리 직접입력" ]
            ]
            , searchResult
            , pagenation
        ]

searchResult =
    div [ class "mealRecord_searchbox2" ]
        [ table [ class "table mealRecord_yf_table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "번호" ]
                    , th []
                        [ text "이름" ]
                    , th []
                        [ text "칼로리" ]
                    ]
                ]
            , tbody []
                (
                    List.indexedMap mealLayout mealData
                )
            ]
        ]

mealLayout idx item=
    tr [] [
        td[][ text (String.fromInt (idx + 1)) ],
        td[][ text item.name ],
        td[][ text item.kcal ]
    ]
pagenation = 
    div [ class "yf_Pagination" ]
        [ nav [ class "pagination is-centered"]
            [ ul [ class "pagination-list" ]
                [ li [ class "" ]
                    [ a [ class "pagination-link" ]
                        [ text "<",  text "<" ]
                    ]
                , a [ class "pagination-link" ]
                    [ text "<" ]
                , li []
                    [ a [ class "pagination-link is-current yf_cut" ]
                        [ text "5" ]
                    ]
                , li []
                    [ a [ class "pagination-link"]
                        [ text ">" ]
                    ]
                , a [ class "pagination-link"]
                    [ text ">>" ]
                ]
            ]
        ]

registMeal =
    div [ class "mealRecord_searchbox2" ]
        [ table [ class "table mealRecord_yf_table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "번호" ]
                    , th []
                        [ text "이름" ]
                    , th []
                        [ text "갯수" ]
                    , th []
                        [ text "칼로리" ]
                    , th []
                        [ text "제거" ]
                    ]
                ]
            , tfoot []
                [ tr []
                    [ th [colspan 3]
                        [ strong []
                            [ text "칼로리 총 합계" ]
                        ]
                    , th [colspan 2]
                        [ text "600 Kacl" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ th []
                        [ text "1" ]
                    , td []
                        [ text "호박고구마" ]
                    , td []
                        [ text "1" ]
                    , td []
                        [ text "100Kcal" ]
                    , td []
                        [ i [ class "far fa-trash-alt" ]
                            []
                        ]
                    ]
                , tr []
                    [ th []
                        [ text "2" ]
                    , td []
                        [ text "고구마피자" ]
                    , td []
                        [ text "2" ]
                    , td []
                        [ text "200Kcal" ]
                    , td []
                        [ i [ class "far fa-trash-alt" ]
                            []
                        ]
                    ]
                , tr []
                    [ th []
                        [ text "3" ]
                    , td []
                        [ text "고구마과자" ]
                    , td []
                        [ text "3" ]
                    , td []
                        [ text "300Kcal" ]
                    , td []
                        [ i [ class "far fa-trash-alt" ]
                            []
                        ]
                    ]
                ]
            ]
        ]

saveBtn =
    div [ class " yf_dark" ]
        [ a [ class "button is-dark", href "yf_mypage_calendar.html" ]
            [ text "저장" ]
        ]

mealData = 
    [
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"}
    ]
 