module Page.MyPageMenu.MealRecordM exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)
import Page.Common exposing(..)
import Page.MyPageMenu.MealLayout exposing(..)
import Api as Api
type alias Model 
    = {
        session : Session
        , checkDevice : String
        , stepDisplay : String
        , whatKindOfMeal : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = ""
        ,stepDisplay = "firstStep"
        , whatKindOfMeal = ""
        , check = mobile}
        , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.none

type Msg 
    = CheckDevice E.Value
    | BackBtn
    | Step (String, String)
    | Meal String

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
                        ({model | checkDevice = "pc"}, Cmd.none)
        BackBtn ->
            (model , Route.backUrl (Session.navKey model.session) 1)
        Step (step, meal)->
            ({model | stepDisplay = step,  whatKindOfMeal = meal}, Cmd.none)
        Meal str ->
            ({model | whatKindOfMeal = str}, Cmd.none)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        div[][allStep model]
    }


allStep model =
    div [class "container"] [
        div [classList [
        ("show", model.stepDisplay == "firstStep"),
        ("none", True)
        ]] [
            appHeaderback "식단기록" "myPageHeader" BackBtn
            , whatKindOfMeal 
        ],
        div [classList [
        ("show", model.stepDisplay == "secStep"),
        ("none", True)
        ]] [
        appHeaderback (model.whatKindOfMeal ++ "등록") "myPageHeader" BackBtn,
        searchInput model (Step("thirdStep", model.whatKindOfMeal)),
        searchResult model mealData (Step ("fourthStep", model.whatKindOfMeal))
         , selectedFood model "n개의 음식" "selectedFood" (Step("fifthStep", model.whatKindOfMeal))
        ],
        div [classList [
        ("show", model.stepDisplay == "thirdStep"),
        ("none", True)
        ]] [
        appHeaderConfirmDetailR "식단직접등록" "myPageHeader"  (Step("secStep", model.whatKindOfMeal))  (Step("secStep", model.whatKindOfMeal)) "확인",
        directRegist model (Step  ("fifthStep", model.whatKindOfMeal ))
        ],
        div [classList [
        ("show", model.stepDisplay == "fourthStep"),
        ("none", True)
        ]] [
        appHeaderConfirmDetailR (model.whatKindOfMeal ++ "수량") "myPageHeader"  (Step("secStep", model.whatKindOfMeal))  (Step("fifthStep", model.whatKindOfMeal)) "확인",
        quantity
        ],
        div [classList [
        ("show", model.stepDisplay == "fifthStep"),
        ("none", True)
        ]] [
        appHeaderConfirmDetailR model.whatKindOfMeal "myPageHeader"  (Step("secStep", model.whatKindOfMeal))  (Step("secStep", model.whatKindOfMeal)) "확인",
        selectedItem selectData
        , selectedFood model "계속 등록하기" "selectedleft" (Step("secStep", model.whatKindOfMeal))
        ]
    ]

whatKindOfMeal  =
    div [ class "container yf_container" ]
        [ div [ class "m_settingbox" ]
            [ div [ class "button m_settingmenu", onClick ( Step("secStep", "아침식단")) ]
                [ img [ src "../image/dite_morning.png" ]
                    [], text "아침식단" 
                ]
            , div [ class "button m_settingmenu", onClick ( Step("secStep", "점심식단")) ]
                [ img [ src "../image/dite_lunch.png" ]
                    [], text "점심식단" 
                ]
            , div [ class "button m_settingmenu1", onClick ( Step("secStep", "저녁식단")) ]
                [ img [ src "../image/dite_dinner.png" ]
                    [], text "저녁식단" 
                ]
            , div [ class "button m_settingmenu1", onClick ( Step("secStep", "간식식단")) ]
                [ img [ src "../image/dite_snack.png" ]
                    [], text "간식식단" 
                ]
            ]
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
selectData =
    [
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"},
        {name= "고구마", kcal = "100Kcal"}
    ]