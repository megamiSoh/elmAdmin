module Page.MyPageMenu.MealLayout exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)
import Page.Common exposing(..)

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keyup" (Decode.map tagger keyCode)

searchResult model scrollEvent detail = 
    div [ class "searchbox"]
        [ table [ class "table is-fullwidth yf_table"  ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "음식이름" ]
                    , th []
                        [ text "칼로리" ]
                    ]
                ]
            , tbody []
                (List.indexedMap(\idx x ->
                     mealLayout idx x model detail
                ) model.foodListData)
            ]
        ]
mealLayout idx item model detail = 
    tr [onClick (detail(item.name, {kcal = item.kcal, one_kcal = ""}, Nothing)), class "wordBreakTable"] [
        -- td [] [text (String.fromInt (idx + 1))],
        td[][text (item.name ++ (String.fromInt (idx + 1)) )],
        td[][text item.kcal ]
    ]

selectedItem selectData delete totalKcal editshow direct=
    div [ class "settingbox" ]
        [ table [ class "table is-fullwidth yf_table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "이름" ]
                    , th []
                        [ text "칼로리" ]
                    , th []
                        [text "삭제"]
                    ]
                ]
            , tfoot []
                [ tr []
                    [ th []
                        [ strong []
                            [ text "칼로리 총 합계(Kcal)" ]
                        ]
                    , th [colspan 2]
                        [ text totalKcal ]
                    ]
                ]
            , tbody []
                (List.map(\x -> selectTableLayout x delete editshow direct ) selectData)
            ]
        ]

selectTableLayout item delete editshow direct= 
    tr [class "wordBreakTable"] [
        td[onClick (
        if item.is_direct then
            direct (item.food_name, item.kcal, String.fromInt item.diary_no)
        else
        editshow (item.food_name, {kcal = item.one_kcal, one_kcal = item.food_count}, (Just (String.fromInt item.diary_no))
        )
    )] [text item.food_name],
        td[onClick (
        if item.is_direct then
            direct (item.food_name, item.kcal, String.fromInt item.diary_no)
        else
        editshow (item.food_name, {kcal = item.one_kcal, one_kcal = item.food_count}, (Just (String.fromInt item.diary_no))
        )
    )] [text item.kcal],
        td[onClick (delete (String.fromInt item.diary_no))] [
            i [ class "far fa-trash-alt" ]
                []
        ]
    ]
directRegist model directinput= 
    div [ class "container yf_container" ]
        [ div [ class "settingbox" ]
            [  input [ class "input m_meal_yf_inputfood", type_ "text", placeholder "음식 입력" , onInput (directinput "food"), value model.directFoodName, id (if model.derectRegistShow then "noScrInput" else "")]
                    []
                , input [ class "input m_meal_yf_inputkacl", type_ "text", placeholder "칼로리 입력" ,onInput (directinput "kcal"), value model.directKcal]
                    [], text "Kcal" 
            ]
        ]
searchInput model step search  scrollEvent detail event = 
    div [ id "searchHeight", class "m_meal_box_container scrollHegiht", scrollEvent
    -- , scrollEvent, style "height" (model.offsetHeight ++ "px")
    ] [
        div  [class "m_meal_box"]
        [ input [ class "input m_meal_yf_input", type_ "text", placeholder "음식을 검색하세요" , onInput search, onKeyDown event, id "keyboardBlur"]
            []
        , p [ class "help_searchresult" ]
            [ text (String.fromInt model.foodData.paginate.total_count ++ "개의 검색결과") ]
        , div [ class "button m_meal_direct", onClick (step("","",""))  ]
            [ text "음식 칼로리 직접입력" ]
        ]
    , searchResult model scrollEvent detail
    ]


selectedFood model title style step= 
    div [ class style, onClick step ]
        [ text title ]


