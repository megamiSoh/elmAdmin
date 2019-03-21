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
searchResult model mealData step= 
    div [ class "searchbox" ]
        [ table [ class "table is-fullwidth yf_table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "음식이름" ]
                    , th []
                        [ text "칼로리" ]
                    ]
                ]
            , tbody []
                (List.map(\x ->
                     mealLayout x model step
                ) mealData)
            ]
        ]
mealLayout item model step = 
    tr [onClick step] [
        td[][text item.name ],
        td[][text item.kcal ]
    ]

selectedItem selectData=
    div [ class "settingbox" ]
        [ table [ class "table is-fullwidth yf_table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "이름" ]
                    , th []
                        [ text "칼로리" ]
                    , th []
                        []
                    ]
                ]
            , tfoot []
                [ tr []
                    [ th []
                        [ strong []
                            [ text "칼로리 총 합계" ]
                        ]
                    , th []
                        [ text "600 Kacl" ]
                    , th []
                        []
                    ]
                ]
            , tbody []
                (List.map selectTableLayout selectData)
            ]
        ]

selectTableLayout item = 
    tr [] [
        td[] [text item.name],
        td[] [text item.kcal],
        td[] [
            i [ class "far fa-trash-alt" ]
                []
        ]
    ]
directRegist model step = 
    div [ class "container yf_container" ]
        [ div [ class "settingbox" ]
            [ div [ class "m_meal_yf_inputbox" ]
                [ text "음식 칼로리 직접입력" ]
            , div [ class "m_meal_listinput_box" ]
                [ input [ class "input m_meal_yf_inputfood", type_ "text", placeholder "음식 입력" ]
                    []
                , input [ class "input m_meal_yf_inputkacl", type_ "text", placeholder "칼로리 입력" ]
                    [], text "Kcal" 
                , i [ class "far fa-trash-alt m_meal_trash" ]
                    []
                ]
            , div [ class "listbtn_box" , onClick step]
                [ div [ class "button m_meal_listadd_but" ]
                    [ text "음식칼로리추가" ]
                ]
            ]
        ]
searchInput model step =
    div  [class "m_meal_box"]
        [ input [ class "input m_meal_yf_input", type_ "text", placeholder "음식을 검색하세요" ]
            []
        , p [ class "help_searchresult" ]
            [ text "n개의 검색결과" ]
        , div [ class "button m_meal_direct", onClick step ]
            [ text "음식 칼로리 직접입력" ]
        ]


quantity =
    div [ class "container yf_container" ]
        [ div [ class "settingbox" ]
            [ h1 [ class "m_meal_food_kcal" ]
                [ text "호박 고구마 350Kcal" ]
            , div [ class "m_meal_settingbox" ]
                [ text "수량" , input [ class "input m_meal_q", type_ "text", placeholder "1" ]
                    [], text "개" 
                ]
            , div [ class "yf_optionssetting" ]
                [ div [ class "m_meal_box1" ]
                    [ a [ class "button m_meal_yf_but" ]
                        [ text "-" ]
                    , input [ class "input m_meal_yf_num2", type_ "text", value "0.25" ]
                        []
                    , a [ class "button m_meal_yf_but" ]
                        [ text "+" ]
                    ]
                , div [ class "m_meal_box1" ]
                    [ a [ class "button m_meal_yf_but" ]
                        [ text "-" ]
                    , input [ class "input m_meal_yf_num2", type_ "text", value "0.5" ]
                        []
                    , a [ class "button m_meal_yf_but" ]
                        [ text "+" ]
                    ]
                , div [ class "m_meal_box1" ]
                    [ a [ class "button m_meal_yf_but" ]
                        [ text "-" ]
                    , input [ class "input m_meal_yf_num2", type_ "text", value "1" ]
                        []
                    , a [ class "button m_meal_yf_but" ]
                        [ text "+" ]
                    ]
                ]
            ]
        ]
selectedFood model title style step= 
    div [ class style, onClick step ]
        [ text title ]
