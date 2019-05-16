module Page.MyPageMenu.MealRecord exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Api as Api
import Json.Encode as Encode
import Json.Decode as Decode
import Api.Decoder as Decoder
import Api.Endpoint as Endpoint
import Http as Http
import Page.Common exposing(..)
import Date exposing (Date, Interval(..), Unit(..))
import Task exposing (Task)
import Time exposing(Month(..))
import Page.MyPageMenu.MyPageInfoLayout exposing (..)
import Route as Route

type alias Model = 
    { session : Session
    , activeBtn : Maybe String
    , check : Bool
    , page : Int
    , per_page : Int
    , data : Data
    , date : String
    , code : String
    , name : String
    , food : Food
    , kcal : String
    , mealPage : Int
    , mealPer_page : Int
    , activeQuantity : String
    , quantityValue : String
    , quantityShow : Bool
    , mealRegistInfo : FoodRegist
    , totalKcal : String
    , category : Maybe String
    , diary_no : String
    , foodSearch : String
    , pageNum : Int
    , directRegist : Bool
    , registFood : String
    , registKcal : String
    }

type alias Data = 
    { data : List KindOfMeal
    , paginate : Page }

type alias KindOfMeal = 
    { diary_no : Int
    , food_count : String
    , food_name : String
    , is_direct: Bool
    , kcal : String
    , one_kcal : String}

type alias Page = 
    { diary_date : String
    , food_code : String
    , page : Int
    , per_page : Int
    , total_count : Int
    , user_id : Int }

type alias Food = 
    { data : List FoodData 
    , paginate : FoodPage }

type alias FoodData = 
    { company : Maybe String
    , construct_year : String
    , kcal : String
    , name : String }

type alias FoodPage = 
    { name : String
    , page : Int
    , per_page : Int
    , total_count : Int }

type alias FoodRegist = 
    { date : String
    , food_code : String
    , food_name : String
    , kcal : Float
    , one_kcal : Float
    , food_count : Float
    , is_direct : Bool }

type alias RegistOrEdit = 
    { foodName : String
    , kcal : String
    , diaryNo : Int }

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    -- let
    --     todayDate = 
    --         ( Date.fromCalendarDate 2019 Jan 1
    --         , Date.today |> Task.perform ReceiveDate
    --         )
    -- in
    ({session = session
    , activeBtn = Just "10"
    , check = mobile
    , page = 1
    , per_page = 10
    , pageNum = 1
    , mealPage = 1
    , mealPer_page = 50
    , date = ""
    , code = "10"
    , name = ""
    , kcal = ""
    , quantityValue = ""
    , activeQuantity = ""
    , quantityShow = False
    , totalKcal = ""
    , category = Nothing
    , diary_no = ""
    , foodSearch = ""
    , directRegist = False
    , registFood = ""
    , registKcal = ""
    -- , today = Date.fromCalendarDate 2019 Jan 1
    , data = 
        { data = []
        , paginate = 
            { diary_date = ""
            , food_code = ""
            , page = 1
            , per_page = 10
            , total_count = 0
            , user_id = 0
            }
        }
    , food = 
        { data = []
        , paginate = 
            { name = ""
            , page = 1
            , per_page = 10
            , total_count = 0 
            }
        }
    , mealRegistInfo = 
       { date = ""
        , food_code = ""
        , food_name = ""
        , kcal = 0
        , one_kcal = 0
        , food_count = 0
        , is_direct = False 
        } 
    }
    , Cmd.batch[Api.getKey () 
    , Api.scrollControl ()
    , Date.today |> Task.perform ReceiveDate]
    )



foodEncode : Int -> Int -> String -> Session -> Cmd Msg
foodEncode page per_page name session = 
    let
        body = 
            Encode.object
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int per_page)
                , ("name", Encode.string name )]
                |> Http.jsonBody
    in
    Api.post Endpoint.foodSearch (Session.cred session) GetFoodData body (Decoder.foodSearch Food FoodData FoodPage)
    
dayKindOfMealEncode : Int -> Int -> Session -> String -> String -> Cmd Msg
dayKindOfMealEncode page per_page session code date =
    let
        body = 
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int per_page) ]
                |> Http.jsonBody
    in
    Api.post (Endpoint.dayKindOfMeal date code )(Session.cred session) GetMealData body (Decoder.dayKindOfMeal Data KindOfMeal Page)

mealEditInfo : FoodRegist -> Session -> String -> Cmd Msg
mealEditInfo foodInfo session diaryNo =
    let
        body =
            Encode.object
                [ ("food_name", Encode.string foodInfo.food_name)
                , ("kcal", Encode.float foodInfo.kcal)
                , ("one_kcal", Encode.float foodInfo.one_kcal)
                , ("food_count", Encode.float foodInfo.food_count)
                , ("is_direct", Encode.bool foodInfo.is_direct)]
                    |> Http.jsonBody
    in
    Api.post (Endpoint.mealEditInfo foodInfo.date diaryNo) (Session.cred session) RegistMealComplete body (Decoder.resultD)

mealRegistInfo : FoodRegist -> Session -> Cmd Msg
mealRegistInfo foodInfo session = 
    let
        body =
            Encode.object
                [ ("date", Encode.string foodInfo.date)
                , ("food_code", Encode.string foodInfo.food_code)
                , ("food_name", Encode.string foodInfo.food_name)
                , ("kcal", Encode.float foodInfo.kcal)
                , ("one_kcal", Encode.float foodInfo.one_kcal)
                , ("food_count", Encode.float foodInfo.food_count)
                , ("is_direct", Encode.bool foodInfo.is_direct)]
                    |> Http.jsonBody
    in
    Api.post (Endpoint.mealRegistInfo) (Session.cred session) RegistMealComplete body (Decoder.resultD)

subscriptions : Model -> Sub Msg
subscriptions model =
    Api.receiveKey ReceiveKey

type Msg 
    = ActiveTab String
    | ReceiveKey Encode.Value
    | GetMealData (Result Http.Error Data)
    | GetFoodData (Result Http.Error Food)
    | SearchInput String
    | FoodQuantity (RegistOrEdit,  Maybe String)
    | QuantityCheck String
    | UpNdown String
    | FoodQuantityClose
    | RegistMealComplete (Result Http.Error Decoder.Success)
    | RegistOrEditMeal String
    | MealDelete String
    | MealDeleteComplete(Result Http.Error Decoder.Success)
    | QuantityInput String
    | PageBtn (Int, String)
    | DirectRegistMeal String
    | DirectMealInput String String
    | ReceiveDate Date

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


justToint string =
    case String.toFloat string of
        Just int ->
            int
    
        Nothing ->
            0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveDate today ->
            ({model | date = getFormattedDate Nothing (Just today)} , dayKindOfMealEncode 1 10 model.session "10" (getFormattedDate Nothing (Just today)))
        DirectMealInput category contents->
            ( case category of
                "food" ->
                    {model | registFood = contents}
                "kcal" ->
                    case String.toFloat contents of
                        Just float ->
                            { model | registKcal = contents}        
                        Nothing ->
                            model
                _ ->
                    model
            , Cmd.none)
        DirectRegistMeal how ->
            let
                foodInfo = model.mealRegistInfo
                foodInfoUpdate = 
                    { foodInfo | date = model.date
                    , food_code  = model.code
                    , food_name = model.registFood
                    , kcal = justToint model.registKcal 
                    , one_kcal = justToint model.registKcal
                    , food_count = 1
                    , is_direct = True}
            in
            case how of
                "justshowNCancle" ->
                    ({model | directRegist = not model.directRegist, registKcal = "", registFood = ""}, Cmd.none)
                "regist" ->
                    ({model | directRegist = not model.directRegist}, mealRegistInfo foodInfoUpdate model.session)
                _ ->
                    (model, Cmd.none)
        PageBtn (idx, str) ->
            let
                idxEncode = Encode.int idx
            in
            
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, Cmd.batch[foodEncode idx model.per_page model.foodSearch model.session])
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, Cmd.batch[foodEncode idx model.per_page model.foodSearch model.session])
                "go" -> 
                    ({model | page = idx}, Cmd.batch[
                        foodEncode idx model.per_page model.foodSearch model.session, Api.setCookie idxEncode])
                _ ->
                    (model, Cmd.none)
        QuantityInput value ->
            -- case String.toFloat value of
            --     Just float ->
            --         if float == 0 then
            --             (model, Cmd.none)
            --         else
                        ({model |quantityValue = value }, Cmd.none)
            
                -- Nothing ->
                --     (model, Cmd.none)
        MealDeleteComplete (Ok ok) ->
            ({model | quantityShow = False}, Cmd.batch[dayKindOfMealEncode model.mealPage model.mealPer_page model.session model.code model.date
            , Api.showToast (Encode.string "삭제 되었습니다.")])
        MealDeleteComplete (Err err) ->
            (model, Cmd.none)
        MealDelete id ->
            (model, Api.get MealDeleteComplete (Endpoint.mealDelete model.date id)(Session.cred model.session) Decoder.resultD )
        RegistOrEditMeal category->
            let _ = Debug.log ""
                old = model.mealRegistInfo
                new = 
                    { old | date = model.date
                    , food_code = model.code
                    , food_name = model.name
                    , kcal = (justToint model.kcal) * (justToint model.quantityValue)
                    , one_kcal = (justToint model.kcal)
                    , food_count = (justToint model.quantityValue)
                    , is_direct = False }
            in
            case String.toFloat model.quantityValue of
                Just float ->
                    if float == 0 then
                        (model, Cmd.none)
                    else
                    case category of
                        "regist" ->
                            ({model | mealRegistInfo = new}, mealRegistInfo new model.session)    
                        "edit" ->
                            ({model | mealRegistInfo = new}, mealEditInfo new model.session model.diary_no) 
                        _ ->
                            (model, Cmd.none)
            
                Nothing ->
                    (model, Cmd.none)
            
        RegistMealComplete (Ok ok)->
            ({model | quantityShow = False }, Cmd.batch[dayKindOfMealEncode model.mealPage model.mealPer_page model.session model.code model.date
            , Api.showToast (Encode.string "등록 되었습니다.")
            , Api.getscrollHeight (Encode.bool (not model.quantityShow))]) 
        RegistMealComplete (Err err)->
            (model, Cmd.none)
        FoodQuantityClose ->
                ({model | quantityShow = False}                    
                , Api.getscrollHeight (Encode.bool (not model.quantityShow)))       
            
        UpNdown updown ->
            case String.toFloat model.quantityValue of
                Just int ->
                    case updown of
                        "up" ->
                            ({model | quantityValue = String.fromFloat(int + (justToint model.activeQuantity))}, Cmd.none) 
                        "down" ->
                            if (justToint model.quantityValue) <= 0 then
                                (model, Cmd.none)
                            else
                            ({model | quantityValue = String.fromFloat(int - (justToint model.activeQuantity))}, Cmd.none)
                        _->
                            (model, Cmd.none)
            
                Nothing ->
                    case updown of
                        "up" ->
                            ({model | quantityValue = String.fromFloat(0 + (justToint model.activeQuantity))}, Cmd.none) 
                        "down" ->
                            (model, Cmd.none)
                        _->
                            (model, Cmd.none)

        QuantityCheck active -> 
            ({model | activeQuantity = active}, Cmd.none)
        FoodQuantity (item , category) ->
            case category of
                Just ok ->
                  ({model | name = item.foodName , kcal = item.kcal, quantityShow = True, category = category, quantityValue = ok, diary_no = String.fromInt (item.diaryNo), activeQuantity = ""}, Api.getscrollHeight (Encode.bool (not model.quantityShow)))  
                Nothing ->
                    ({model | name = item.foodName , kcal = item.kcal, quantityShow = True, category = category, quantityValue = "", activeQuantity = ""}, Api.getscrollHeight (Encode.bool (not model.quantityShow)))
            
        SearchInput foodname ->
            ({model | foodSearch = foodname}, foodEncode model.page model.per_page foodname model.session)
        GetFoodData (Ok ok) ->
            let _ = Debug.log "ok" ok.data
                
            in
            
            ({model | food = ok}, Cmd.none)
        GetFoodData (Err err) ->
            (model, Cmd.none)
        GetMealData (Ok ok) ->
            let
                filter = List.filterMap (\x -> String.toFloat x.kcal) ok.data
                total = List.sum filter
            in
            let _ = Debug.log "f" filter
            in
            ({model | data = ok, totalKcal = String.fromFloat total }, Cmd.none)
        GetMealData (Err err) ->
            (model, Cmd.none)
        ReceiveKey code ->
            case Decode.decodeValue Decode.string code of
                Ok ok ->
                    ({model | code = ok, activeBtn = Just ok}, dayKindOfMealEncode model.mealPage model.mealPer_page model.session ok model.date)
                Err err ->
                    let _ = Debug.log "err" err
                        
                    in
                    
                    (model, Cmd.none)
        ActiveTab item ->
            ( {model | activeBtn = Just item, code = item}, dayKindOfMealEncode model.mealPage model.mealPer_page model.session item model.date )

view : Model -> {title : String , content : Html Msg}
view model =
    { title = "YourFitExer"
    , content =
        div [ class "container" ]
            [
                -- contentsHere
                tabMenu model,
                searchBox model,
                registMeal model ,
                saveBtn
                -- , div [class "foodQuantity_container_Wrap", style "display" (
                --     if model.quantityShow then
                --     "flex"
                --     else
                --     "none"
                -- )] [
                --     foodQuantity model
                -- ]
                , div [style "display" 
                ( if model.quantityShow then "flex" else "none")][
                case model.category of
                    Just ok ->  
                        div [class "foodQuantity_container_Wrap"] [
                            foodQuantity model
                        ]
                    Nothing ->
                        div [class "foodQuantity_container_Wrap"] [
                            foodQuantity model
                        ]]
                , div [class "foodQuantity_container_Wrap", style "display"
                    ( if model.directRegist then "flex" else "none")] [
                    directRegistMeal model
                ]
                        
            ]
    }

directRegistMeal model = 
    div [ class "inputkaclbox" ]
        [
            div [class "foodSettingClose", onClick FoodQuantityClose] [
            i [ class "far fa-times-circle" ][] 
            ]
            , div [ class "yf_inputbox" ]
            [ text "음식 칼로리 직접 입력" ]
        , div [ class "listinput_box" ]
            [ input [ class "input yf_inputfood", type_ "text", placeholder "음식 입력", onInput (DirectMealInput "food"), value model.registFood ]
                []
            , input [ class "input yf_inputkacl", type_ "text", placeholder "칼로리 입력" , onInput (DirectMealInput "kcal"), value model.registKcal]
                [] , text "Kcal" 
            ]
        , div [ class "listbtn_box" ]
            [ div [ class "button listadd_but cursor", onClick (DirectRegistMeal "regist") ]
                [ text "식단추가하기" ]
            , div [ class "button listadd2_but cursor", onClick (DirectRegistMeal "justshowNCancle") ]
                [ text "취소" ]
            ]
        ]


tabMenu model =
    div [ class "mealRecord_tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large" ]
            [ ul []
                [ li [ classList [
                        (" mealRecord_yf_active", model.activeBtn == Just "10")
                    ], onClick (ActiveTab "10")]
                    [ 
                        text "아침식단" 
                    ]
                , li [ classList [
                        ("mealRecord_yf_active", model.activeBtn == Just "20")
                    ], onClick (ActiveTab "20")]
                    [  
                        text "점심식단" 
                    ]
                , li [classList [
                        ("mealRecord_yf_active", model.activeBtn == Just "30")
                    ], onClick (ActiveTab "30")]
                    [  
                        text "저녁식단" 
                    ]
                , li [classList [
                        ("mealRecord_yf_active", model.activeBtn == Just "40")
                    ], onClick (ActiveTab "40")]
                    [ 
                        text "간식식단" 
                    ]
                ]
            ]
        ]

searchBox model = 
    div [ class "mealRecord_searchbox" ]
        [ div [ class "field mealRecord_yf_field" ]
            [ input [ class "input yf_food_input", type_ "text", placeholder "음식을 검색하세요", onInput SearchInput ]
                []
            , div [ class "button yf_food_searchwindow", href "yf_mypage_dite2.html", onClick (DirectRegistMeal "justshowNCancle")  ]
                [ text "음식 칼로리 직접입력" ]
            ]
            , searchResult model 
        ]

searchResult model =
    div [ class "mealRecord_searchbox2" ]
        [   div [class "searchResultCount"] [text ("총 " ++ String.fromInt model.food.paginate.total_count ++ "검색 결과")]
            , table [ class "table mealRecord_yf_table" ]
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
            , if List.isEmpty model.food.data then
                tbody []
                [ 
                tr [] [
                        td [colspan 3, class "tablenoResult"] [text "음식을 검색 해 주세요."]
                    ] 
                ]
            else
            tbody []
            ( List.indexedMap (\idx x -> mealLayout idx x model) model.food.data )
            , if List.isEmpty model.food.data then
                tfoot []
                []
            else
                tfoot [] [
                tr [] [
                    td[colspan 3][
                        pagination 
                PageBtn
                model.food.paginate
                model.pageNum
                    ]
                ]
            ]
            ]
            
        ]
foodQuantity model = 
    div [class "foodquantity_container"] 
        [ div [class "foodSettingClose", onClick FoodQuantityClose] [
            i [ class "far fa-times-circle" ][] 
        ]
        , div [class "foodTitle"] [
        text (model.name ++ " " ++ (model.kcal ++ " Kcal"))
        ]
        , div [class "foodquantity"] [
            input [type_ "text", class "input", placeholder "음식 수량을 입력 해 주세요.", type_ "number"
            , value model.quantityValue
            , onInput QuantityInput
            ] []
         ]
        , ul [class"foodquantity_btn"] 
            [ li [class "button fas fa-plus", onClick (UpNdown "up") ] []
            , li [classList [("button" , True)
            , ("quantityBtn", True)
            , ("activeQuantity", model.activeQuantity == "1")]
            , onClick (QuantityCheck "1") 
            ] 
            [text "1"]
            , li [classList [("button" , True)
            , ("quantityBtn", True)
            , ("activeQuantity", model.activeQuantity == "0.5")]
            , onClick (QuantityCheck "0.5") 
            ] [text "0.5"]
            , li [classList [("button" , True)
            , ("quantityBtn", True)
            , ("activeQuantity", model.activeQuantity == "0.25")]
            , onClick (QuantityCheck "0.25") 
            ] [text "0.25"]
            , li [class "button fas fa-minus", onClick (UpNdown "down")][]
            ]
            , ul [class "foodRegistComplete"] 
                [ 
                    case model.category of
                        Just ok ->
                           li [class "button", onClick (RegistOrEditMeal "edit")] 
                            [text  "수정"]
                    
                        Nothing ->
                            li [class "button", onClick (RegistOrEditMeal "regist")] 
                            [text  "등록"]                
                , li [class "button", onClick FoodQuantityClose] [text "취소"]
                ]
        ]
mealLayout idx item model =
    tr [onClick (FoodQuantity ({foodName = item.name , kcal = item.kcal, diaryNo = 0 }, Nothing)), class "cursor"] [
        td[][  text (
                    String.fromInt(model.food.paginate.total_count - ((model.food.paginate.page - 1) * 10) - (idx)  )
                ) ],
        td[][ text item.name ],
        td[][ text (item.kcal ++ " Kcal")]
    ]


registContents idx item =
    tr []
        [ th []
            [ text (String.fromInt (idx + 1)) ]
        , td [onClick (FoodQuantity ({foodName = item.food_name, kcal = item.one_kcal, diaryNo = item.diary_no}, Just item.food_count)), class "cursor"]
            [ text item.food_name ]
        , td []
            [ if item.is_direct then text " _ " else text (item.food_count ++ " 개") ]
        , td []
            [ text (item.kcal ++ " Kcal") ]
        , td [onClick (MealDelete (String.fromInt item.diary_no))]
            [ i [ class "far fa-trash-alt" ]
                []
            ]
        ]

registMeal model =
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
            , if List.isEmpty model.data.data then
                 tbody []
                [ 
                tr [] [
                        td [colspan 5, class "tablenoResult"] [text "등록된 식단이 없습니다."]
                    ] 
                ]
            else
             tbody []
                (List.indexedMap registContents model.data.data)
             , tfoot []
                [ tr []
                    [ th [colspan 3]
                        [ strong []
                            [ text "칼로리 총 합계" ]
                        ]
                    , th [colspan 2]
                        [ text (
                            if List.isEmpty model.data.data then
                            ""
                            else
                            (model.totalKcal ++  " Kacl")
                        ) ]
                    ]
                ]
            ]
        ]

saveBtn =
    div [ class " yf_dark" ]
        [ a [ class "button is-dark", Route.href Route.MyC ]
            [ text "캘린더로 이동" ]
        ]


 