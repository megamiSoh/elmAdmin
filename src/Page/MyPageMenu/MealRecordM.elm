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
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Date exposing (Date, Interval(..), Unit(..))
import Task exposing (Task)
import Time exposing(Month(..))
import Page.MyPageMenu.MyPageInfoLayout exposing (..)
import Round as Round
type alias Model = 
    { session : Session
    , checkDevice : String
    , stepDisplay : String
    , whatKindOfMeal : String
    , check : Bool
    , key : String
    , searchFoodName : String
    , page : Int
    , per_page : Int
    , foodData : Food
    , screenInfo : ScreenInfo
    , foodListData : List FoodData
    , offsetHeight : String
    , foodName : String
    , kcal : String
    , detailShow : Bool
    , active : String
    , foodQuantity : Float
    , foodQuantityString : String
    , mealRegistInfo : FoodRegist
    , date : String
    , meal_page : Int
    , meal_per_page : Int
    , mealDataList : Data
    , showList : Bool
    , derectRegistShow : Bool
    , directFoodName : String
    , directKcal : String
    , totalKcal : String
    , editShow : Bool
    , diary_no :String
    , registOrEdit : Maybe String
    , btnDisabled : Bool
    , scrHeight : Float
    , scrCount : Int
    , errType : String
    , deleteId : String
    }

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

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias FoodRegist = 
    { date : String
    , food_code : String
    , food_name : String
    , kcal : Float
    , one_kcal : Float
    , food_count : Float
    , is_direct : Bool }

type alias Data = 
    { data : List KindOfMeal
    , paginate : Page }

type alias KindOfMeal = 
    { diary_no : Int
    , food_count : String
    , food_name : String
    , is_direct: Bool
    , kcal : String
    , one_kcal : String }

type alias Page = 
    { diary_date : String
    , food_code : String
    , page : Int
    , per_page : Int
    , total_count : Int
    , user_id : Int }

type alias Kcal = 
    { kcal : String
    , one_kcal : String }

foodEncode : Int -> Int -> String -> Session -> (Result Http.Error Food -> Msg ) -> Cmd Msg
foodEncode page per_page name session msg = 
    let
        body = 
            E.object
                [ ("page" , E.int page)
                , ("per_page", E.int per_page)
                , ("name", E.string name )]
                |> Http.jsonBody
    in
    Api.post Endpoint.foodSearch (Session.cred session) msg body (Decoder.foodSearch Food FoodData FoodPage)

mealRegistInfo : FoodRegist -> Session -> Cmd Msg
mealRegistInfo foodInfo session = 
    let
        body =
            E.object
                [ ("date", E.string foodInfo.date)
                , ("food_code", E.string foodInfo.food_code)
                , ("food_name", E.string foodInfo.food_name)
                , ("kcal", E.float foodInfo.kcal)
                , ("one_kcal", E.float foodInfo.one_kcal)
                , ("food_count", E.float foodInfo.food_count)
                , ("is_direct", E.bool foodInfo.is_direct)]
                    |> Http.jsonBody
    in
    Api.post (Endpoint.mealRegistInfo) (Session.cred session) RegistMealComplete body (Decoder.resultD)

dayKindOfMealEncode : Int -> Int -> Session -> String -> String -> Cmd Msg
dayKindOfMealEncode page per_page session code date =
    let
        body = 
            E.object
                [ ("page", E.int page)
                , ("per_page", E.int per_page) ]
                |> Http.jsonBody
    in
    Api.post (Endpoint.dayKindOfMeal date code )(Session.cred session) GetMealData body (Decoder.dayKindOfMeal Data KindOfMeal Page)

mealEditInfo : FoodRegist -> Session -> String -> Cmd Msg
mealEditInfo foodInfo session diaryNo =
    let
        body =
            E.object
                [ ("food_name", E.string foodInfo.food_name)
                , ("kcal", E.float foodInfo.kcal)
                , ("one_kcal", E.float foodInfo.one_kcal)
                , ("food_count", E.float foodInfo.food_count)
                , ("is_direct", E.bool foodInfo.is_direct)]
                    |> Http.jsonBody
    in
    Api.post (Endpoint.mealEditInfo foodInfo.date diaryNo) (Session.cred session) RegistMealComplete body (Decoder.resultD)

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keyup" (Decode.map tagger keyCode)


init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = (
    { session = session
    , checkDevice = ""
    , stepDisplay = "firstStep"
    , whatKindOfMeal = ""
    , check = mobile
    , key = ""
    , searchFoodName = ""
    , page = 1
    , per_page = 20
    , foodListData = []
    , offsetHeight = "812"
    , foodName = ""
    , kcal = ""
    , detailShow = False
    , active = ""
    , scrHeight = 0
    , foodQuantity = 0
    , foodQuantityString = "0"
    , date = ""
    , meal_page = 1
    , meal_per_page = 20
    , showList = False
    , derectRegistShow = False
    , directFoodName = ""
    , directKcal = ""
    , totalKcal = ""
    , scrCount = 0
    , editShow = False
    , diary_no = ""
    , registOrEdit = Nothing
    , btnDisabled = True
    , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
    , foodData =
        { data = []
        , paginate = 
            { name = ""
            , page = 1
            , per_page = 20
            , total_count = 0}
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
    , mealDataList = 
        { data = []
        , paginate = 
            { diary_date = ""
            , food_code = ""
            , page = 1
            , per_page = 50
            , total_count = 0
            , user_id = 0 }
        }
    , errType = ""
    , deleteId = ""
    }
    , Cmd.batch [ Api.getKey ()
    , Date.today |> Task.perform ReceiveDate]
    )

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.batch[Api.receiveKey SaveKey
   , Session.changes GotSession (Session.navKey model.session)
   , Api.touch ReceiveHeight]

type Msg 
    = CheckDevice E.Value
    | BackBtn
    | DirectRegist (String, String, String)
    | Meal String
    | SaveKey E.Value
    | GetFoodData (Result Http.Error Food)
    | SearchFood String
    | ScrollEvent ScreenInfo
    | AddFoodData (Result Http.Error Food)
    | NoOp
    | GoDetail (String, Kcal , Maybe String)
    | QuantityFood String
    | Active String
    | PlusOrMinus String
    | GoRegist 
    | RegistMealComplete (Result Http.Error Decoder.Success)
    | ReceiveDate Date
    | GetMealData (Result Http.Error Data)
    | ShowRegistList 
    | DeleteItem String
    | MealDeleteComplete (Result Http.Error Decoder.Success)
    | DirectRegistInput String String
    | GoDirectRegist
    | GoEdit Bool
    | GotSession Session
    | SearchKeyUp Int
    | ReceiveHeight E.Value


toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

justToFloat item = 
    case String.toFloat item of
        Just a->
            a
    
        Nothing ->
            0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of    
        ReceiveHeight scrHeight ->
            case Decode.decodeValue Decode.int scrHeight of
                Ok ok ->
                    if ok /= model.scrCount then
                        if (model.foodData.paginate.total_count // model.per_page ) + 1 >= model.page then
                            if List.isEmpty model.foodData.data then
                            ({model | scrCount = ok }, Api.unfocus NoOp)
                            else
                            ({model | scrCount = ok , page = 1 + model.page}, foodEncode (1 + model.page) model.per_page model.searchFoodName model.session AddFoodData)
                        else
                        (model, Cmd.none)
                    else
                        (model, Cmd.none)
                Err err ->
                    (model, Cmd.none)
        SearchKeyUp id -> 
            (model, Cmd.batch[foodEncode model.page model.per_page model.searchFoodName model.session GetFoodData
            , (scrollToTop NoOp)])
        GotSession session ->
            ({model | session = session}, 
             case model.errType of
                "delete" ->
                     Api.get MealDeleteComplete (Endpoint.mealDelete model.date model.deleteId)(Session.cred session) Decoder.resultD
                "getmeal" ->
                     dayKindOfMealEncode model.meal_page model.meal_per_page session model.key model.date
                "regist" ->
                    mealRegistInfo model.mealRegistInfo session
                "getFood" ->
                    foodEncode model.page model.per_page model.searchFoodName session GetFoodData
                "addFood" ->
                    foodEncode (model.page) model.per_page model.searchFoodName session AddFoodData
                _ ->
                    dayKindOfMealEncode model.meal_page model.meal_per_page session model.key model.date )
        GoEdit is_direct ->
            let
                old = model.mealRegistInfo
                noDirectItem = 
                    { old | food_name = model.foodName
                    , one_kcal = justToFloat model.kcal
                    , kcal = (justToFloat model.kcal) * model.foodQuantity
                    , food_count = model.foodQuantity
                    , is_direct = False
                    , date = model.date}
                directItem = 
                    { old | food_name = model.directFoodName
                    , one_kcal = justToFloat model.directKcal
                    , kcal = justToFloat model.directKcal
                    , is_direct = True
                    , date = model.date}
            in
            
            if is_direct then
            ({model | editShow = False}, 
            -- Cmd.none
            mealEditInfo directItem model.session model.diary_no
            )
            else
            ({model | editShow = False}, mealEditInfo noDirectItem model.session model.diary_no)
        GoDirectRegist ->
            let
                old = model.mealRegistInfo
                result = 
                    { old | date = model.date
                    , food_code = model.key
                    , food_name = model.directFoodName
                    , one_kcal = justToFloat model.directKcal
                    , kcal = justToFloat model.directKcal
                    , is_direct = True}
            in
            ({model |mealRegistInfo = result}, mealRegistInfo result model.session)
        DirectRegistInput category contents ->
            case category of
                "food" ->
                    if contents == "" || model.directKcal == "" then
                    ({model | directFoodName = contents, btnDisabled = True}, Cmd.none)
                    else
                    ({model | directFoodName = contents, btnDisabled = False}, Cmd.none)
                "kcal" ->
                    case String.toFloat contents of
                        Just float ->
                            if contents == "" || model.directFoodName == "" then
                            ({model | directKcal = contents, btnDisabled = True}, Cmd.none)
                            else
                            ({model | directKcal = contents, btnDisabled = False}, Cmd.none)
                    
                        Nothing ->
                            ({model | directKcal = "", btnDisabled = True}, Cmd.none)

                _ ->
                    (model, Cmd.none)
        MealDeleteComplete (Ok ok) ->
            (model, Cmd.batch[dayKindOfMealEncode model.meal_page model.meal_per_page model.session model.key model.date
            , Api.showToast (E.string "삭제 되었습니다.")])
        MealDeleteComplete (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "delete"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        DeleteItem id ->
            ({model | deleteId = id}, Api.get MealDeleteComplete (Endpoint.mealDelete model.date id)(Session.cred model.session) Decoder.resultD)
        ShowRegistList ->
            ({model | showList = not model.showList}, Cmd.none)
        GetMealData (Ok ok) ->
            let
                filter = List.filterMap (\x -> String.toFloat x.kcal) ok.data
                total = List.sum filter
            in
            ({model | mealDataList = ok, totalKcal = Round.round 2 total }, Cmd.none)
        GetMealData (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "getmeal"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        ReceiveDate today ->
            ({model | date = getFormattedDate Nothing (Just today)} , Cmd.none)
        RegistMealComplete (Ok ok) ->
            ({model | detailShow = False, foodQuantity = 0, foodQuantityString = "0", derectRegistShow = False, directFoodName = "", directKcal = "", active = ""}, Cmd.batch[Api.showToast (E.string "등록되었습니다.")
            , dayKindOfMealEncode model.meal_page model.meal_per_page model.session model.key model.date])
        RegistMealComplete (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "regist"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        GoRegist ->
            let
                old = model.mealRegistInfo
                result = 
                    { old | date = model.date
                    , food_code = model.key
                    , food_name = model.foodName
                    , kcal = (justToFloat model.kcal) * model.foodQuantity
                    , one_kcal = justToFloat model.kcal
                    , food_count = model.foodQuantity
                    , is_direct = False}
            in
            ({model | active = "", btnDisabled = True, mealRegistInfo = result}, Cmd.batch[mealRegistInfo result model.session
            ])
        PlusOrMinus what ->
            -- case String.toFloat model.foodQuantityString of
            --     Just a ->
                    case what of
                        "plus" ->
                            ({model | foodQuantity = justToFloat model.foodQuantityString + justToFloat model.active, foodQuantityString = String.fromFloat(justToFloat model.foodQuantityString + justToFloat model.active)
                            , btnDisabled = False}, Cmd.none)
                        "minus" ->
                            if model.foodQuantity <= justToFloat model.active then
                                ({model | btnDisabled = True, foodQuantity = 0, foodQuantityString = "0"} , Cmd.none)
                            else
                                ({model | foodQuantity = model.foodQuantity - justToFloat model.active, foodQuantityString = String.fromFloat(model.foodQuantity - justToFloat model.active)
                                , btnDisabled = False}, Cmd.none)
                        _ ->
                            (model, Cmd.none)
                -- Nothing ->
                --     case what of
                --         "plus" ->
                --             ({model | foodQuantity = 1 + justToFloat model.active, foodQuantityString = String.fromFloat(1 + justToFloat model.active)
                --             , btnDisabled = False}, Cmd.none)
                --         "minus" ->
                --            ({model | btnDisabled = True}, Cmd.none)
                --         _ ->
                --             (model, Cmd.none)
    
        Active active ->
            ({model | active = active}, Cmd.none)
        QuantityFood quantity ->
            if justToFloat quantity <= 1 || quantity == "1"  then
            ({model | btnDisabled = True, foodQuantityString = quantity }, Cmd.none)    
            
            else
            ({model | foodQuantityString = quantity , btnDisabled = False}, Cmd.none)
            
            
        GoDetail (name, kcal, no ) ->
            ({model | foodName = name , kcal = kcal.kcal, detailShow = not model.detailShow, diary_no = 
                case no of
                    Just diaryno ->
                        diaryno
                
                    Nothing ->
                        ""
            , foodQuantityString = kcal.one_kcal
            , foodQuantity = justToFloat kcal.one_kcal
            , editShow = not model.editShow
            , registOrEdit = no
            , active = ""
            , btnDisabled = 
                case no of
                    Just ok ->
                        False
                
                    Nothing ->
                        True
            }, Cmd.none)
        NoOp ->
            (model, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
            
            -- let _ = Debug.log "hellow" scrollHeight
                
            -- in
            
            --  if (scrollHeight - Basics.round model.scrHeight) <= offsetHeight  then
            --     if (model.foodData.paginate.total_count // model.per_page ) + 1 >= model.page then
            --        if List.isEmpty model.foodData.data then
            --         (model, Api.unfocus NoOp)
            --         else
            --         ({model | page = 1 + model.page, offsetHeight = String.fromInt offsetHeight}, foodEncode (1 + model.page) model.per_page model.searchFoodName model.session AddFoodData) 
            --     else
                    (model, Api.unfocus NoOp)
            -- else
            --     ({model | offsetHeight = String.fromInt offsetHeight}, Cmd.batch[Api.unfocus NoOp
            --     ])
        SearchFood food ->
            ({model | searchFoodName = food, page = 1}, Cmd.none)
        GetFoodData (Ok ok) ->
            ({model | foodData = ok, foodListData = ok.data}, Cmd.none)
        GetFoodData (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "getFood"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        AddFoodData (Ok ok) ->
            ({model | foodData = ok, foodListData = model.foodListData ++ ok.data}, Cmd.none)
        AddFoodData (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            ({model | errType = "addFood"}, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        SaveKey key ->
            let
                decodeKey = Decode.decodeValue Decode.string key
            in
            
            case decodeKey of
                Ok ok ->
                    ({model | key = String.left 2 ok, stepDisplay = "secStep"}, dayKindOfMealEncode model.meal_page model.meal_per_page model.session (String.left 2 ok) (String.dropLeft 3 ok))
                Err err ->
                    (model, Cmd.none)
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
            ({model | key = ""}, Cmd.none)
        DirectRegist (name, kcal, id)->
                    ({model | derectRegistShow = not model.derectRegistShow, editShow = not model.editShow, directFoodName = name, directKcal = kcal, diary_no = id, btnDisabled = if id == "" then True else False, registOrEdit = if id == "" then Nothing else Just id}, Cmd.none)
        Meal str ->
            ({model | key = str}, Cmd.none)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        div[][allStep model]
    }

stepDetailItem meal model= 
    div [class "background"] [
        div [class ("topSearch_container " ++ (if model.detailShow then "fadeContainer" else "") ++ (
            if model.showList then " leftFade" else ""
        ))] [
            appHeaderConfirmDetailR (meal ++ "검색") "myPageHeader whiteColor" BackBtn ShowRegistList  "다음"
        ,
        searchInput model DirectRegist SearchFood(scrollEvent ScrollEvent) GoDetail SearchKeyUp
        ]
        , selectedFood model (String.fromInt model.mealDataList.paginate.total_count ++" 개의 음식") "selectedFood" ShowRegistList 
        , div [class ("registListContainer " ++ (if model.showList then "registNewOne" else "") ++
         if model.showList then 
            if model.detailShow  || model.derectRegistShow then " registFirst" 
            else ""
        else "")] [
            ul [class "registHeader"] 
                [ li[onClick ShowRegistList]
                    [ span [class "fas fa-angle-left"][] ]
                , li[][text meal]
                , li[][
                    a [Route.href Route.MyC , class "whiteColor"] [text "확인"]
                ]
                ]
            , div [] [
                
                selectedItem model.mealDataList.data DeleteItem model.totalKcal GoDetail DirectRegist
            ]
            -- , div [class "backToRegist", onClick ShowRegistList] [text "계속 등록하기"]
        ]
        
        , div [class ("m_mealRecordDetail  " ++ (if model.derectRegistShow then "newOne" else ""))]
            [ 
                case model.registOrEdit of
                    Just edit ->
                         ul [class "temporaryHeader"] 
                            [ li [onClick (DirectRegist ("","",""))]
                            [ span [class "fas fa-times"][] ]
                            , li[][text "음식 칼로리 수정"]
                            , li[][
                                button [onClick (GoEdit True), disabled model.btnDisabled, style "color" (if model.btnDisabled then "gray" else "#fff")] [text "저장"]
                            ] 
                            ]
                    Nothing ->
                         ul [class "temporaryHeader"] 
                        [ li [onClick (DirectRegist ("","",""))]
                        [ span [class "fas fa-times"][] ]
                        , li[][text "음식 칼로리 직접입력"]
                        , li[][button [onClick GoDirectRegist, disabled model.btnDisabled, style "color" (if model.btnDisabled then "gray" else "#fff")] [
                            text "등록"
                        ]]
                        ]
                , directRegist model DirectRegistInput
            ]
        , div [class ("m_mealRecordDetail " ++ (if model.detailShow then "newOne" else ""))
        ] [
            registLayout meal model
        ]
    ]

registLayout meal model = 
     div [] [
         case model.registOrEdit of
                Just beHere ->
                    ul [class "temporaryHeader"] 
                    [ li[onClick (GoDetail ("", {kcal = "", one_kcal = ""}, Nothing))]
                        [ span [class "fas fa-times"][] ]
                        , li[][text (meal ++ "수정")]
                        , li [] [
                            button [onClick (GoEdit False), disabled model.btnDisabled, style "color" (if model.btnDisabled then "gray" else "#fff")] [text "저장"]
                        ]  
                    ]
                Nothing ->        
                    ul [class "temporaryHeader"] 
                    [ li[onClick (GoDetail ("", {kcal = "", one_kcal = ""}, Nothing))]
                    [ span [class "fas fa-times"][] ]
                    , li[][text (meal ++ "등록")]
                    , li[][
                        button [onClick GoRegist, disabled model.btnDisabled, style "color" (if model.btnDisabled then "gray" else "#fff") ] [text "등록"]
                    ]
                    ]
            , div [class "m_food_title" ] [ text (model.foodName ++  model.kcal ++ "Kcal") ]
            ,
                input [class "input",  value (String.fromFloat model.foodQuantity), disabled True][] 
            
            , ul [class "m_quantity_btn_wrpa"] 
                [ li [class "button fas fa-plus", onClick (PlusOrMinus "plus")] []
                , li [classList 
                    [("button", True)
                    , ("m_active" , model.active == "1")]
                , onClick (Active "1")
                ] [text "1개"]
                , li [classList 
                    [("button", True)
                    , ("m_active" , model.active == "0.5")]
                , onClick (Active "0.5")
                ] [text "0.5개"]
                , li [classList 
                    [("button", True)
                    , ("m_active" , model.active == "0.25")]
                , onClick (Active "0.25")
                ] [text "0.25개"]
                , li [class "button fas fa-minus", onClick (PlusOrMinus "minus")] []
         ]
    ]
    
allStep model =
    div [class "container", style "overflow" "hidden", style "height" "100vh"] [
    case model.key of
        "10" ->
            stepDetailItem "아침식단" model
        "20" ->
            stepDetailItem "점식식단" model
        "30" ->
            stepDetailItem "저녁식단" model
        "40" ->
            stepDetailItem "간식식단" model
        _ ->
            div [] [
                appHeaderRDetail "식단기록" "myPageHeader  whiteColor" Route.MyPage "fas fa-angle-left"
                , whatKindOfMeal 
            ]
    ]   
whatKindOfMeal  =
    div [ class "container yf_container" ]
        [ div [ class "m_settingbox" ]
            [ div [ class "button m_settingmenu", onClick( Meal "10")]
                [ img [ src "../image/dite_morning.png" ]
                    [], text "아침식단" 
                ]
            , div [ class "button m_settingmenu" , onClick( Meal "20")]
                [ img [ src "../image/dite_lunch.png" ]
                    [], text "점심식단" 
                ]
            , div [ class "button m_settingmenu1" , onClick( Meal "30")]
                [ img [ src "../image/dite_dinner.png" ]
                    [], text "저녁식단" 
                ]
            , div [ class "button m_settingmenu1" , onClick( Meal "40")]
                [ img [ src "../image/dite_snack.png" ]
                    [], text "간식식단" 
                ]
            ]
        ]
