module Route exposing (..)

-- import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Session exposing (Session)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
-- import Browser.Navigation as Nav


-- ROUTING

-- navKey : Nav.Key
-- navKey key = key


type Route
    = Home
    | Other
    | YourFitExer
    | MakeExer
    | Together
    | MyPage
    | YourfitDetail
    | YourFitList
    | Filter
    | FilterS1
    | FilterS2
    | TogetherW
    | Faq
    | Info
    | MealR
    | MyC
    | MyPost
    | MyScrap
    | MyS
    | MakeDetail
    | MyAccount
    | MealRM
    | InfoD
    | FaqD
    | FaqW
    | Login
    | Signup
    | SC
    | MakeEdit
    | Empty
    | Logout
    | EditFilter
    | ScrapD
    | PostD
    | MSearch
    | MakeEditLast
    | Private
    | SetPwd
    | FPwd
    | LogoutConfirm
    | TA
    | MyPageBottomMenu
    | C
    | CD
    | MJList
    | MJD
    | YP
    | GP

parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (s "home")
        , Parser.map Other (s "other")
        , Parser.map YourFitExer (s "yourfitExercise")
        , Parser.map MakeExer (s "makeExercise")
        , Parser.map Together (s "together")
        , Parser.map MyPage (s "myPage")
        , Parser.map YourfitDetail (s "yourfitDetail")
        , Parser.map YourFitList (s "yourfitListDetail")
        , Parser.map Filter (s "filter")
        , Parser.map FilterS1 (s "filterStep1")
        , Parser.map FilterS2 (s "filterStep2")
        , Parser.map TogetherW (s "togetherWrite")
        , Parser.map Faq (s "faq")
        , Parser.map Info (s "info")
        , Parser.map MealR (s "mealRecord")
        , Parser.map MyC (s "myCalendar")
        , Parser.map MyPost (s "myPost")
        , Parser.map MyScrap (s "myScrap")
        , Parser.map MyS (s "myStatistical")
        , Parser.map MakeDetail (s "makeExerciseDetail")
        , Parser.map MyAccount (s "myAccount")
        , Parser.map MealRM (s "mealRecordM")
        , Parser.map InfoD (s "infoDetail")
        , Parser.map FaqD (s "faqDetail")
        , Parser.map FaqW (s "faqWrite")
        , Parser.map Login (s "login")
        , Parser.map Signup (s "signup")
        , Parser.map SC (s "signupComplete")
        , Parser.map MakeEdit (s "makeExerciseEdit")
        , Parser.map Empty (s "have2Login")
        , Parser.map Logout (s "logout")
        , Parser.map EditFilter (s "editFilter")
        , Parser.map PostD (s "myPostDetail")
        , Parser.map ScrapD (s "myScrapDetail")
        , Parser.map MSearch (s "makeExerciseSearch")
        , Parser.map MakeEditLast (s "makeEditStepLast")
        , Parser.map Private (s "privateTerms")
        , Parser.map SetPwd (s "setpassword")
        , Parser.map FPwd (s "forgotpassword")
        , Parser.map LogoutConfirm (s "logoutcomplete")
        , Parser.map TA (s "test")
        , Parser.map MyPageBottomMenu (s "open")
        , Parser.map C (s "contact")
        , Parser.map CD (s "contactDetail")
        , Parser.map MJList(s "paperweightList")
        , Parser.map MJD (s "paperweightDetail")
        , Parser.map YP (s "yourfitPrice")
        , Parser.map GP (s "gateProgress")
        ]


-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

-- backUrl : Session -> Nav.Key -> Int -> Cmd msg
backUrl key int =
    Nav.back key int

pushUrl : Nav.Key -> Route ->Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)
-- INTERNAL
load url = 
    Nav.load url

routeToString : Route -> String
routeToString page =
    let
        pages =
            case page of
                Home ->
                    []
                Other ->
                    ["other"]
                YourFitExer ->
                    ["yourfitExercise"]
                MakeExer ->
                    ["makeExercise"]
                Together ->
                    ["together"]
                MyPage ->
                    ["myPage"]
                YourfitDetail ->
                    ["yourfitDetail"]
                YourFitList ->
                    ["yourfitListDetail"]
                Filter ->
                    ["filter"]
                FilterS1 ->
                    ["filterStep1"]
                FilterS2 ->
                    ["filterStep2"]
                TogetherW ->
                    ["togetherWrite"]
                Faq ->
                    ["faq"]
                Info ->
                    ["info"]
                MealR ->
                    ["mealRecord"]
                MyC ->
                    ["myCalendar"]
                MyPost ->
                    ["myPost"]
                MyScrap ->
                    ["myScrap"]
                MyS ->
                    ["myStatistical"]
                MakeDetail ->
                    ["makeExerciseDetail"]
                MyAccount ->
                    ["myAccount"]
                MealRM ->
                    ["mealRecordM"]
                InfoD ->
                    ["infoDetail"]
                FaqD ->
                    ["faqDetail"]
                FaqW ->
                    ["faqWrite"]
                Login ->
                    ["login"]
                Signup ->
                    ["signup"]
                SC ->
                    ["signupComplete"]
                MakeEdit ->
                    ["makeExerciseEdit"]
                Empty ->
                    ["have2Login"]
                Logout ->
                    ["logout"]
                EditFilter ->
                    ["editFilter"]
                PostD ->
                    ["myPostDetail"]
                ScrapD ->
                    ["myScrapDetail"]
                MSearch->
                    ["makeExerciseSearch"]
                MakeEditLast ->
                    ["makeEditStepLast"]
                Private ->
                    ["privateTerms"]
                SetPwd ->
                    ["setpassword"]
                FPwd ->
                    ["forgotpassword"]
                LogoutConfirm ->
                    ["logoutcomplete"]
                TA ->
                    ["test"]
                MyPageBottomMenu ->
                    ["open"]
                C ->
                    ["contact"]
                CD ->
                    ["contactDetail"]
                MJList ->
                    ["paperweightList"]
                MJD ->
                    ["paperweightDetail"]
                YP ->
                    ["yourfitPrice"]
                GP ->
                    ["gateProgress"]
    in
    "#/" ++ String.join "/" pages