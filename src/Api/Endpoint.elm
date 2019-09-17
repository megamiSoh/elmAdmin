module Api.Endpoint exposing 
    ( Endpoint
    , request
    , yourfitVideoList
    , yfDetail
    , level
    , yfDetailDetail
    , makeExerList
    , login
    , refresh
    , signup
    , toolFilter
    , exerFilter
    , levelFilter
    , partFilter
    , filter
    , registFilter
    , makeDetail
    , makeEdit
    , makeDelete
    , myInfo
    , changeNick
    , accountDelete
    , togetherList
    , togetherShare
    , myPost
    , myPostDelete
    , infolist
    , detailInfo
    , togetherLike
    , bodyRecord
    , getBodyInfo
    , scrap
    , togetherScrap
    , scrapList
    , scrapDetail
    , postList
    , pwdChange
    , editComplete
    , togetherlike
    , emailAuth
    , connect
    , temporaryPwd
    , profileImg
    , resetprofileImg
    , checkoverlapId
    , webtogetherList
    , videoCompleteRecord
    , faqlist
    , faqDetail
    , faqDelete 
    , faqregist
    , faqeidt
    , diary 
    , beforeImg
    , afterImg
    , dayKindOfMeal
    , foodSearch
    , mealRegistInfo
    , mealDelete
    , mealEditInfo
    , exerciseCompleteList
    , faqfaqList
    , faqfaqDetail
    , statisticalweek
    , statisticalMonth
    , askgender
    , askExercise_point
    , askSearch
    , askAnswer 
    , askResult
    , askExer 
    , askRecommend
    , sessionCheck
    , askdetail
    , productWeek
    , myPaperweightList
    , mypaperweightDetail
    , renewWeekExercise
    , askBirth
    , shareCode
    , bannerList
    , priceData
    , orderGo
    , possibleToCheck
    , orders
    , promote
    , togetherWrite
    )

import Http
import Url.Builder exposing (QueryParameter)

request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , tracker = config.tracker
        }


type Endpoint
    = Endpoint String

unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "http://13.209.49.169:4000/api"
    -- Url.Builder.crossOrigin "https://api.yfit.co.kr/api"
        ("v1" :: paths)
        queryParams
        |> Endpoint



refresh : Endpoint
refresh =
    url [ "auth", "front", "token"] []

login : Endpoint
login =
    url [ "auth","front", "sign_in" ] []

signup : Endpoint
signup = 
    url ["auth", "front", "sign_up"] []

sessionCheck : Endpoint 
sessionCheck = 
    url ["auth", "front" ,"show"][]

yourfitVideoList : Endpoint
yourfitVideoList = 
    url ["front", "exercises"] []

yfDetail : Endpoint
yfDetail =  
    url ["front", "exercises", "filter"] []

level : Endpoint
level = 
    url ["front", "exercises" , "difficulty_code"] []

yfDetailDetail : String -> Endpoint
yfDetailDetail id = 
    url ["front", "exercises", id] []

makeExerList : Endpoint
makeExerList = 
    url ["front", "fit"] []


toolFilter : Endpoint 
toolFilter = 
    url ["front", "fit", "instrument_code"] []

exerFilter : Endpoint
exerFilter =
    url ["front", "fit", "exercise_code"] []

levelFilter : Endpoint
levelFilter = 
    url ["front", "fit", "difficulty_code"] []

partFilter : Endpoint
partFilter = 
    url ["front", "fit", "part_detail_code"] []

filter : Endpoint
filter = 
    url ["front","fit","filter"] []

registFilter : Endpoint
registFilter = 
    url ["front","fit","new"] []


makeDetail : String -> Endpoint
makeDetail id= 
    url ["front","fit", id] []

makeEdit : String -> Endpoint
makeEdit id =
    url ["front","fit", id, "edit"] []

makeDelete : String -> Endpoint
makeDelete id = 
    url ["front","fit",id,"delete"] []

myInfo : Endpoint
myInfo = 
    url ["front","my"] []

changeNick : Endpoint 
changeNick =
    url ["front","my", "nickname"] []

accountDelete : Endpoint
accountDelete = 
    url ["front","my", "leave"] []

togetherList : Endpoint
togetherList = 
    url ["front", "together"] []

webtogetherList : Endpoint
webtogetherList = 
    url ["front", "together", "list"] []

togetherShare : String -> Endpoint
togetherShare id = 
    url ["front", "fit", id, "share"] []

myPost : Endpoint
myPost = 
    url ["front", "my", "boards"] []

myPostDelete : String -> Endpoint
myPostDelete id =
    url ["front", "my", id, "delete"] []

infolist : Endpoint
infolist = 
    url ["front", "notices"] []

detailInfo : String -> Endpoint
detailInfo id = 
    url ["front","notices",id] []

bodyRecord : Endpoint
bodyRecord =
    url ["front", "body", "new"] []

getBodyInfo : Endpoint
getBodyInfo = 
    url ["front" , "body"] []

scrapDetail : String -> String -> Endpoint
scrapDetail code id = 
    url ["front", "scraps", code, id] []

togetherLike : String -> Endpoint
togetherLike id = 
    url ["front", "together", id, "recommend"] []

scrap : String -> Endpoint    
scrap id = 
    url ["front", "exercises", id , "scrap"] []

togetherScrap : String -> Endpoint
togetherScrap id = 
   url ["front", "together", id , "scrap"] []

scrapList = 
    url ["front", "scraps"] []

postList : String -> Endpoint
postList id = 
    url ["front", "my", "boards", id] []

pwdChange : Endpoint
pwdChange = 
    url ["front","my","password"] []

editComplete : String -> Endpoint
editComplete id = 
    url ["front", "fit", id, "edit"] []

togetherlike : String -> Endpoint
togetherlike id = 
    url ["front", "together", id] []

emailAuth : Endpoint    
emailAuth = 
    url ["auth","send"] []

connect : Endpoint
connect = 
    url ["front", "connect"] []

temporaryPwd : Endpoint
temporaryPwd = 
    url ["auth", "password"] []

profileImg : Endpoint
profileImg = 
    url ["front", "my", "profile"] []

resetprofileImg : Endpoint
resetprofileImg = 
    url ["front", "my", "profile", "delete"] []

checkoverlapId : Endpoint
checkoverlapId = 
    url ["auth", "id" ,"posible"] []

videoCompleteRecord : String -> Endpoint
videoCompleteRecord id =
    url ["front", "my", "exercises", id][]

faqlist : Endpoint
faqlist = 
    url ["front", "inquiries"] []

faqDetail : String -> Endpoint
faqDetail id = 
    url ["front", "inquiries", id] []

faqDelete : String -> Endpoint
faqDelete id = 
    url ["front","inquiries",id, "delete"][]

faqregist : Endpoint
faqregist =
    url ["front", "inquiries", "new"] []

faqeidt : String -> Endpoint
faqeidt id =
    url ["front", "inquiries", id ,"edit"] []

diary : String -> Endpoint
diary date =
    url ["front", "diary", date] []

beforeImg : String -> Endpoint
beforeImg date = 
    url ["front", "body" , date , "before"][]

afterImg : String -> Endpoint
afterImg date = 
    url ["front", "body" , date , "after"][]

dayKindOfMeal : String -> String -> Endpoint
dayKindOfMeal date code = 
    url ["front", "foods" , date , code ][]

foodSearch : Endpoint
foodSearch =
    url ["front", "foods" ][]

mealRegistInfo : Endpoint
mealRegistInfo = 
    url ["front", "foods", "new"][]

mealDelete : String -> String -> Endpoint
mealDelete date no =
    url ["front", "foods", date, no , "delete"][]

mealEditInfo : String -> String -> Endpoint 
mealEditInfo date no = 
    url ["front", "foods", date, no , "edit"][]

exerciseCompleteList : String -> Endpoint
exerciseCompleteList date = 
    url ["front", "diary", "exercises", date] []

faqfaqList : Endpoint 
faqfaqList =
    url ["front", "faqs"][]

faqfaqDetail : String -> Endpoint
faqfaqDetail id = 
    url ["front", "faqs", id] []

statisticalweek : Endpoint
statisticalweek = 
    url ["front", "statistics" , "week"][]

statisticalMonth : String -> Endpoint
statisticalMonth date = 
    url ["front", "statistics", "month" , date][]

askgender : Endpoint
askgender =
    url ["front", "ask", "gender"][]

askExercise_point : Endpoint
askExercise_point = 
    url ["front", "ask", "exercise_point"][]

askSearch : Endpoint
askSearch = 
    url ["front", "ask", "search"] []

askAnswer : Endpoint
askAnswer = 
    url ["front","ask","answer"][]

askResult : Endpoint
askResult = 
    url ["front", "ask", "result"][]

askExer : Endpoint
askExer = 
    url ["front", "ask", "exercises"][]

askRecommend : Endpoint
askRecommend = 
    url ["front", "ask" , "recommend"][]

askdetail : String-> String -> Endpoint
askdetail no id = 
    url ["front" , "ask" , no , id][]

productWeek : Endpoint
productWeek =
    url ["front", "product", "new", "week"][]

myPaperweightList : Endpoint
myPaperweightList =
    url ["front", "products"][]


mypaperweightDetail : String -> Endpoint
mypaperweightDetail no = 
    url ["front", "products" , no][]

renewWeekExercise : Endpoint
renewWeekExercise = 
    url ["front", "product", "renew", "week"][]

askBirth : Endpoint
askBirth = 
    url ["front" , "ask" , "birthday"][]

shareCode : Endpoint
shareCode =
    url ["front", "together", "share_code"][]

bannerList : Endpoint
bannerList =
    url ["front", "home", "banners"][]

priceData : Endpoint
priceData = 
    url ["front", "products", "period"][]

orderGo : Endpoint
orderGo = 
    url ["front", "products", "request"][]

possibleToCheck : Endpoint
possibleToCheck = 
    url ["front", "orders", "any_is_ing"][]

orders : Endpoint
orders = 
    url ["front", "orders"][]

promote : Endpoint
promote = 
    url ["front","orders", "promote"][]

togetherWrite : Endpoint
togetherWrite = 
    url ["front", "together" , "new"][]