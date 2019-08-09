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
    , askBirth)

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

signup = 
    url ["auth", "front", "sign_up"] []
 
sessionCheck = 
    url ["auth", "front" ,"show"][]

-- yourfitvideo
yourfitVideoList = 
    url ["front", "exercises"] []

yfDetail =  
    url ["front", "exercises", "filter"] []


level = 
    url ["front", "exercises" , "difficulty_code"] []

yfDetailDetail id = 
    url ["front", "exercises", id] []

makeExerList = 
    url ["front", "fit"] []


-- filterCode 
toolFilter = 
    url ["front", "fit", "instrument_code"] []

exerFilter =
    url ["front", "fit", "exercise_code"] []

levelFilter= 
    url ["front", "fit", "difficulty_code"] []

partFilter = 
    url ["front", "fit", "part_detail_code"] []

filter = 
    url ["front","fit","filter"] []

registFilter = 
    url ["front","fit","new"] []

-- makeDetail

makeDetail id= 
    url ["front","fit", id] []

-- makeEdit
makeEdit id =
    url ["front","fit", id, "edit"] []

makeDelete id = 
    url ["front","fit",id,"delete"] []


-- mypage
myInfo = 
    url ["front","my"] []

 
changeNick=
    url ["front","my", "nickname"] []

accountDelete = 
    url ["front","my", "leave"] []

-- together

togetherList = 
    url ["front", "together"] []


webtogetherList = 
    url ["front", "together", "list"] []

togetherShare id = 
    url ["front", "fit", id, "share"] []

-- mypage

myPost = 
    url ["front", "my", "boards"] []


myPostDelete id =
    url ["front", "my", id, "delete"] []

infolist = 
    url ["front", "notices"] []

detailInfo id = 
    url ["front","notices",id] []

bodyRecord =
    url ["front", "body", "new"] []

getBodyInfo = 
    url ["front" , "body"] []

scrapDetail code id = 
    url ["front", "scraps", code, id] []


-- together

togetherLike id = 
    url ["front", "together", id, "recommend"] []

    
scrap id = 
    url ["front", "exercises", id , "scrap"] []

togetherScrap id = 
   url ["front", "together", id , "scrap"] []

scrapList = 
    url ["front", "scraps"] []

postList id = 
    url ["front", "my", "boards", id] []


-- password
pwdChange = 
    url ["front","my","password"] []
editComplete id = 
    url ["front", "fit", id, "edit"] []


togetherlike id = 
    url ["front", "together", id] []
    
-- emailAuth
emailAuth = 
    url ["auth","send"] []

connect = 
    url ["front", "connect"] []

temporaryPwd = 
    url ["auth", "password"] []

profileImg = 
    url ["front", "my", "profile"] []

resetprofileImg = 
    url ["front", "my", "profile", "delete"] []

checkoverlapId = 
    url ["auth", "id" ,"posible"] []

videoCompleteRecord id =
    url ["front", "my", "exercises", id][]


-- faq
faqlist = 
    url ["front", "inquiries"] []

faqDetail id = 
    url ["front", "inquiries", id] []
faqDelete id = 
    url ["front","inquiries",id, "delete"][]
faqregist =
    url ["front", "inquiries", "new"] []

faqeidt id =
    url ["front", "inquiries", id ,"edit"] []

diary date =
    url ["front", "diary", date] []

beforeImg date = 
    url ["front", "body" , date , "before"][]

afterImg date = 
    url ["front", "body" , date , "after"][]

dayKindOfMeal date code = 
    url ["front", "foods" , date , code ][]

foodSearch =
    url ["front", "foods" ][]

mealRegistInfo = 
    url ["front", "foods", "new"][]

mealDelete date no=
    url ["front", "foods", date, no , "delete"][]

mealEditInfo date no = 
    url ["front", "foods", date, no , "edit"][]

exerciseCompleteList date = 
    url ["front", "diary", "exercises", date] []

faqfaqList =
    url ["front", "faqs"][]

faqfaqDetail id = 
    url ["front", "faqs", id] []

statisticalweek = 
    url ["front", "statistics" , "week"][]

statisticalMonth date = 
    url ["front", "statistics", "month" , date][]


askgender =
    url ["front", "ask", "gender"][]

askExercise_point = 
    url ["front", "ask", "exercise_point"][]

askSearch = 
    url ["front", "ask", "search"] []

askAnswer = 
    url ["front","ask","answer"][]

askResult = 
    url ["front", "ask", "result"][]

askExer = 
    url ["front", "ask", "exercises"][]

askRecommend = 
    url ["front", "ask" , "recommend"][]

askdetail no id = 
    url ["front" , "ask" , no , id][]

productWeek =
    url ["front", "product", "new", "week"][]

myPaperweightList =
    url ["front", "products"][]

mypaperweightDetail no = 
    url ["front", "products" , no][]

renewWeekExercise = 
    url ["front", "product", "renew", "week"][]

askBirth = 
    url ["front" , "ask" , "birthday"][]