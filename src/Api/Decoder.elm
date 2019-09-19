module Api.Decoder exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)

type alias Success = 
   {result : String}

type alias DataWrap = 
    { data : MyData }

type alias MyData =
    { exercise : Int
    , share : Int
    , user : UserData }

type alias UserData =
    { id : Int
    , nickname : Maybe String
    , username : String
    , profile : Maybe String}

resultD : Decoder Success
resultD = 
    Decode.succeed Success
        |> required "result" string


resultDecoder result = 
    Decode.succeed result
        |> required "result" string

tokenDecoder token=
    Decode.succeed token
        |> required "token" string

yourfitList listWrap listdata exerlist= 
    Decode.succeed listWrap 
        |> required "data" (Decode.list (yourfitListData listdata exerlist))

yourfitListData listdata exerlist=
    Decode.succeed listdata
        |> required "code" string
        |> required "exercises" (Decode.list (exerciseList exerlist))
        |> required "name" string

exerciseList exerlist = 
    Decode.succeed exerlist
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "mediaid" string
        |> required "thembnail" string 
        |> required "title" string

yourfitDetailListData listdata detaildata page =    
    Decode.succeed listdata
        |> required "data" (Decode.list (yfDetail detaildata))
        |> required "paginate" (paginateyoufitDetailList page)

paginateyoufitDetailList page = 
    Decode.succeed page
        |> required "difficulty_code" (Decode.list string)
        |> required "exercise_part_code" string
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int


yfDetail detaildata =
    Decode.succeed detaildata
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "mediaid" string
        |> required "thembnail" string 
        |> required "title" string

yfDetailDetail getData yfdetail detailitem pairItem= 
    Decode.succeed getData
        |> required "data" (yfDetailData yfdetail detailitem pairItem)

yfDetailData yfdetail detailitem pairItem=
    Decode.succeed yfdetail
        |> optional "difficulty_name" (Decode.map Just string)Nothing
        |> required "duration" string
        |> required "exercise_items" (Decode.list (yfDetailDataItem detailitem))
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "pairing" (Decode.list (pairingItem pairItem))
        |> required "title" string
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "thumbnail" string
        |> optional "description" (Decode.map Just string) Nothing


yfDetailDataItem detailitem= 
    Decode.succeed detailitem
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int

pairingItem pairItem=
    Decode.succeed pairItem
        |> required "file" string
        |> required "image" string
        |> required "title" string
-- makeexercise

makeExerList data listdata page= 
    Decode.succeed data
        |> required "data" (Decode.list (makeExerListData listdata) )
        |> required "paginate" (makeExerPage page)

makeExerListData listdata= 
    Decode.succeed listdata
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> required "duration" string
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "mediaid" string
        |> required "thembnail" string
        |> required "title" string
makeExerPage page= 
    Decode.succeed page 
        |> required "difficulty_code" string
        |> required "end_date" string
        |> required "exercise_part_code" string
        |> required "inserted_id" int
        |> required "make_code" string
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

-- filter
filterResult filterdata filterList page=
    Decode.succeed filterdata
        |> required "data" (Decode.list (filterStep2 filterList))
        |> required "paginate" (filterPagenate page)


filterPagenate page = 
    Decode.succeed page
        |> required "difficulty_code" (Decode.list string)
        |> required "exercise_code" (Decode.list string)
        |> required "instrument_code" (Decode.list string)
        |> required "page" int
        |> required "part_detail_code" (Decode.list string)
        |> required "per_page" int
        |> required "title" string
        |> required "total_count" int

filter filterList = 
    Decode.succeed filterList
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "exercise_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> required "part_detail_name" (Decode.list string)
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "value" (Decode.map Just int) Nothing
        |> optional "duration" (Decode.map Just string) Nothing
        |> optional "thembnail" (Decode.map Just string) Nothing

filterStep2 filterList = 
    Decode.succeed filterList
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "exercise_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> required "part_detail_name" (Decode.list string)
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "value" (Decode.map Just int) Nothing
        |> optional "duration" (Decode.map Just string) Nothing
        |> optional "thembnail" (Decode.map Just string) Nothing

getFilterDecoder getFilter= 
    Decode.succeed getFilter
        |> required "page" int
        |> required "per_page" int
        |> required "difficulty_code" (Decode.list string)
        |> required "exercise_code" (Decode.list string)
        |> required "instrument_code" (Decode.list string)
        |> required "part_detail_code" (Decode.list string)
        |> required "title" string


-- difficultyApi
levelDecoder levelData item = 
    Decode.succeed levelData
        |> required "data" (Decode.list (level item))

level item = 
    Decode.succeed item
        |> required "code" string
        |> required "name" string


-- makeExerciseDetail

makeExerDecoder data datainfo =
    Decode.succeed data
        |> required "data" (makedatainfo datainfo)
        

makedatainfo datainfo = 
    Decode.succeed datainfo
        |> optional "difficulty_name" (Decode.map Just string ) Nothing
        |> required "exercise_items" (Decode.list string)
        |> optional "exercise_part_name" (Decode.map Just string ) Nothing
        |> required "id" int
        |> required "title" string

makeedit data datainfo =
    Decode.succeed data
        |> required "title" string
        |> required "items" (Decode.list (makeEditData datainfo))

makeEditData datainfo = 
    Decode.succeed datainfo
        |> optional "action_id" (Decode.map Just int)Nothing
        |> required "is_rest" bool
        |> required "value" int

-- login
loginState login = 
    Decode.succeed login
        |> required "error" string

-- mypage
dataWRap datawrap my user= 
    Decode.succeed datawrap 
        |> required "data" (mydata my user)
        
sessionCheckMydata  = 
    Decode.succeed DataWrap
        |> required "data" sessionMyData

sessionMyData=    
    Decode.succeed MyData
        |> required "exercise" int
        |> required "share" int
        |> required "user" sessionuserdata

sessionuserdata  = 
    Decode.succeed UserData
        |> required "id" int
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
        |> optional "profile" (Decode.map Just string ) Nothing

mydata my user=    
    Decode.succeed my
        |> required "exercise" int
        |> required "share" int
        |> required "user" (userdata user)
       
userdata user = 
    Decode.succeed user
        |> required "id" int
        |> optional "nickname" (Decode.map Just string) Nothing
        |> required "username" string
        |> optional "profile" (Decode.map Just string ) Nothing

scrInfo info = 
    Decode.succeed info
        |> required "height" int
        |> required "scrTop" int
        |> required "scrHeight" int 

-- together
webtogetherdatawrap datawrap togetherdata detail page =
    Decode.succeed datawrap
        |> required "data" (Decode.list (webtogetherdata togetherdata detail  ))
        |> required "paginate" (paginate page)

webtogetherdata togetherdata detail = 
    Decode.succeed togetherdata
        |> optional "content" (Decode.map Just string)Nothing
        |> optional "detail" (Decode.map Just (Decode.list (webdetailtogether detail))) Nothing
        |> required "id" int
        |> required "inserted_at" string 
        |> required "is_delete" bool
        |> required "link_code" string
        |> required "recommend_cnt" int
        |> optional "nickname" (Decode.map Just string) Nothing
        |> optional "profile" (Decode.map Just string) Nothing
        |> required "share_code" string
        

webdetailtogether detail = 
    Decode.succeed detail
        |> required "id" int
        |> optional "title" (Decode.map Just string)Nothing
        |> optional "thembnail" (Decode.map Just string)Nothing
        |> optional "content" (Decode.map Just string)Nothing
        |> optional "photo" (Decode.map Just string)Nothing

togetherdatawrap datawrap ogetherdata detail page item pair snippet yItem = 
    Decode.succeed datawrap
        |> required "data" (Decode.list (tdata ogetherdata detail item pair snippet yItem))
        |> required "paginate" (paginate page)


        
    
togetherdatalikewrap datawrap ogetherdata detail item pair snippet yItem = 
    Decode.succeed datawrap
        |> required "data" (tdata ogetherdata detail item pair snippet yItem )

mypostDataWrap datawrap ogetherdata detail item pair snippets yItem= 
    Decode.succeed datawrap
        |> required "data" (sddata ogetherdata detail item pair snippets yItem)

sddata togetherdata detail item pair snippets yItem = 
    Decode.succeed togetherdata
        |> optional "content" (Decode.map Just string)Nothing
        |> optional "detail" (Decode.map Just (Decode.list (sdetailTogether detail item pair snippets yItem))) Nothing
        |> required "id" int
        |> required "inserted_at" string 
        |> required "is_delete" bool
        |> required "link_code" string
        |> required "recommend_cnt" int
        |> optional "nickname" (Decode.map Just string) Nothing
              

youtubeDetailItem snippet yItem = 
    Decode.succeed snippet 
        |> required "items" (Decode.list (youtubeItem yItem))

youtubeItem yItem = 
    Decode.succeed yItem 
        |> required "id" string
        

sdetailTogether detail item pair snippets yItem= 
    Decode.succeed detail 
        |> optional "thembnail" (Decode.map Just string) Nothing
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "duration" (Decode.map Just string) Nothing
        |> optional "exercise_items" (Decode.map Just (Decode.list (togetherItem item)))Nothing
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> optional "inserted_at" (Decode.map Just string) Nothing
        |> optional "pairing" (Decode.map Just (Decode.list (pairing pair)))Nothing
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "content" (Decode.map Just string) Nothing
        |> optional "snippet" (Decode.map Just (youtubeDetailItem snippets yItem)) Nothing  
        |> optional "photo" (Decode.map Just string) Nothing


tdata togetherdata detail item pair snippets yItem = 
    Decode.succeed togetherdata
        |> optional "content" (Decode.map Just string) Nothing
        |> optional "detail" (Decode.map Just (Decode.list (sdetailTogether detail item pair snippets yItem))) Nothing
        |> required "id" int
        |> required "inserted_at" string 
        |> required "is_delete" bool
        |> required "link_code" string
        |> required "recommend_cnt" int
        |> optional "nickname" (Decode.map Just string) Nothing
        |> optional "profile" (Decode.map Just string) Nothing
        

detailTogether detail item pair= 
    Decode.succeed detail 
        |> required "thembnail" string
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> required "duration" string
        |> required "exercise_items" (Decode.list (togetherItem item))
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "pairing" (Decode.list (pairing pair))
        |> required "title" string

togetherItem items = 
    Decode.succeed items
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int
pairing pair= 
    Decode.succeed pair
        |> required "file" string
        |> required "image" string
        |> required "title" string

paginate page = 
    Decode.succeed page
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int

mypostdata datawrap data page= 
    Decode.succeed datawrap 
        |> required "data" (Decode.list (mypostlist data))
        |> required "paginate" (mypostpage page)

mypostlist data= 
    Decode.succeed data
        |> optional "content" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "link_code" string
mypostpage page = 
    Decode.succeed page
        |> required "inserted_id" int
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int

infoData data datalist page =
    Decode.succeed data
        |> required "data" (Decode.list (infodatalist datalist))
        |> required "paginate" (infopage page)

infodatalist datalist = 
    Decode.succeed datalist
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string

infopage page = 
    Decode.succeed page
        |> required "end_date" string
        |> required "is_use" bool
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

detailInfo data detail = 
    Decode.succeed data 
        |> required "data" (detaillistInfo detail)

detaillistInfo detail = 
    Decode.succeed detail
        |> required "content" string
        |> required "id" int
        |> required "title" string
    

togetherLike like data= 
    Decode.succeed like
        |> required "data" (dataCount data)

dataCount data = 
    Decode.succeed data
        |> required "count" int
    
bodyInfo data list protain= 
    Decode.succeed data  
        |> required "data" (bodyInfoList list protain)

bodyInfoList list protain = 
    Decode.succeed list
        |> required "birthday" string
        |> required "body_no" int
        |> required "goal_weight" string
        |> required "height" string
        |> required "is_male" bool
        |> required "weight" string
        |> required "age" int
        |> required "protain" (bodyProtain protain)

bodyProtain protain = 
    Decode.succeed protain
        |> optional "need" (Decode.map Just int) Nothing
        |> optional "recommend" (Decode.map Just int) Nothing

myscrapData data list item page=
    Decode.succeed data
        |> required "data" (Decode.list (scrapDataList list item))
        |> required "paginate" (scrappage page)

scrapDataList list item= 
    Decode.succeed list
        |> required "detail" (Decode.list (scrapDataItem item))
        |> required "scrap_code" string
        |> required "scrap_id" int
scrapDataItem item= 
    Decode.succeed item
        |> required "id" int
        |> required "lookup" int
        |> optional "lookup_at" (Decode.map Just string) Nothing
        |> required "mediaid" string
        |> required "thembnail" string
        |> required "title" string
scrappage page= 
    Decode.succeed page
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "user_id" int
codeId ci = 
    Decode.succeed ci
        |> required "code" string
        |> required "id" string

makeEdit data item exitem pair= 
    Decode.succeed data
        |> required "data" (makeEditDetail item exitem pair)

makeEditDetail item exitem pair= 
    Decode.succeed item
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> required "duration"  string
        |> required "exercise_items"  (Decode.list (makeEditexitem exitem))
        |> optional "exercise_part_name" (Decode.map Just (Decode.list string)) Nothing
        |> required "id"  int
        |> required "inserted_at"  string
        |> required "pairing"  (Decode.list (makeeditpair pair))
        |> required "title"  string
        |> optional "description" (Decode.map Just string) Nothing

makeEditexitem exitem = 
    Decode.succeed exitem
        |> optional "action_id" (Decode.map Just int) Nothing
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> required "duration" string
        -- |> required "exercise_id" int
        |> optional "exercise_name" (Decode.map Just string ) Nothing
        |> optional "instrument_name" (Decode.map Just string ) Nothing
        -- |> required "is_rest" bool
        -- |> required "mediaid" string
        |> required "part_detail_name" (Decode.list (Decode.nullable string))
        -- |> required "sort" int
        |> required "thembnail" string
        |> required "title" string
        |> required "value" int

makeeditpair pair = 
    Decode.succeed pair
        |> required "file" string
        |> required "image" string
        |> required "title" string 

authMail data = 
    Decode.succeed data
        |> required "data" string

profileData data img= 
    Decode.succeed data
        |> required "data" (profileImage img)

profileImage img = 
    Decode.succeed img
        |> required "content_length" int
        |> required "content_type" string
        |> required "extension" string
        |> required "name" string
        |> required "origin_name" string
        |> required "path" string

checkOverlapmail data =
    Decode.succeed data
        |> required "data" bool

faqList faq data page = 
    Decode.succeed faq
        |> required "data" (Decode.list (faqdata data)) 
        |> required "paginate" (faqpage page)

faqdata data = 
    Decode.succeed data
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_answer" bool
        |> required "title" string

faqpage page = 
    Decode.succeed page
        |> required "asked_id" int
        |> required "end_date" string
        |> optional "is_answer" (Decode.map Just bool) Nothing
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int
        |> required "username" string

faqdetail data detail =
    Decode.succeed data 
        |> required "data" (faqdetaildata detail)
faqdetaildata detail =
    Decode.succeed detail
        |> optional "answer" (Decode.map Just string) Nothing
        |> required "asked_id" int
        |> required "content" string
        |> required "id" int
        |> required "is_answer" bool
        |> required "title" string
        |> required "username" string
    
diaryData data meal body kcal photo bmi=
    Decode.succeed data
        |> required "data" (diarymeal meal body kcal photo bmi)
    
diarymeal meal body kcal photo bmi = 
    Decode.succeed meal
        |> required "body" (diaryBody body bmi)
        |> required "date_exercise" string
        |> required "date_kcal" string
        |> required "kcal" (Decode.list (diaryKcal kcal))
        |> required "photo" (diaryPhoto photo)

diaryBody body bmi =
    Decode.succeed body
        |> required "age" int
        |> required "bmi" (diaryBmi bmi)
        |> required "bmr" float
        |> required "body_fat_percentage" float
        |> required "change_weight" string
        |> required "goal_weight" string
        |> required "height" string
        |> optional "is_male" (Decode.map Just bool) Nothing
        |> required "remain_weight" string
        |> required "weight" string
diaryBmi bmi =
    Decode.succeed bmi
        |>required "division" string
        |>required "value" float
        
diaryKcal kcal = 
    Decode.succeed kcal 
        |> required "food_code" string
        |> required "kcal" string
    

diaryPhoto photo = 
    Decode.succeed photo
        |>optional "after" (Decode.map Just string) Nothing
        |>optional "before" (Decode.map Just string) Nothing

myBodyImg data img =
    Decode.succeed data
    |> required "data" (myBody img)

myBody img = 
    Decode.succeed img
    |> required "content_length" int
    |> required "content_type" string
    |> required "extension" string
    |> required "name" string
    |> required "origin_name" string
    |> required "path" string

dayKindOfMeal data meal page = 
    Decode.succeed data
        |> required "data" (Decode.list (dayKindOfMealData meal))
        |> required "paginate" (dayKindOfPage page)

dayKindOfMealData meal = 
    Decode.succeed meal
        |> required "diary_no" int
        |> required "food_count" string
        |> required "food_name" string
        |> required "is_direct" bool
        |> required "kcal" string
        |> required "one_kcal" string

dayKindOfPage page = 
    Decode.succeed page
        |> required "diary_date" string
        |> required "food_code" string
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "user_id" int

foodSearch data food page = 
    Decode.succeed data
        |> required "data" (Decode.list (foodSearchData food))
        |> required "paginate" (foodSearchPage page) 

foodSearchData food = 
    Decode.succeed food
        |> optional "company" (Decode.map Just string ) Nothing
        |> required "construct_year" string
        |> required "kcal" string
        |> required "name" string

foodSearchPage page = 
    Decode.succeed page
        |> required "name" string
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int

exerciseCompleteList data list page=
    Decode.succeed data
        |> required "data" (Decode.list (exerciseCompleteData list))
        |> required "paginate" (exerciseCompleteListPaginate page)


exerciseCompleteData list = 
    Decode.succeed list
        |> required "exercise_no" int
        |> required "exericse_id" int
        |> required "mediaid" string
        |> required "thembnail" string
        |> required "title" string

exerciseCompleteListPaginate page = 
    Decode.succeed page
        |> required "date" string
        |> required "exercise_date" string
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "user_id" int


faqfaqList datawrap data page = 
    Decode.succeed datawrap
        |> required "data" (Decode.list (faqfaqdata data))
        |> required "paginate" (faqfaqpage page)

faqfaqdata data = 
    Decode.succeed data
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
        |> required "title" string


faqfaqpage page = 
    Decode.succeed page
        |> required "end_date" string
        |> required "is_use" bool
        |> required "page" int
        |> required "per_page" int
        |> required "start_date" string
        |> required "title" string
        |> required "total_count" int

faqfaqDetail data detail = 
    Decode.succeed data
        |> required "data" (faqfaqdetailData detail)

faqfaqdetailData detail = 
    Decode.succeed detail
        |> required "content" string
        |> required "id" int
        |> required "title" string

statisticalWeek data list= 
    Decode.succeed data 
        |> required "data" (Decode.list (statisticalWeekData list ))
        
    
statisticalWeekData list = 
    Decode.succeed list
        |> required "date" string
        |> optional "exercise" (Decode.map Just string) Nothing
        |> optional "food" (Decode.map Just string) Nothing
        |> optional "weight" (Decode.map Just string) Nothing


askyours data ask item= 
    Decode.succeed data
        |> required "data" (askyoursData ask item)

askyoursData ask item= 
    Decode.succeed ask
        |> required "content" string
        |> optional "default" (Decode.map Just bool) Nothing
        |> required "items" (Decode.list (askyoursItems item))
        |> required "name" string

askyoursItems item = 
    Decode.succeed item 
        |> required "text" string
        |> required "value" bool


askyoursPoint data ask item= 
    Decode.succeed data
        |> required "data" (askyoursDataPoint ask item)

askyoursDataPoint ask item= 
    Decode.succeed ask
        |> required "content" string
        |> optional "default" (Decode.map Just bool) Nothing
        |> required "items" (Decode.list (askyoursItemsPoint item))
        |> required "name" string

askyoursItemsPoint item = 
    Decode.succeed item 
        |> required "code" string
        |> required "name" string


askSearch data item = 
    Decode.succeed data 
        |> required "data" (Decode.list (askSearchItem item))

askSearchItem item=
    Decode.succeed item
        |> required "content" string
        |> required "exercise_part_code" string
        |> required "id" int

askResultData data item result detail =
    Decode.succeed data 
        |> required "data" (askresult item result detail )

askresult item result detail= 
    Decode.succeed item
        |> required "ask_no" int
        |> required "result" (askresultresult result detail)
        

askresultresult result detail = 
    Decode.succeed result
        |> required "content" string
        |> required "detail" (Decode.list (askresultDetail detail))
        |> required "part" string
        |> required "target" string

askresultDetail detail = 
    Decode.succeed detail
        |> required "content" string
        |> required "name" string
        |> required "sort" int

askExer data list = 
    Decode.succeed data 
        |> required "data" (Decode.list (askExerList list))

askExerList list = 
    Decode.succeed list
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_part_name" string
        |> required "exercise_id" int
        |> required "mediaid" string
        |> required "thembnail" string
        |> required "title" string
        |> required "ask_no" int
        |> required "is_ing" bool

sessionCheck data = 
    Decode.succeed data
        |> required "id" int
        |> required "username" string




askDetailData data detail item pair= 
    Decode.succeed data
        |> required "data" (askDetail detail item pair)

askDetail detail item pair= 
    Decode.succeed detail 
        |> required "description" string
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_id" int
        |> required "exercise_items" (Decode.list (askDetailItem item))
        |> required "exercise_part_name" string
        |> required "thumbnail" string
        |> required "title" string
        |> required "is_ing" bool
        |> required "pairing" (Decode.list (detailmypaperPairing pair))

askDetailItem item =
    Decode.succeed item
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int


myPaperweightList data item detail page = 
    Decode.succeed data 
        |> required "data" (Decode.list (mypaperweight item detail ))
        |> required "paginate" (mypaperweightpage page)


mypaperweight item detail = 
    Decode.succeed item
        |> required "bought_at" string
        |> required "detail" (Decode.list (mypaperweightDetail detail))
        |> required "end_at" string
        |> required "is_buy" bool 
        |> required "product_code" string
        |> required "product_id" int
        |> required "product_no" int
        |> required "start_at" string

mypaperweightDetail detail = 
    Decode.succeed detail
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "thembnail" string
        |> required "title" string

mypaperweightpage page = 
    Decode.succeed page
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "user_id" int

detailMypaperweight data detail item pair  = 
    Decode.succeed data 
        |> optional "data" (Decode.map Just (detailMypaperData detail item pair) ) Nothing

detailMypaperData detail item pair = 
    Decode.succeed detail
        |> required "description" string
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_items" (Decode.list (detailmypaperitem item))
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_buy" bool
        |> required "pairing" (Decode.list (detailmypaperPairing pair))
        |> required "product_no" int
        |> required "thumbnail" string
        |> required "title" string


detailmypaperitem item  = 
    Decode.succeed item 
        |> optional "action_id" (Decode.map Just int) Nothing
        |> optional "difficulty_name" (Decode.map Just string )Nothing
        |> required "duration" string
        |> required "exercise_id" int
        |> optional "exercise_name" (Decode.map Just string )Nothing
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> required "is_rest" bool
        |> required "mediaid" string
        |> required "part_detail_name" (Decode.list (Decode.nullable string))
        |> required "sort" int
        |> required "thembnail" string
        |> required "title" string
        |> required "value" int

detailmypaperPairing pair = 
    Decode.succeed pair
        |> required "file" string
        |> required "image" string
        |> required "title" string




myaskDetailData data detail item= 
    Decode.succeed data
        |> required "data" (myaskDetail detail item)

myaskDetail detail item= 
    Decode.succeed detail 
        |> required "description" string
        |> required "difficulty_name" string
        |> required "duration" string
        |> required "exercise_id" int
        |> required "exercise_items" (Decode.list (myaskDetailItem item))
        |> required "exercise_part_name" string
        |> required "product_no" int
        |> required "thumbnail" string
        |> required "title" string
        |> required "is_buy" bool

myaskDetailItem item =
    Decode.succeed item
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int


myaskBirthData data birth = 
    Decode.succeed data
        |> required "data" (myaskBirth birth)

myaskBirth birth =
    Decode.succeed birth
        |> required "content" string
        |> required "default" (Decode.nullable string)
        |> required "name" string

shareData data share= 
    Decode.succeed data 
        |> required "data" (Decode.list (shareType share))

shareType share = 
    Decode.succeed share
        |> required "code" string
        |> required "name" string


bannerListdata data banner = 
    Decode.succeed data
        |> required "data" (Decode.list (bannerList banner))

bannerList banner = 
    Decode.succeed banner
        |> required "description" string
        |> required "id" int
        |> required "is_link" bool
        |> optional "link" ( Decode.map Just string ) Nothing
        |> required "src" string
        |> optional "target" ( Decode.map Just string ) Nothing
        |> required "title" string
        |> required "backcolor" ( Decode.nullable string )
        |> required "is_vertical" bool

priceData  data price = 
    Decode.succeed data
        |> required "data" (Decode.list (priceList price))

priceList price = 
    Decode.succeed price 
        |> required "day_num" int
        |> required "description" string
        |> required "id" int
        |> required "is_pay" bool
        |> required "name" string
        |> required "price" int

orderData data item = 
    Decode.succeed data
        |> required "data" (order item)

order item = 
    Decode.succeed item 
        |> required "amount" int
        |> required "buyer_email" string
        |> required "buyer_name" string
        |> required "digital" bool
        |> required "merchant_uid" string
        |> required "name" string

possibleToWatch data = 
    Decode.succeed data 
        |> required "data" bool

ordersData data list page = 
    Decode.succeed data
        |> required "data" (Decode.list (ordersList list))
        |> required "paginate" (orderPaginate page)

ordersList list = 
    Decode.succeed list 
        |> required "bought_at" string
        |> required "end_at" string
        |> required "id" int
        |> required "is_ing" bool
        |> required "name" string
        |> required "price" int
        |> required "start_at" string
        |> required "state" string

orderPaginate page = 
    Decode.succeed page 
        |> optional "is_pay" (Decode.map Just bool) Nothing
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int
        |> required "user_id" int
