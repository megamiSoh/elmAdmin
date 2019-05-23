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

yourfitDetailListData listdata detaildata=    
    Decode.succeed listdata
        |> required "data" (Decode.list (yfDetail detaildata))

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
        |> required "data" (Decode.list (webtogetherdata togetherdata detail ))
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
        

webdetailtogether detail = 
    Decode.succeed detail
        |> required "id" int
        |> required "title" string
        |> required "thembnail" string

togetherdatawrap datawrap ogetherdata detail page item pair= 
    Decode.succeed datawrap
        |> required "data" (Decode.list (tdata ogetherdata detail item pair))
        |> required "paginate" (paginate page)

togetherdatalikewrap datawrap ogetherdata detail item pair= 
    Decode.succeed datawrap
        |> required "data" (tdata ogetherdata detail item pair)

mypostDataWrap datawrap ogetherdata detail item pair= 
    Decode.succeed datawrap
        |> required "data" (sddata ogetherdata detail item pair)

sddata togetherdata detail item pair= 
    Decode.succeed togetherdata
        |> optional "content" (Decode.map Just string)Nothing
        |> optional "detail" (Decode.map Just (Decode.list (sdetailTogether detail item pair))) Nothing
        |> required "id" int
        |> required "inserted_at" string 
        |> required "is_delete" bool
        |> required "link_code" string
        |> required "recommend_cnt" int
        |> optional "nickname" (Decode.map Just string) Nothing
        
        

sdetailTogether detail item pair= 
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


tdata togetherdata detail item pair= 
    Decode.succeed togetherdata
        |> optional "content" (Decode.map Just string) Nothing
        |> optional "detail" (Decode.map Just (Decode.list (sdetailTogether detail item pair))) Nothing
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