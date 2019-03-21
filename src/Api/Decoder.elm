module Api.Decoder exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)

type alias Success = 
   {result : String}

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
        |> required "exercise_part_name" string
        |> required "id" int
        |> required "title" string

yourfitDetailListData listdata detaildata=    
    Decode.succeed listdata
        |> required "data" (Decode.list (yfDetail detaildata))

yfDetail detaildata =
    Decode.succeed detaildata
        |> required "difficulty_name" string
        |> required "exercise_part_name" string
        |> required "id" int
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
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "is_use" bool
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
filterResult filterdata filterList =
    Decode.succeed filterdata
        |> required "data" (Decode.list (filterStep2 filterList))


filter filterList = 
    Decode.succeed filterList
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "exercise_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> optional "part_detail_name" (Decode.map Just (Decode.list string)) Nothing
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "value" (Decode.map Just int) Nothing

filterStep2 filterList = 
    Decode.succeed filterList
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> optional "exercise_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> optional "instrument_name" (Decode.map Just string) Nothing
        |> optional "part_detail_name" (Decode.map Just (Decode.list string)) Nothing
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "value" (Decode.map Just int) Nothing

getFilterDecoder getFilter= 
    Decode.succeed getFilter
        |> required "difficulty_code" (Decode.list string)
        |> required "exercise_code" (Decode.list string)
        |> required "instrument_code" (Decode.list string)
        |> required "part_detail_code" (Decode.list string)
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