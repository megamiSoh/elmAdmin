module Page.MyPageMenu.Faq exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Route exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Page.Detail.FaqDetail as FaqDetail
type alias Model 
    = {
        session : Session
        , checkDevice : String
        , idx : String
        , check : Bool
        , faq : Faq
        , getId : Int
        , pageNum : Int
        , page : Int
        , per_page : Int
        , detail : Detail
        , screenInfo : ScreenInfo
        , ofheight : Bool
        , appFaqData : List Data
    }

type alias Faq =
    { data : List Data
    , paginate : Page }

type alias Data = 
    { id : Int
    , inserted_at : String
    , is_answer : Bool
    , title : String }

type alias Detail = 
    { answer : Maybe String
    , asked_id : Int
    , content : String
    , id : Int
    , is_answer : Bool
    , title : String
    , username : String }

type alias Page = 
    { asked_id : Int
    , end_date : String
    , is_answer : Maybe Bool
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    , username : String }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

init : Session -> Bool ->(Model, Cmd Msg)
init session mobile
    = (
        {session = session
        ,checkDevice = "",
        idx = ""
        , check = mobile
        , getId = 0
        , pageNum = 1
        , page = 1
        , per_page = 10
        , ofheight = False
        , appFaqData = []
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , faq = 
            { data = []
            , paginate =
                { asked_id = 0
                , end_date = ""
                , is_answer = Nothing
                , page = 1
                , per_page = 10
                , start_date = ""
                , title = ""
                , total_count = 0
                , username = ""}
            }
        , detail = 
            { answer = Nothing
            , asked_id = 0
            , content = ""
            , id = 0
            , is_answer = False
            , title = ""
            , username = "" }
        
        }
        , faqEncode 1 10 session
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Api.successSave GetDetail

faqEncode page per_page session =
    let
        body = 
            E.object 
                [ ("page", E.int page)
                , ("per_page", E.int per_page)]
                    |> Http.jsonBody 
    in
    Api.post Endpoint.faqlist (Session.cred session) FaqList body (Decoder.faqList Faq Data Page)
    
detailApi id session = 
    Api.get AppDetail (Endpoint.faqDetail id) (Session.cred session) (Decoder.faqdetail FaqDetail.Data Detail)

type Msg 
    = CheckDevice E.Value
    | Show Int Int
    | FaqList (Result Http.Error Faq)
    | DetailGo Int
    | GetDetail E.Value
    | PageBtn (Int, String)
    | AppDetail (Result Http.Error FaqDetail.Data)
    | GoDelete Int
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | ScrollEvent ScreenInfo

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
            let _ = Debug.log "scroll" scrollTop
                endOfPage =  model.faq.paginate.total_count // model.per_page 
            in
             if (scrollHeight - scrollTop) <= offsetHeight then
                if model.page < (endOfPage + 1) then
                let _ = Debug.log "scroll" scrollTop
                    
                in
                
                ({model | page = model.page + 1}, faqEncode  (model.page + 1) model.per_page model.session )
                else
                ({model | ofheight = True}, Cmd.none)
            else
                (model, Cmd.none)
        DeleteSuccess (Ok ok) ->
            (model, Cmd.batch [
                Api.showToast (E.string "삭제되었습니다.")
                , faqEncode model.page model.per_page model.session
            ])
        DeleteSuccess (Err err) ->
            (model, Cmd.none)
        GoDelete id ->
            ({model | getId = id}, Api.get DeleteSuccess (Endpoint.faqDelete (String.fromInt id)) (Session.cred model.session) (Decoder.resultD) )
        AppDetail (Ok ok) ->
            ({model | detail = ok.data}, Cmd.none)
        AppDetail (Err err) ->
            (model, Cmd.none)
        PageBtn (idx, str) ->
            let
                idxEncode = E.int idx
            in
            
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, Cmd.batch[faqEncode idx model.per_page model.session, Api.setCookie idxEncode])
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, Cmd.batch[faqEncode idx model.per_page model.session, Api.setCookie idxEncode])
                "go" -> 
                    ({model | page = idx}, Cmd.batch[faqEncode idx model.per_page model.session, Api.setCookie idxEncode])
                _ ->
                    (model, Cmd.none)
        GetDetail str ->
            (model, Route.pushUrl (Session.navKey model.session) Route.FaqD )
        DetailGo id ->
            ({model | getId = id}, Api.saveKey (E.string (String.fromInt id)))
        FaqList (Ok ok) ->
            -- if model.check then
            ({model | faq = ok, appFaqData = model.appFaqData ++ ok.data}, Cmd.none)
            -- else
            -- ({model | faq = ok}, Cmd.none)
        FaqList (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
            else
            (model, Cmd.none)
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model | checkDevice = string}, Cmd.none)
                
                    Err _ ->
                        ({model | checkDevice = ""}, Cmd.none)
                        
        Show idx id->
            if String.fromInt(idx) == model.idx then
            ({model| idx = ""}, Cmd.none)    
            else
            ({model| idx = String.fromInt(idx), getId = id}, detailApi (String.fromInt id) model.session)

view : Model -> {title : String , content : Html Msg}
view model =
    if model.check then
    { title = "YourFitExer"
    , content = 
        div [] [
            app model
        ]
    }
    else
    { title = "YourFitExer"
    , content = 
        div [] [
            web model
        ]
    }
web model = 
    div [ class "container" ]
        [
            commonJustHeader "/image/icon_qna.png" "1:1문의",
            if List.isEmpty model.faq.data then
            div [class "noResult"] [text "1:1 문의가 없습니다."]
            else
            div [] [
                contentsBody model,
                pagination 
                    PageBtn
                    model.faq.paginate
                    model.pageNum
            ],
            faqWrite
        ]
app model = 
    div [class "container"] [
        appHeaderConfirmDetail "1:1문의" "myPageHeader" Route.MyPage "fas fa-angle-left" Route.FaqW "글쓰기" , 
        appContentsBody model,
        floating

    ]
appContentsBody model=
    div [class (
            if model.ofheight then
            "m_faqEndScrollBox"
            else
            "m_fqaScrollbox"
    ) , scrollEvent ScrollEvent] [
        div [class "m_loadlistbox", scrollEvent ScrollEvent] (List.indexedMap (\idx x ->  itemLayout idx x model) model.appFaqData)
    ]
itemLayout idx item model =
        div [class "eventArea" ,onClick (Show idx item.id)] [
            div [class "eventAreaChild"] [
                pre [class "titleLeft"] [text item.title],
                div [class "qna_date inlineB"] [
                    ul [] [
                        li [] [text (String.dropRight 10 item.inserted_at)],
                        li [] [
                            if item.is_answer then
                            span [class "faq_done"] [text "답변완료"]
                            else
                            span [class "faq_waiting"] [text "대기중"]
                        ]
                    ]
                ]
            ]
            , expandQ idx model item.id
        ]
expandAnswer title = 
        div [class"answerbox"] [
            p[class"answerbox_text"][text ("Re: " ++  title)]
        ]
showAnswer answer= 
    tr [class"tr_showAnswer"]
        [ td [ colspan 2 ]
            [  pre [class "faq_q"][text (justString answer)] ]
        ]



expandQ idx model id =
    div [classList [
        ("heightZero", True),
        ("heightShow", String.fromInt(idx)  == model.idx )
    ]] [
        div [][
            pre[class"faq_q"] [text ( model.detail.content)],
            p [class"m_fnq_btn"  ]
                [ 
                    if model.detail.is_answer then
                    div [] []
                    else
                    div [] [
                        div [ class "button faq_q_btn ", onClick (DetailGo id)]
                        [ i [ class "fas fa-edit " ]
                            [], text "수정" 
                        ]
                        , div [ class "button faq_q_btn" , onClick (GoDelete id) ]
                        [ i [ class "far fa-trash-alt " ]
                            [], text "삭제" 
                        ]
                    ]
                ]
            ]
            ,
           
                expandAnswer (decodeChar model.detail.title)
                , showAnswer model.detail.answer

                
            
    ]


contentsBody model =
    div [ class "info_mediabox" ]
        [ div [ class "table info_yf_table" ]
            [ div [class "tableRow"]
                    [ div [class "tableCell faq_num"]
                        [ text "번호" ]
                    , div [class "tableCell faq_title"]
                        [ text "제목" ]
                    , div [class "tableCell faq_ing"]
                        [ text "진행사항" ]
                    , div [class "tableCell faq_date"]
                        [ text "등록일" ]
                    ]
            , tbody []
                (List.indexedMap (\idx x -> contentsLayout idx x model) model.faq.data)
            ]
        ]

contentsLayout idx item model = 
    div [class "tableRow cursor", onClick (DetailGo item.id)] [
        div [class "tableCell qna_numtext"] [text (
                    String.fromInt(model.faq.paginate.total_count - ((model.faq.paginate.page - 1) * 10) - (idx)  )
                )],
        div [class "tableCell qna_title_text"] 
            [text (
                item.title
                    |> String.replace "%26" "&"
                    |> String.replace "%25" "%"
            ) ],
        div [class "tableCell qna_ing_text"] [
            if item.is_answer then
            span [class "faq_done"] [text "답변 완료"]
            else
            span [class "faq_waiting"][text "대기중"]
        ],
        div [class "tableCell qna_title_date"] [text (String.dropRight 10 item.inserted_at)]
    ]

pagenation=
    div [ class "yf_Pagination" ]
        [ nav [ class "pagination is-centered" ]
            [ ul [ class "pagination-list" ]
                [ li [ class "" ]
                    [ a [ class "pagination-link"]
                        [ text "<" , text "<" ]
                    ]
                , a [ class "pagination-link"]
                    [ text "<" ]
                , li []
                    [ a [ class "pagination-link is-current yf_cut" ]
                        [ text "5" ]
                    ]
                , li []
                    [ a [ class "pagination-link"]
                        [ text ">" ]
                    ]
                , a [ class "pagination-link" ]
                    [ text ">>" ]
                ]
            ]
        ]
faqWrite = 
    div [ class " yf_dark" ]
        [ a [ class "button is-dark", Route.href Route.FaqW]
            [ text "글쓰기" ]
        ]
floating = 
    a [ Route.href Route.FaqW, class "float" ]
        [ i [ class "fas fa-pen icon_pen" ]
            []
        ]
     
justString item = 
    case item of
        Just ok ->
            decodeChar ok
    
        Nothing ->
            "등록된 답변이 없습니다."
decodeChar char = 
    char
        |> String.replace  "%26" "&"
        |> String.replace  "%25" "%"