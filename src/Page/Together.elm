module Page.Together exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing (..)
import Port as P
import Json.Encode as E
import Json.Decode as Decode
import Api as Api
type alias Model 
    = {
        session : Session,
        isActive : String,
        checkDevice : String
        , check : Bool
    }
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    =
     (
        {session = session
        ,isActive = "All"
        ,checkDevice = ""
        , check = mobile
        }
        ,  P.checkMobile ()
    )

type Msg 
    = IsActive String
    | CheckDevice E.Value

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions :Model -> Sub Msg
subscriptions model=
    P.check CheckDevice

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IsActive title ->
            ({model | isActive = title} , Cmd.none)
        CheckDevice str ->
           let 
                result =
                    Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model| checkDevice = string}, Cmd.none)
                    Err _ -> 
                        ({model | checkDevice = ""}, Cmd.none)


view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "함께해요"
    , content = 
       if model.checkDevice == "pc" then
            web model
       else 
            div [] [appHeaderSearch "함께해요" "togetherHeader", 
            div [] [text "현재 준비 중입니다."]
            -- app model
            ]
    }

web model = 
    div [class "containerwrap"] [
                div [class "container"][
                    div [class "notification yf_workout"] [
                        commonHeader "../image/icon_together.png" "함께해요",
                        div [class "yf_yfworkout_search_wrap together"] [
                            tabbox model,
                            contentsBody
                        ]
                    ]
                ]
            ]

app model =
    div [class "container"] [
        appTab model,
        appStartBtn,
        div [] (List.map appContentsItem userContentsItem )
    ]

appTab model = 
        div [ class "m_to_menubox" ]
                  [ div [ classList [
                    ("m_together_yf_active" , model.isActive == "All")
                ], onClick (IsActive "All") ] 
                [ i [class "fab fa-amilia m_to_menubox_icon" ]
                    [], text "전체" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "people")
                ], onClick (IsActive "people") ] 
                [ i [class "fas fa-users m_to_menubox_icon" ]
                    [], text "피플" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "recipe")
                ], onClick (IsActive "recipe") ] 
                [ i [class "fas fa-utensils m_to_menubox_icon" ]
                    [], text "레시피" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "news")
                ], onClick (IsActive "news") ] 
                [ i [class "fas fa-file-alt m_to_menubox_icon" ]
                    [], text "뉴스" 
                ]
            , div [ classList [
                    ("m_together_yf_active" , model.isActive == "fitness")
                ], onClick (IsActive "fitness") ] 
                [ i [class "fas fa-dumbbell m_to_menubox_icon" ]
                    [], text "피트니스" 
                ]
            ]

            

appStartBtn = 
    div [ class "m_to_mediabox" ]
        [ div [ class "media-content m_to_yf_content" ]
            [ text "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요." , p []
                [ a [ class "button is-dark m_to_edit_btn", Route.href Route.TogetherW ]
                    [ i [ class "fas fa-edit" ]
                        []
                    ]
                ]
            ]
        ]
videoShow = 
    div [class"m_to_video"][
    img [ src "/image/dummy_video_image2.png" ]
        []
    ,div [ class "m_to_yf_more" ]
        [ div [ ]
            [ strong []
                [ text "자세히보기" ]
            ]
        ]
    ]

webvideoShow = 
          div [ class "to_yf_more" ]
            [ strong []
                [ text "자세히보기" ]
            ]
        
    


appContentsItem item =
    div [ class "m_to_mediabox2" ]
        [   if item.videoUrl == "" then
            span[] []
            else
           
            div [ class "m_to_yf_boxtop" ]
            [ p [ class "image is-128x128 " ]
                [ img [ src item.userImg]
                    []
                ]
            , div [ class "m_to_yf_id" ]
                [ strong []
                    [ text item.userId ]
                ]
            ], videoShow
        , div [ class "m_to_yf_text" ]
            [ text item.article ]
        , div [ class "level-left m_to_yf_like" ]
            [ text( "좋아요" ++ String.fromInt(item.isLike) ++"개" ) 
            , i [ class "far fa-heart together_heart"]
                []
            ]

        , div [ class "level-left m_to_yf_scrap" ]
            [ text( "스크랩" ++ String.fromInt(item.isLike) ++"개" ) 
            , i [ class "far fa-bookmark together_bookmark" ]
                []
            ]
        
        ]
tabbox model=
    div [ class "tapbox" ]
        [ div [ class "tabs is-toggle is-fullwidth is-large" ]
            [ ul []
                [ li [ classList [
                    ("together_yf_active" , model.isActive == "All")
                ], onClick (IsActive "All") ]
                    [ p []
                        [ span []
                            [ text "전체" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "people")
                ], onClick (IsActive "people")]
                    [ p []
                        [ span []
                            [ text "피플" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "recipe")
                ], onClick (IsActive "recipe")]
                    [ p []
                        [ span []
                            [ text "레시피" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "news")
                ], onClick (IsActive "news")]
                    [ p []
                        [ span []
                            [ text "뉴스" ]
                        ]
                    ]
                , li [ classList [
                    ("together_yf_active" , model.isActive == "fitness")
                ], onClick (IsActive "fitness")]
                    [ p []
                        [ span []
                            [ text "피트니스" ]
                        ]
                    ]
                ]
            ]
        ]


contentsBody =
    div [ class "together_searchbox_wrap" ]
        [ div [ class "together_searchbox" ]
            [ div [ class "together_mediabox" ]
                [ div [ class "media-content together_yf_content"]
                [ img [ src "image/takeimage.png", alt "takeimage" ]
                []
                ],
                   h1 [ class "to_yf_h2" ]
                   [ text "운동, 다이어트, 식단, 일상에 대한 대화를 나눠요!" ,
                     p []
                        [ a [ class "button is-dark together_edit_btn" , Route.href Route.TogetherW]
                            [ i [ class "fas fa-edit" ]
                                []
                            ]
                        ]
                    ]
                                 ]
            , div [](List.map userItem userContentsItem)
            ]
        ]
userItem item =
    div [ class "together_mediabox2" ]
        [ div [ class "together_yf_boxtop" ]
            [ p [ class "image is-64x64 together_imgbox" ]
                [ img [ src item.userImg ]
                    []
                ]
            , div [ class "together_yf_id" ]
                [ strong []
                    [ text item.userId ]
                ]
            ]
        , div [ class "together_yf_text" ]
            [ 
                if item.videoUrl=="" then
                    span [] []
                else
                    img [src item.videoUrl ] [] ,
                 text item.more, text item.article]
        , div [ class "level-left together_yf_like" ]
            [ text( "좋아요"++ String.fromInt (item.isLike) ++"개")  , br []
                []
            , i [ class "far fa-heart together_heart" ]
                []
            ]
        , div [ class "level-left together_yf_scrap" ]
            [ text( "스크랩"++ String.fromInt (item.isLike) ++"개")  , br []
                []
            , i [ class "far fa-bookmark together_bookmark" ]
                []
            ]
        ]

userContentsItem 
    = [
        {
            no = 1,
            article = "행복스럽고 않는 그들은 우리의 아름다우냐? 있으며, 남는 봄날의 타오르고 이것을 있으랴? 장식하는 청춘 보는 끓는 그림자는 듣는다. 황금시대를 예수는 얼음과 우리의 칼이다. 천자만홍이 같으며, 생생하며, 인간은 그들의 찾아 봄날의 이것이다. 것은 꽃 커다란 옷을 할지라도 무한한 오직 있으랴? 풍부하게 소금이라 많이 대중을 대고, 청춘의 길지 싶이 인도하겠다는 때문이다. 노래하며 희망의 바이며, 듣는다. 웅대한 듣기만 인간이 이 철환하였는가? 인생에 것은 청춘의 하였으며, 그리하였는가?",
            isLike = 3,
            videoUrl = "/image/dummy_video_image2.png",
            userId = "유어핏사용자",
            userImg = "https://bulma.io/images/placeholders/128x128.png",
            more="자세히보기"
        } ,
        {
            no = 2,
            article = "행복스럽고 않는 그들은 우리의 아름다우냐? 있으며, 남는 봄날의 타오르고 이것을 있으랴? 장식하는 청춘 보는 끓는 그림자는 듣는다. 황금시대를 예수는 얼음과 우리의 칼이다. 천자만홍이 같으며, 생생하며, 인간은 그들의 찾아 봄날의 이것이다. 것은 꽃 커다란 옷을 할지라도 무한한 오직 있으랴? 풍부하게 소금이라 많이 대중을 대고, 청춘의 길지 싶이 인도하겠다는 때문이다. 노래하며 희망의 바이며, 듣는다. 웅대한 듣기만 인간이 이 철환하였는가? 인생에 것은 청춘의 하였으며, 그리하였는가?",
            isLike = 3,
            videoUrl = "",
            userId = "유어핏사용자",
            userImg = "https://bulma.io/images/placeholders/128x128.png",
              more="자세히보기"
        } ,
        {
            no = 3,
            article = "행복스럽고 않는 그들은 우리의 아름다우냐? 있으며, 남는 봄날의 타오르고 이것을 있으랴? 장식하는 청춘 보는 끓는 그림자는 듣는다. 황금시대를 예수는 얼음과 우리의 칼이다. 천자만홍이 같으며, 생생하며, 인간은 그들의 찾아 봄날의 이것이다. 것은 꽃 커다란 옷을 할지라도 무한한 오직 있으랴? 풍부하게 소금이라 많이 대중을 대고, 청춘의 길지 싶이 인도하겠다는 때문이다. 노래하며 희망의 바이며, 듣는다. 웅대한 듣기만 인간이 이 철환하였는가? 인생에 것은 청춘의 하였으며, 그리하였는가?",
            isLike = 3,
            videoUrl = "/image/dummy_video_image2.png",
            userId = "유어핏사용자",
            userImg = "https://bulma.io/images/placeholders/128x128.png",
              more="자세히보기"
        } ,
        {
            no = 4,
            article = "행복스럽고 않는 그들은 우리의 아름다우냐? 있으며, 남는 봄날의 타오르고 이것을 있으랴? 장식하는 청춘 보는 끓는 그림자는 듣는다. 황금시대를 예수는 얼음과 우리의 칼이다. 천자만홍이 같으며, 생생하며, 인간은 그들의 찾아 봄날의 이것이다. 것은 꽃 커다란 옷을 할지라도 무한한 오직 있으랴? 풍부하게 소금이라 많이 대중을 대고, 청춘의 길지 싶이 인도하겠다는 때문이다. 노래하며 희망의 바이며, 듣는다. 웅대한 듣기만 인간이 이 철환하였는가? 인생에 것은 청춘의 하였으며, 그리하였는가?",
            isLike = 3,
            videoUrl = "",            
            userId = "유어핏사용자",
            userImg = "https://bulma.io/images/placeholders/128x128.png",
             more="자세히보기"
        } 
    ]