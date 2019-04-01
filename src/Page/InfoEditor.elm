module Page.InfoEditor exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Json.Encode as JE
import Markdown.Block as Block exposing (Block)
import Markdown.Config exposing (HtmlOption(..),  defaultSanitizeOptions)
import Markdown.Inline as Inline
import Regex exposing (Regex)


-- infoArticle model textAreaInput readOnly title linkUrl msg =
--     div [ class "content", id "info" ]
--         [ 
--             columnsHtml [
--             pageTitle title
--             ],
--             columnsHtml [
--                 formInputEvent "제목" "제목을 입력 해 주세요" False msg model.title
--             ],
--             columnsHtml [
--                 labelWrap "HTML 입력",
--                 labelWrap "미리보기"
--             ],
--             columnsHtml [
--                 div [ class "editorArea" ]
--                 [ 
--                     editorView model.textarea textAreaInput readOnly
--                 ],
--                 div [ class "previewStyle" ]
--                 [ 
--                 div [class "backgroundText"] []
--                 ,
--                 markdownView model
--                 ]
--             ]
--         --     ,    
--         --  linkUrl
--         ]

-- infoDetail model textAreaInput readOnly title linkUrl msg titleModel contents=
--     div [ class "content" ]
--         [ 
--             columnsHtml [
--             pageTitle title
--             ],
--             columnsHtml [
--                 formInputEvent "제목" "제목을 입력 해 주세요" readOnly msg titleModel
--             ],
--             columnsHtml [
--                 labelWrap "HTML 입력",
--                 labelWrap "미리보기"
--             ],
--             columnsHtml [
--                 div [ class "editorArea" ]
--                 [ 
--                     editorView model.textarea textAreaInput readOnly
--                 ],
--                 div [ class "previewStyle" ]
--                 [ 
--                 div [class "backgroundText"] []
--                 ,
--                 markdownView model
--                 ]
--             ]

--         --  linkUrl
--         ]

-- editorView : String -> msg  -> Html msg
editorView md textAreaInput readOnly=
        textarea
            [ onInput textAreaInput
            , property "defaultValue" (JE.string md)
            , class "editor editorStyle"
            , spellcheck False
            , disabled readOnly
            , placeholder "내용을 입력 해 주세요."
            ]
            []

-- detailmarkdownView { options, textarea, onDemandText, showToC, selectedPreviewTab }detailText =
--     let
--         textToParse =
--                 detailText
--     in
--     Html.Lazy.lazy3 render options showToC textToParse
-- markdownView : String -> Html msg
markdownView { options, textarea, onDemandText, showToC, selectedPreviewTab } =
    let
        textToParse =
                textarea
    in
    Html.Lazy.lazy3 render options showToC textToParse


render : Markdown.Config.Options -> Bool -> String -> Html msg
render options showToC md =
    let
        blocks =
            Block.parse (Just options) md

        blocksView =
            List.concatMap customHtmlBlock blocks
    in
    if showToC then
        blocksView
            |> (::) (tocView blocks)
            |> div [ class "preview" ]

    else
        div [ class "preview" ] blocksView



-- Heading Link


customHtmlBlock : Block b i -> List (Html msg)
customHtmlBlock block =
    case block of
        Block.Heading _ level inlines ->
            let
                hElement =
                    case level of
                        1 ->
                            h1

                        2 ->
                            h2

                        3 ->
                            h3

                        4 ->
                            h4

                        5 ->
                            h5

                        _ ->
                            h6
            in
            [ hElement
                [ Html.Attributes.id
                    (formatToCLink
                        (Inline.extractText inlines)
                    )
                ]
                (List.map Inline.toHtml inlines)
            ]

        _ ->
            Block.defaultHtml
                (Just customHtmlBlock)
                Nothing
                block



tocView : List (Block b i) -> Html msg
tocView =
    List.concatMap (Block.query getHeading)
        >> List.foldl organizeHeadings []
        >> List.reverse
        >> List.map reverseToCItem
        >> tocViewHelp
        >> (\a -> (::) a [])
        >> (::) (h1 [] [ text "Table of Content" ])
        >> div []


getHeading : Block b i -> List ( Int, String )
getHeading block =
    case block of
        Block.Heading _ lvl inlines ->
            [ ( lvl, Inline.extractText inlines ) ]

        _ ->
            []


type ToCItem
    = Item Int String (List ToCItem)


organizeHeadings : ( Int, String ) -> List ToCItem -> List ToCItem
organizeHeadings ( lvl, str ) items =
    case items of
        [] ->
            [ Item lvl str [] ]

        (Item lvl_ str_ items_) :: tail ->
            if lvl <= lvl_ then
                Item lvl str [] :: items

            else
                organizeHeadings ( lvl, str ) items_
                    |> Item lvl_ str_
                    |> (\a -> (::) a tail)


reverseToCItem : ToCItem -> ToCItem
reverseToCItem (Item lvl heading subHeadings) =
    List.reverse subHeadings
        |> List.map reverseToCItem
        |> Item lvl heading


tocViewHelp : List ToCItem -> Html msg
tocViewHelp =
    List.map tocItemView
        >> ul []


tocItemView : ToCItem -> Html msg
tocItemView (Item lvl heading subHeadings) =
    if List.isEmpty subHeadings then
        li [] [ tocLinkView heading ]

    else
        li []
            [ tocLinkView heading
            , tocViewHelp subHeadings
            ]


tocLinkView : String -> Html msg
tocLinkView str =
    a
        [ formatToCLink str
            |> (++) "#"
            |> Html.Attributes.href
        ]
        [ text str ]


formatToCLink : String -> String
formatToCLink =
    String.toLower
        >> Regex.replace oneOrMoreSpaces (always "-")


oneOrMoreSpaces : Regex
oneOrMoreSpaces =
    Regex.fromString "\\s+"
        |> Maybe.withDefault Regex.never




