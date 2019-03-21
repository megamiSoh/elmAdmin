module Page.Blank exposing (view)

import Html exposing (Html)


view : { title : String, content : Html msg, check : String }
view =
    { check="",
        title = ""
    , content = Html.text ""
    }