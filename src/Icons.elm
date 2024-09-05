module Icons exposing (..)

import Element exposing (Element, el)
import Phosphor


kill : Element msg
kill =
    icon Phosphor.skull


heal : Element msg
heal =
    icon Phosphor.heart


shield : Element msg
shield =
    icon Phosphor.shield


neutral : Element msg
neutral =
    icon Phosphor.smileyBlank


icon : Phosphor.Icon -> Element msg
icon v =
    v Phosphor.Duotone
        |> Phosphor.toHtml []
        |> Element.html
        |> el []
