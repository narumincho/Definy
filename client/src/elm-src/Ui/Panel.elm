module Ui.Panel exposing
    ( FitStyle(..)
    , FixFix(..)
    , FixGrow(..)
    , GrowFix(..)
    , GrowGrow(..)
    , HorizontalAlignment
    , ImageRendering(..)
    , VerticalAlignment
    , bottom
    , centerX
    , centerY
    , left
    , right
    , top
    )

import Color
import Css
import Html.Styled
import Html.Styled.Attributes


{-| 横方向と縦方向で揃える位置が必要なパネル
-}
type FixFix msg
    = FixFixFromGrowGrow
        { width : Int
        , height : Int
        , growGrow : GrowGrow msg
        }


{-| 幅は中身によって、高さは外の大きさによって決まるパネル
-}
type FixGrow msg
    = FixGrowFromFixFix
        { horizontalAlignment : HorizontalAlignment
        , fixFix : FixFix msg
        }
    | FixGrowFromGrowGrow
        { width : Int
        , growGrow : GrowGrow msg
        }


{-| 幅は外の大きさによって、高さは中身によって決まるパネル
-}
type GrowFix msg
    = GrowFixFromFixFix
        { verticalAlignment : VerticalAlignment
        , fixFix : FixFix msg
        }
    | GrowFixFromGrowGrow
        { height : Int
        , growGrow : GrowGrow msg
        }


{-| 幅と高さが外の大きさによってきまるパネル
-}
type GrowGrow msg
    = FromFixFix
        { horizontalAlignment : HorizontalAlignment
        , verticalAlignment : VerticalAlignment
        , fixFix : FixFix msg
        }
    | FromFixGrow
        { horizontalAlignment : HorizontalAlignment
        , fixGrow : FixGrow msg
        }
    | FromGrowFix
        { verticalAlignment : VerticalAlignment
        , growFix : GrowFix msg
        }
    | Box
        { padding : Int
        , border : Border
        , color : Color.Color
        }
    | Text
        { textAlign : TextAlign
        , verticalAlignment : VerticalAlignment
        , text : String
        , size : Float
        , color : Color.Color
        }
    | ImageFromDataUrl
        { dataUrl : String
        , fitStyle : FitStyle
        , alternativeText : String
        , rendering : ImageRendering
        }


type FitStyle
    = Contain
    | Cover


type ImageRendering
    = ImageRenderingPixelated
    | ImageRenderingAuto


type Border
    = Border
        { top : BorderStyle
        , right : BorderStyle
        , left : BorderStyle
        , bottom : BorderStyle
        }


type BorderStyle
    = BorderStyle
        { color : Color.Color
        , width : Int
        }


type TextAlign
    = TextAlignStart
    | TextAlignEnd
    | TextAlignCenter
    | TextAlignJustify


growGrowToHtml : GrowGrow msg -> Html.Styled.Html msg
growGrowToHtml growGrow =
    case growGrow of
        FromFixFix { horizontalAlignment, verticalAlignment, fixFix } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.displayFlex
                    , horizontalAlignmentToStyle horizontalAlignment
                    , verticalAlignmentToStyle verticalAlignment
                    ]
                ]
                [ fixFixToHtml fixFix ]

        FromFixGrow { horizontalAlignment, fixGrow } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.displayFlex
                    , horizontalAlignmentToStyle horizontalAlignment
                    ]
                ]
                [ fixGrowToHtml fixGrow ]

        FromGrowFix { verticalAlignment, growFix } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.displayFlex
                    , verticalAlignmentToStyle verticalAlignment
                    ]
                ]
                [ growFixToHtml growFix ]

        Box { padding, border, color } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.color (Css.hex (Color.toRGBString color))
                    ]
                ]
                []

        Text { textAlign, verticalAlignment, text, color } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.textAlign
                        (case textAlign of
                            TextAlignStart ->
                                Css.start

                            TextAlignEnd ->
                                Css.end

                            TextAlignCenter ->
                                Css.center

                            TextAlignJustify ->
                                Css.justify
                        )
                    , verticalAlignmentToStyle verticalAlignment
                    , Css.color (Css.hex (Color.toHex color))
                    , Css.overflowWrap Css.breakWord
                    ]
                ]
                [ Html.Styled.text text ]

        ImageFromDataUrl { dataUrl, fitStyle, alternativeText, rendering } ->
            Html.Styled.img
                [ Html.Styled.Attributes.css
                    ([ Css.width (Css.pct 100)
                     , Css.height (Css.pct 100)
                     , Css.property "object-fit"
                        (case fitStyle of
                            Contain ->
                                "contain"

                            Cover ->
                                "cover"
                        )
                     , Css.display Css.block
                     ]
                        ++ (case rendering of
                                ImageRenderingAuto ->
                                    []

                                ImageRenderingPixelated ->
                                    [ Css.property "image-rendering" "pixelated" ]
                           )
                    )
                , Html.Styled.Attributes.src
                    (if String.startsWith "data:" dataUrl then
                        dataUrl

                     else
                        ""
                    )
                , Html.Styled.Attributes.alt alternativeText
                ]
                []


fixFixToHtml : FixFix msg -> Html.Styled.Html msg
fixFixToHtml fixFix =
    case fixFix of
        FixFixFromGrowGrow { width, height, growGrow } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.px (toFloat width))
                    , Css.height (Css.px (toFloat height))
                    ]
                ]
                [ growGrowToHtml growGrow ]


fixGrowToHtml : FixGrow msg -> Html.Styled.Html msg
fixGrowToHtml fixGrow =
    case fixGrow of
        FixGrowFromFixFix { horizontalAlignment, fixFix } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ horizontalAlignmentToStyle horizontalAlignment
                    , Css.height (Css.pct 100)
                    ]
                ]
                [ fixFixToHtml fixFix ]

        FixGrowFromGrowGrow { width, growGrow } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.px (toFloat width))
                    , Css.height (Css.pct 100)
                    ]
                ]
                [ growGrowToHtml growGrow ]


growFixToHtml : GrowFix msg -> Html.Styled.Html msg
growFixToHtml growFix =
    case growFix of
        GrowFixFromFixFix { verticalAlignment, fixFix } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ verticalAlignmentToStyle verticalAlignment
                    , Css.width (Css.pct 100)
                    ]
                ]
                [ fixFixToHtml fixFix ]

        GrowFixFromGrowGrow { height, growGrow } ->
            Html.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.width (Css.pct 100)
                    , Css.height (Css.px (toFloat height))
                    ]
                ]
                [ growGrowToHtml growGrow ]


{-| 横方向のそろえ方
-}
type HorizontalAlignment
    = Left
    | CenterX
    | Right


{-| 横方向のそろえ方。左によせる
-}
left : HorizontalAlignment
left =
    Left


{-| 横方向のそろえ方。中央にそろえる
-}
centerX : HorizontalAlignment
centerX =
    CenterX


{-| 横方向のそろえ方。右によせる
-}
right : HorizontalAlignment
right =
    Right


{-| 縦のそろえ方
-}
type VerticalAlignment
    = Top
    | CenterY
    | Bottom


{-| 縦方向のそろえ方。上によせる
-}
top : VerticalAlignment
top =
    Top


{-| 縦方向のそろえ方。中央にそろえる
-}
centerY : VerticalAlignment
centerY =
    CenterY


{-| 縦方向のそろえ方。下によせる
-}
bottom : VerticalAlignment
bottom =
    Bottom


{-| 表示領域と表示幅と水平の揃え方からX座標を求める
-}
horizontalAlignmentToStyle : HorizontalAlignment -> Css.Style
horizontalAlignmentToStyle horizontalAlignment =
    Css.justifyContent
        (case horizontalAlignment of
            Left ->
                Css.start

            CenterX ->
                Css.center

            Right ->
                Css.right
        )


{-| 表示領域と表示高さと垂直の揃え方からY座標を求める
-}
verticalAlignmentToStyle : VerticalAlignment -> Css.Style
verticalAlignmentToStyle verticalAlignment =
    Css.alignItems
        (case verticalAlignment of
            Top ->
                Css.flexStart

            CenterY ->
                Css.center

            Bottom ->
                Css.flexEnd
        )
