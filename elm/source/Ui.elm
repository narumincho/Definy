module Ui exposing
    ( AlignItems(..)
    , Content
    , FitStyle(..)
    , Font(..)
    , ImageRendering(..)
    , Panel
    , Pointer
    , PointerImage(..)
    , TextAlignment(..)
    , border
    , borderRadius
    , column
    , depth
    , empty
    , height
    , imageFromUrl
    , map
    , offset
    , onClick
    , onPointerDown
    , onPointerEnter
    , onPointerLeave
    , onPointerMove
    , overflowVisible
    , padding
    , pointerGetPosition
    , pointerImage
    , row
    , textBox
    , textBoxFitHeight
    , toHtml
    , vectorImage
    , width
    )

import Bitwise
import Css
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import Json.Decode.Pipeline
import Utility.ListExtra
import VectorImage


{-| 幅と高さが外の大きさによってきまるパネル
-}
type Panel message
    = Panel
        { styleAndEvent : StyleAndEventComputed message
        , content : Content message
        }


{-| ポインターのイベントで受け取れるマウスの状態
-}
type Pointer
    = Pointer
        { id : Int -- 識別するためのID
        , position : ( Float, Float ) -- 要素内での位置
        , button : Maybe PointerButton -- 押したボタン。Nothingは最初のイベント以降で変更がなかったとき
        , pressure : Float -- 圧力 0～1
        , tangentialPressure : Float
        , size : ( Float, Float )
        , tilt : ( Float, Float )
        , twist : Float
        , type_ : PointerType
        , isPrimary : Bool
        , buttons : List PointerButton
        , target : Bool
        }


type PointerType
    = Mouse
    | Pen
    | Touch
    | PointerTypeUnknown String


type PointerButton
    = Primary
    | Secondary
    | Auxiliary
    | BrowserBack
    | BrowserForward
    | Eraser


{-| スタイルとイベント
-}
type alias StyleAndEventComputed message =
    { padding : Int
    , width : Maybe Int
    , height : Maybe Int
    , offset : Maybe ( Int, Int )
    , overflowVisible : Bool
    , pointerImage : Maybe PointerImage
    , border : Maybe BorderStyle
    , borderRadius : Int
    , backGroundColor : Maybe Css.Color
    , click : Maybe message
    , pointerEnter : Maybe (Pointer -> message)
    , pointerLeave : Maybe (Pointer -> message)
    , pointerMove : Maybe (Pointer -> message)
    , pointerDown : Maybe (Pointer -> message)
    }


type StyleAndEvent message
    = Padding Int
    | Width Int
    | Height Int
    | Offset ( Int, Int )
    | OverflowVisible
    | PointerImage PointerImage
    | Border BorderStyle
    | BorderRadius Int
    | OnClick message
    | OnPointerEnter (Pointer -> message)
    | OnPointerLeave (Pointer -> message)
    | OnPointerMove (Pointer -> message)
    | OnPointerDown (Pointer -> message)


type PointerImage
    = HorizontalResize -- ew-resize ↔
    | VerticalResize -- ns-resize ↕


type Content msg
    = TextBox
        { text : String
        , font : Font
        , verticalAlignment : AlignItems
        , textAlignment : TextAlignment
        }
    | TextBoxFitHeight
        { text : String
        , font : Font
        , textAlignment : TextAlignment
        }
    | ImageFromUrl
        { url : String
        , fitStyle : FitStyle
        , alternativeText : String
        , rendering : ImageRendering
        }
    | VectorImage
        { fitStyle : FitStyle
        , viewBox : { x : Int, y : Int, width : Int, height : Int }
        , elements : List (VectorImage.Element msg)
        }
    | Monochromatic Css.Color
    | Empty
    | DepthList (List (Panel msg))
    | RowList (List (Panel msg)) Int
    | ColumnList (List (Panel msg)) Int


type FitStyle
    = Contain
    | Cover


type ImageRendering
    = ImageRenderingPixelated
    | ImageRenderingAuto


type BorderStyle
    = BorderStyle
        { color : Css.Color
        , width :
            { top : Int
            , right : Int
            , left : Int
            , bottom : Int
            }
        }


type TextAlignment
    = TextAlignStart
    | TextAlignEnd
    | TextAlignCenter
    | TextAlignJustify


type Font
    = Font
        { typeface : String
        , size : Int
        , letterSpacing : Float
        , color : Css.Color
        }


toHtml : Panel msg -> Html.Styled.Html msg
toHtml =
    panelToHtml (ChildrenStyle { gridPositionLeftTop = False, position = Nothing })


padding : Int -> StyleAndEvent message
padding =
    Padding


width : Int -> StyleAndEvent message
width =
    Width


height : Int -> StyleAndEvent message
height =
    Height


offset : ( Int, Int ) -> StyleAndEvent message
offset =
    Offset


overflowVisible : StyleAndEvent message
overflowVisible =
    OverflowVisible


pointerImage : PointerImage -> StyleAndEvent message
pointerImage =
    PointerImage


border : BorderStyle -> StyleAndEvent message
border =
    Border


borderRadius : Int -> StyleAndEvent message
borderRadius =
    BorderRadius


onClick : message -> StyleAndEvent message
onClick =
    OnClick


onPointerEnter : (Pointer -> message) -> StyleAndEvent message
onPointerEnter =
    OnPointerEnter


onPointerLeave : (Pointer -> message) -> StyleAndEvent message
onPointerLeave =
    OnPointerLeave


onPointerMove : (Pointer -> message) -> StyleAndEvent message
onPointerMove =
    OnPointerMove


onPointerDown : (Pointer -> message) -> StyleAndEvent message
onPointerDown =
    OnPointerDown


{-| テキストを表示する
-}
textBox :
    StyleAndEventComputed message
    ->
        { align : TextAlignment
        , vertical : AlignItems
        , font : Font
        }
    -> String
    -> Panel message
textBox styleAndEvent { align, vertical, font } text =
    Panel
        { styleAndEvent = styleAndEvent
        , content =
            TextBox
                { font = font
                , text = text
                , verticalAlignment = vertical
                , textAlignment = align
                }
        }


{-| テキストを表示する
高さがフォントや中身の文字に応じて変化する
-}
textBoxFitHeight : StyleAndEventComputed message -> { align : TextAlignment, font : Font } -> String -> Panel message
textBoxFitHeight styleAndEvent { align, font } text =
    Panel
        { styleAndEvent = styleAndEvent
        , content =
            TextBoxFitHeight
                { font = font
                , text = text
                , textAlignment = align
                }
        }


imageFromUrl :
    StyleAndEventComputed message
    ->
        { fitStyle : FitStyle
        , alternativeText : String
        , rendering : ImageRendering
        }
    -> String
    -> Panel message
imageFromUrl styleAndEvent imageStyle url =
    Panel
        { styleAndEvent = styleAndEvent
        , content =
            ImageFromUrl
                { url = url
                , fitStyle = imageStyle.fitStyle
                , alternativeText = imageStyle.alternativeText
                , rendering = imageStyle.rendering
                }
        }


vectorImage :
    StyleAndEventComputed message
    -> { fitStyle : FitStyle, viewBox : { x : Int, y : Int, width : Int, height : Int }, elements : List (VectorImage.Element message) }
    -> Panel message
vectorImage styleAndEvent content =
    Panel
        { styleAndEvent = styleAndEvent
        , content =
            VectorImage content
        }


empty : StyleAndEventComputed message -> Panel message
empty styleAndEvent =
    Panel
        { styleAndEvent = styleAndEvent
        , content = Empty
        }


depth : StyleAndEventComputed message -> List (Panel message) -> Panel message
depth styleAndEvent children =
    Panel
        { styleAndEvent = styleAndEvent
        , content = DepthList children
        }


row : StyleAndEventComputed message -> Int -> List (Panel message) -> Panel message
row styleAndEvent gap children =
    Panel
        { styleAndEvent = styleAndEvent
        , content = RowList children gap
        }


column : StyleAndEventComputed message -> Int -> List (Panel message) -> Panel message
column styleAndEvent gap children =
    Panel
        { styleAndEvent = styleAndEvent
        , content = ColumnList children gap
        }


defaultStyleAndEvent : StyleAndEventComputed message
defaultStyleAndEvent =
    { padding = 0
    , width = Nothing
    , height = Nothing
    , offset = Nothing
    , overflowVisible = False
    , pointerImage = Nothing
    , border = Nothing
    , borderRadius = 0
    , backGroundColor = Nothing
    , click = Nothing
    , pointerEnter = Nothing
    , pointerLeave = Nothing
    , pointerMove = Nothing
    , pointerDown = Nothing
    }


map : (a -> b) -> Panel a -> Panel b
map func (Panel { styleAndEvent, content }) =
    Panel
        { styleAndEvent = styleAndEventMap func styleAndEvent
        , content =
            case content of
                TextBox rec ->
                    TextBox rec

                TextBoxFitHeight rec ->
                    TextBoxFitHeight rec

                ImageFromUrl rec ->
                    ImageFromUrl rec

                VectorImage { fitStyle, viewBox, elements } ->
                    VectorImage
                        { fitStyle = fitStyle
                        , viewBox = viewBox
                        , elements = elements |> List.map (VectorImage.map func)
                        }

                Monochromatic color ->
                    Monochromatic color

                Empty ->
                    Empty

                DepthList list ->
                    DepthList (list |> List.map (map func))

                RowList list gap ->
                    RowList
                        (list |> List.map (map func))
                        gap

                ColumnList list gap ->
                    ColumnList
                        (list |> List.map (map func))
                        gap
        }


styleAndEventMap : (a -> b) -> StyleAndEventComputed a -> StyleAndEventComputed b
styleAndEventMap func styleAndEvent =
    { padding = styleAndEvent.padding
    , width = styleAndEvent.width
    , height = styleAndEvent.height
    , offset = styleAndEvent.offset
    , overflowVisible = styleAndEvent.overflowVisible
    , pointerImage = styleAndEvent.pointerImage
    , border = styleAndEvent.border
    , borderRadius = styleAndEvent.borderRadius
    , backGroundColor = styleAndEvent.backGroundColor
    , click = Maybe.map func styleAndEvent.click
    , pointerEnter = Maybe.map (\msgFunc pointer -> msgFunc pointer |> func) styleAndEvent.pointerEnter
    , pointerLeave = Maybe.map (\msgFunc pointer -> msgFunc pointer |> func) styleAndEvent.pointerLeave
    , pointerMove = Maybe.map (\msgFunc pointer -> msgFunc pointer |> func) styleAndEvent.pointerLeave
    , pointerDown = Maybe.map (\msgFunc pointer -> msgFunc pointer |> func) styleAndEvent.pointerDown
    }


type ChildrenStyle
    = ChildrenStyle
        { gridPositionLeftTop : Bool
        , position : Maybe ( Int, Int )
        }


panelToHtml : ChildrenStyle -> Panel msg -> Html.Styled.Html msg
panelToHtml childrenStyle panel =
    let
        (Panel { styleAndEvent, content }) =
            panel
    in
    panelToHtmlElementType
        panel
        ([ Html.Styled.Attributes.css [ panelToStyle styleAndEvent childrenStyle content ] ]
            ++ growGrowContentToListAttributes content
            ++ eventsToHtmlAttributes styleAndEvent
        )
        (contentToListHtml content)


panelToHtmlElementType :
    Panel msg
    -> List (Html.Styled.Attribute msg)
    -> List (Html.Styled.Html msg)
    -> Html.Styled.Html msg
panelToHtmlElementType (Panel { styleAndEvent, content }) =
    case content of
        ImageFromUrl _ ->
            Html.Styled.img

        _ ->
            case styleAndEvent.click of
                Just _ ->
                    -- Button要素がdisplay:gridできないバグへの対処も含めての入れ子 https://stackoverflow.com/questions/51815477/is-a-button-allowed-to-have-displaygrid
                    \attributes children ->
                        Html.Styled.button
                            [ Html.Styled.Attributes.css
                                [ Css.width (Css.pct 100)
                                , Css.height (Css.pct 100)
                                , Css.padding Css.zero
                                , Css.border2 Css.zero Css.none
                                , Css.backgroundColor Css.transparent
                                , Css.cursor Css.pointer
                                ]
                            ]
                            [ Html.Styled.div attributes children ]

                Nothing ->
                    Html.Styled.div


panelToStyle : StyleAndEventComputed message -> ChildrenStyle -> Content message -> Css.Style
panelToStyle styleAndEvent childrenStyle content =
    [ [ Css.padding (Css.px (toFloat styleAndEvent.padding))
      , contentToStyle
            childrenStyle
            styleAndEvent.overflowVisible
            content
      ]
    , case styleAndEvent.border of
        Just border_ ->
            [ borderStyleToStyle border_ ]

        Nothing ->
            []
    , case styleAndEvent.width of
        Just width_ ->
            [ Css.width (Css.px (toFloat width_)) ]

        Nothing ->
            []
    , case styleAndEvent.height of
        Just height_ ->
            [ Css.height (Css.px (toFloat height_)) ]

        Nothing ->
            []
    , case styleAndEvent.offset of
        Just ( left, top ) ->
            [ Css.transform
                (Css.translate2
                    (Css.px (toFloat left))
                    (Css.px (toFloat top))
                )
            ]

        Nothing ->
            []
    , case styleAndEvent.pointerImage of
        Just HorizontalResize ->
            [ Css.cursor Css.ewResize ]

        Just VerticalResize ->
            [ Css.cursor Css.ewResize ]

        Nothing ->
            []
    , case styleAndEvent.borderRadius of
        0 ->
            []

        _ ->
            [ Css.borderRadius (Css.px (toFloat styleAndEvent.borderRadius)) ]
    , case styleAndEvent.backGroundColor of
        Just color ->
            [ Css.backgroundColor color ]

        Nothing ->
            []
    ]
        |> List.concat
        |> Css.batch


borderStyleToStyle : BorderStyle -> Css.Style
borderStyleToStyle (BorderStyle record) =
    [ Css.borderColor record.color
    , Css.borderWidth4
        (Css.px (toFloat record.width.top))
        (Css.px (toFloat record.width.right))
        (Css.px (toFloat record.width.bottom))
        (Css.px (toFloat record.width.left))
    ]
        |> Css.batch


contentToStyle : ChildrenStyle -> Bool -> Content msg -> Css.Style
contentToStyle (ChildrenStyle { gridPositionLeftTop, position }) overflowVisible_ content =
    (case content of
        TextBox rec ->
            let
                (Font { typeface, size, letterSpacing, color }) =
                    rec.font
            in
            [ Css.property "display" "grid"
            , Css.color color
            , Css.fontSize (Css.px (toFloat size))
            , Css.fontFamilies [ Css.qt typeface ]
            , Css.letterSpacing (Css.px letterSpacing)
            , Css.overflowWrap Css.breakWord
            , textAlignToStyle rec.textAlignment
            , alignItemsToStyle rec.verticalAlignment
            ]

        TextBoxFitHeight rec ->
            let
                (Font { typeface, size, letterSpacing, color }) =
                    rec.font
            in
            [ Css.property "display" "grid"
            , Css.color color
            , Css.fontSize (Css.px (toFloat size))
            , Css.fontFamilies [ Css.qt typeface ]
            , Css.letterSpacing (Css.px letterSpacing)
            , Css.overflowWrap Css.breakWord
            , textAlignToStyle rec.textAlignment
            ]

        ImageFromUrl { fitStyle, rendering } ->
            [ Css.property "object-fit"
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

        VectorImage _ ->
            []

        Monochromatic color ->
            [ Css.backgroundColor color ]

        Empty ->
            []

        DepthList _ ->
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" "1fr"
            , Css.property "grid-template-columns" "1fr"
            ]

        RowList list gap ->
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" (panelListToGridTemplateColumns list)
            , Css.property "gap" (String.fromInt gap ++ "px")
            ]

        ColumnList list gap ->
            [ Css.property "display" "grid"
            , Css.property "grid-template-rows" (panelListToGridTemplateRows list)
            , Css.property "gap" (String.fromInt gap ++ "px")
            ]
    )
        ++ (if overflowVisible_ then
                []

            else
                [ Css.overflow Css.hidden ]
           )
        ++ (if gridPositionLeftTop then
                [ gridSetPosition ]

            else
                []
           )
        ++ (case position of
                Just ( left, top ) ->
                    [ Css.left (Css.px (toFloat left))
                    , Css.top (Css.px (toFloat top))
                    ]

                Nothing ->
                    []
           )
        |> Css.batch


{-| コンテンツをHTMLの属性に変換する
-}
growGrowContentToListAttributes : Content msg -> List (Html.Styled.Attribute msg)
growGrowContentToListAttributes content =
    case content of
        TextBox _ ->
            []

        TextBoxFitHeight _ ->
            []

        ImageFromUrl { url, alternativeText } ->
            [ Html.Styled.Attributes.src url
            , Html.Styled.Attributes.alt alternativeText
            ]

        VectorImage _ ->
            []

        Monochromatic _ ->
            []

        Empty ->
            []

        DepthList _ ->
            []

        RowList _ _ ->
            []

        ColumnList _ _ ->
            []


{-| イベントをElmのイベントリスナーの属性に変換する
-}
eventsToHtmlAttributes : StyleAndEventComputed msg -> List (Html.Styled.Attribute msg)
eventsToHtmlAttributes styleAndEvent =
    [ styleAndEvent.pointerEnter
        |> Maybe.map (\msg -> Html.Styled.Events.on "pointerenter" (pointerEventDecoder |> Json.Decode.map msg))
    , styleAndEvent.pointerLeave
        |> Maybe.map (\msg -> Html.Styled.Events.on "pointerleave" (pointerEventDecoder |> Json.Decode.map msg))
    , styleAndEvent.pointerMove
        |> Maybe.map (\msg -> Html.Styled.Events.on "pointermove" (pointerEventDecoder |> Json.Decode.map msg))
    , styleAndEvent.pointerDown
        |> Maybe.map (\msg -> Html.Styled.Events.on "pointerdown" (pointerEventDecoder |> Json.Decode.map msg))
    , styleAndEvent.click
        |> Maybe.map (\msg -> Html.Styled.Events.onClick msg)
    ]
        |> Utility.ListExtra.takeFromMaybeList


pointerEventDecoder : Json.Decode.Decoder Pointer
pointerEventDecoder =
    Json.Decode.succeed
        (\id clientX clientY button pressure tangentialPressure width_ height_ tiltX tiltY twist pointerType isPrimary buttons eventPhase ->
            Pointer
                { id = id
                , position = ( clientX, clientY )
                , button =
                    case button of
                        0 ->
                            Just Primary

                        1 ->
                            Just Auxiliary

                        2 ->
                            Just Secondary

                        3 ->
                            Just BrowserBack

                        4 ->
                            Just BrowserForward

                        5 ->
                            Just Eraser

                        _ ->
                            Nothing
                , pressure = pressure
                , tangentialPressure = tangentialPressure
                , size = ( width_, height_ )
                , tilt = ( tiltX, tiltY )
                , twist = twist
                , type_ =
                    case pointerType of
                        "mouse" ->
                            Mouse

                        "pen" ->
                            Pen

                        "touch" ->
                            Touch

                        _ ->
                            PointerTypeUnknown pointerType
                , isPrimary = isPrimary
                , buttons =
                    [ ( Primary, (buttons |> Bitwise.and 1) /= 0 )
                    , ( Secondary, (buttons |> Bitwise.and 2) /= 0 )
                    , ( Auxiliary, (buttons |> Bitwise.and 4) /= 0 )
                    , ( BrowserBack, (buttons |> Bitwise.and 8) /= 0 )
                    , ( BrowserForward, (buttons |> Bitwise.and 16) /= 0 )
                    , ( Eraser, (buttons |> Bitwise.and 32) /= 0 )
                    ]
                        |> List.filter Tuple.second
                        |> List.map Tuple.first
                , target = eventPhase == 2
                }
        )
        |> Json.Decode.Pipeline.required "pointerId" Json.Decode.int
        |> Json.Decode.Pipeline.required "clientX" Json.Decode.float
        |> Json.Decode.Pipeline.required "clientY" Json.Decode.float
        |> Json.Decode.Pipeline.required "button" Json.Decode.int
        |> Json.Decode.Pipeline.required "pressure" Json.Decode.float
        |> Json.Decode.Pipeline.required "tangentialPressure" Json.Decode.float
        |> Json.Decode.Pipeline.required "width" Json.Decode.float
        |> Json.Decode.Pipeline.required "height" Json.Decode.float
        |> Json.Decode.Pipeline.required "tiltX" Json.Decode.float
        |> Json.Decode.Pipeline.required "tiltY" Json.Decode.float
        |> Json.Decode.Pipeline.required "twist" Json.Decode.float
        |> Json.Decode.Pipeline.required "pointerType" Json.Decode.string
        |> Json.Decode.Pipeline.required "isPrimary" Json.Decode.bool
        |> Json.Decode.Pipeline.required "buttons" Json.Decode.int
        |> Json.Decode.Pipeline.required "eventPhase" Json.Decode.int


contentToListHtml : Content msg -> List (Html.Styled.Html msg)
contentToListHtml content =
    case content of
        TextBox rec ->
            [ Html.Styled.text rec.text ]

        TextBoxFitHeight rec ->
            [ Html.Styled.text rec.text ]

        ImageFromUrl _ ->
            []

        VectorImage { elements, viewBox } ->
            [ elements
                |> VectorImage.toHtml viewBox Nothing
            ]

        Monochromatic _ ->
            []

        Empty ->
            []

        DepthList list ->
            list
                |> List.map
                    (panelToHtml
                        (ChildrenStyle
                            { gridPositionLeftTop = True
                            , position = Nothing
                            }
                        )
                    )

        RowList list _ ->
            list
                |> List.map
                    (panelToHtml
                        (ChildrenStyle
                            { gridPositionLeftTop = False
                            , position = Nothing
                            }
                        )
                    )

        ColumnList list _ ->
            list
                |> List.map
                    (panelToHtml
                        (ChildrenStyle
                            { gridPositionLeftTop = False
                            , position = Nothing
                            }
                        )
                    )


panelListToGridTemplateColumns : List (Panel msg) -> String
panelListToGridTemplateColumns =
    List.map
        (panelToGrowOrFixAutoWidth >> growOrFixAutoToGridTemplateString)
        >> String.join " "


panelListToGridTemplateRows : List (Panel msg) -> String
panelListToGridTemplateRows =
    List.map
        (panelToGrowOrFixAutoHeight >> growOrFixAutoToGridTemplateString)
        >> String.join " "


type GrowOrFixAuto
    = Grow
    | FixMaxContent


growOrFixAutoToGridTemplateString : GrowOrFixAuto -> String
growOrFixAutoToGridTemplateString growOrFixAuto =
    case growOrFixAuto of
        Grow ->
            "1fr"

        FixMaxContent ->
            "max-content"


panelToGrowOrFixAutoWidth : Panel msg -> GrowOrFixAuto
panelToGrowOrFixAutoWidth (Panel { styleAndEvent }) =
    case styleAndEvent.width of
        Just _ ->
            FixMaxContent

        Nothing ->
            Grow


panelToGrowOrFixAutoHeight : Panel msg -> GrowOrFixAuto
panelToGrowOrFixAutoHeight (Panel { styleAndEvent, content }) =
    case ( styleAndEvent.height, content ) of
        ( Just _, _ ) ->
            FixMaxContent

        ( Nothing, TextBoxFitHeight _ ) ->
            FixMaxContent

        ( Nothing, _ ) ->
            Grow


gridSetPosition : Css.Style
gridSetPosition =
    Css.batch
        [ Css.property "grid-row" "1 / 2"
        , Css.property "grid-column" "1 / 2"
        ]


{-| 縦のそろえ方
-}
type AlignItems
    = Top
    | CenterY
    | Bottom


alignItemsToStyle : AlignItems -> Css.Style
alignItemsToStyle alignItems =
    Css.alignItems
        (case alignItems of
            Top ->
                Css.flexStart

            CenterY ->
                Css.center

            Bottom ->
                Css.flexEnd
        )


textAlignToStyle : TextAlignment -> Css.Style
textAlignToStyle textAlignment =
    Css.textAlign
        (case textAlignment of
            TextAlignStart ->
                Css.start

            TextAlignEnd ->
                Css.end

            TextAlignCenter ->
                Css.center

            TextAlignJustify ->
                Css.justify
        )


pointerGetPosition : Pointer -> ( Float, Float )
pointerGetPosition (Pointer { position }) =
    position
