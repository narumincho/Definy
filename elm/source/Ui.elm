module Ui exposing
    ( Alignment(..)
    , BitmapImageAttributes(..)
    , BorderRadius(..)
    , BorderStyle(..)
    , FitStyle(..)
    , ImageRendering(..)
    , LinkAttributes(..)
    , Panel
    , Pointer
    , PointerImage(..)
    , Size
    , Style
    , TextAlignment(..)
    , TextAttributes(..)
    , TextInputAttributes(..)
    , VectorImageAttributes(..)
    , auto
    , backgroundColor
    , bitmapImage
    , border
    , borderRadius
    , button
    , column
    , depth
    , empty
    , fix
    , focusAble
    , gap
    , link
    , map
    , offset
    , overflowVisible
    , padding
    , pointerImage
    , row
    , scroll
    , stretch
    , stretchWithMaxSize
    , text
    , textInput
    , toHtml
    , vectorImage
    )

import Bitwise
import Css
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import Json.Decode.Pipeline
import Url
import VectorImage


{-| デフォルトで幅と高さが外の大きさによって決まる,パネル
-}
type Panel message
    = Panel
        { style : StyleComputed
        , width : Size
        , height : Size
        , content : Content message
        }


type Content message
    = Text TextAttributes
    | BitmapImage BitmapImageAttributes
    | VectorImage (VectorImageAttributes message)
    | Empty
    | Depth (List ( ( Alignment, Alignment ), Panel message ))
    | Row (List (Panel message))
    | Column (List (Panel message))
    | Button (ButtonAttributes message)
    | Link (LinkAttributes message)
    | PointerPanel (PointerPanelAttributes message)
    | Scroll (Panel message)
    | TextInput (TextInputAttributes message)


type TextAttributes
    = TextAttributes
        { text : String
        , typeface : String
        , size : Int
        , letterSpacing : Float
        , lineHeight : Float
        , color : Css.Color
        , textAlignment : TextAlignment
        }


type BitmapImageAttributes
    = BitmapImageAttributes
        { blobUrl : String
        , fitStyle : FitStyle
        , alternativeText : String
        , rendering : ImageRendering
        }


type VectorImageAttributes message
    = VectorImageAttributes
        { fitStyle : FitStyle
        , viewBox : { x : Int, y : Int, width : Int, height : Int }
        , elements : List (VectorImage.Element message)
        }


type ButtonAttributes message
    = ButtonAttributes
        { clickMessage : message
        , child : Panel message
        }


type LinkAttributes message
    = LinkAttributes
        { url : Url.Url
        , id : Maybe String
        , child : Panel message
        }


type PointerPanelAttributes message
    = PointerPanelAttributes
        { enterMessage : Maybe (Pointer -> message)
        , leaveMessage : Maybe (Pointer -> message)
        , moveMessage : Maybe (Pointer -> message)
        , downMessage : Maybe (Pointer -> message)
        , child : Panel message
        }


type TextInputAttributes message
    = TextInputAttributes
        { inputMessage : String -> message
        , name : String
        , id : String
        , multiLine : Bool
        , fontSize : Int
        }


type Size
    = Fix Int
    | Stretch (Maybe Int)
    | Auto


{-| 固定サイズにする
-}
fix : Int -> Size
fix =
    Fix


{-| 親の大きさに合わせる (デフォルト)
-}
stretch : Size
stretch =
    Stretch Nothing


{-| 基本的に親のサイズになるように大きくなるが,指定した最大の大きさに収まるようにする
-}
stretchWithMaxSize : Int -> Size
stretchWithMaxSize maxSize =
    Stretch (Just maxSize)


{-| 中身に合わせる
-}
auto : Size
auto =
    Auto


type Alignment
    = Start
    | Center
    | End


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
type StyleComputed
    = StyleComputed
        { padding : Int
        , offset : Maybe ( Int, Int )
        , overflowVisible : Bool
        , pointerImage : Maybe PointerImage
        , border : Maybe BorderStyle
        , borderRadius : BorderRadius
        , backGroundColor : Maybe Css.Color
        , gap : Int
        , focusAble : Maybe String
        }


type Style
    = Padding Int
    | Offset ( Int, Int )
    | OverflowVisible
    | PointerImage PointerImage
    | Border BorderStyle
    | BorderRadius BorderRadius
    | BackGroundColor Css.Color
    | Gap Int
    | FocusAble String


type BorderRadius
    = BorderRadiusPx Int
    | BorderRadiusPercent Int


type PointerImage
    = HorizontalResize -- ew-resize ↔
    | VerticalResize -- ns-resize ↕


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


toHtml : Panel msg -> Html.Styled.Html msg
toHtml =
    panelToHtml (GridCell { row = 0, column = 0 }) StretchStretch


padding : Int -> Style
padding =
    Padding


offset : ( Int, Int ) -> Style
offset =
    Offset


overflowVisible : Style
overflowVisible =
    OverflowVisible


pointerImage : PointerImage -> Style
pointerImage =
    PointerImage


border : BorderStyle -> Style
border =
    Border


borderRadius : BorderRadius -> Style
borderRadius =
    BorderRadius


backgroundColor : Css.Color -> Style
backgroundColor =
    BackGroundColor


{-| rowやcolumnの間隔を開けることができる. デフォルトは0
-}
gap : Int -> Style
gap =
    Gap


{-| フォーカスできる要素にして, 指定したIDにする
-}
focusAble : String -> Style
focusAble =
    FocusAble


{-| テキストボックス
-}
text : Size -> Size -> List Style -> TextAttributes -> Panel message
text width height style textBoxAttributes =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Text textBoxAttributes
        }


{-| ビットマップ画像
-}
bitmapImage : Size -> Size -> List Style -> BitmapImageAttributes -> Panel message
bitmapImage width height style bitmapImageAttributes =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = BitmapImage bitmapImageAttributes
        }


{-| ベクター画像
-}
vectorImage : Size -> Size -> List Style -> VectorImageAttributes message -> Panel message
vectorImage width height style vectorImageAttributes =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = VectorImage vectorImageAttributes
        }


{-| からのパネル
-}
empty : Size -> Size -> List Style -> Panel message
empty width height style =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Empty
        }


{-| ボタン
-}
button : Size -> Size -> List Style -> message -> Panel message -> Panel message
button width height style clickMessage child =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content =
            Button
                (ButtonAttributes
                    { clickMessage = clickMessage
                    , child = child
                    }
                )
        }


link : Size -> Size -> List Style -> LinkAttributes message -> Panel message
link width height style linkAttributes =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Link linkAttributes
        }


{-| 中身が領域より大きい場合,中身をスクロールできるようにする
-}
scroll : Size -> Size -> List Style -> Panel message -> Panel message
scroll width height style child =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Scroll child
        }


{-| パネルを同じ領域に重ねる
-}
depth : Size -> Size -> List Style -> List ( ( Alignment, Alignment ), Panel message ) -> Panel message
depth width height style children =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Depth children
        }


{-| 横方向に並べる
-}
row : Size -> Size -> List Style -> List (Panel message) -> Panel message
row width height style children =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Row children
        }


{-| 縦方向に並べる
-}
column : Size -> Size -> List Style -> List (Panel message) -> Panel message
column width height style children =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = Column children
        }


{-| 文字の入力ボックス
-}
textInput : Size -> Size -> List Style -> TextInputAttributes message -> Panel message
textInput width height style attributes =
    Panel
        { style = computeStyle style
        , width = width
        , height = height
        , content = TextInput attributes
        }


computeStyle : List Style -> StyleComputed
computeStyle list =
    case list of
        x :: xs ->
            let
                (StyleComputed rest) =
                    computeStyle xs
            in
            StyleComputed
                (case x of
                    Padding int ->
                        { rest | padding = int }

                    Offset vector ->
                        { rest | offset = Just vector }

                    OverflowVisible ->
                        { rest | overflowVisible = True }

                    PointerImage pointerImage_ ->
                        { rest | pointerImage = Just pointerImage_ }

                    Border borderStyle ->
                        { rest | border = Just borderStyle }

                    BorderRadius int ->
                        { rest | borderRadius = int }

                    BackGroundColor color ->
                        { rest | backGroundColor = Just color }

                    Gap px ->
                        { rest | gap = px }

                    FocusAble idString ->
                        { rest | focusAble = Just idString }
                )

        [] ->
            defaultStyleComputed


defaultStyleComputed : StyleComputed
defaultStyleComputed =
    StyleComputed
        { padding = 0
        , offset = Nothing
        , overflowVisible = False
        , pointerImage = Nothing
        , border = Nothing
        , borderRadius = BorderRadiusPx 0
        , backGroundColor = Nothing
        , gap = 0
        , focusAble = Nothing
        }


map : (a -> b) -> Panel a -> Panel b
map func (Panel record) =
    Panel
        { style = record.style
        , width = record.width
        , height = record.height
        , content = mapContent func record.content
        }


mapContent : (a -> b) -> Content a -> Content b
mapContent func content =
    case content of
        Text textBoxAttributes ->
            Text textBoxAttributes

        BitmapImage bitmapImageAttributes ->
            BitmapImage bitmapImageAttributes

        VectorImage (VectorImageAttributes record) ->
            VectorImage
                (VectorImageAttributes
                    { fitStyle = record.fitStyle
                    , viewBox = record.viewBox
                    , elements = List.map (VectorImage.map func) record.elements
                    }
                )

        Empty ->
            Empty

        Button (ButtonAttributes record) ->
            Button
                (ButtonAttributes
                    { clickMessage = func record.clickMessage
                    , child = map func record.child
                    }
                )

        Link (LinkAttributes record) ->
            Link
                (LinkAttributes
                    { url = record.url
                    , id = record.id
                    , child = map func record.child
                    }
                )

        Depth children ->
            Depth (List.map (Tuple.mapSecond (map func)) children)

        Row children ->
            Row (List.map (map func) children)

        Column children ->
            Column (List.map (map func) children)

        PointerPanel (PointerPanelAttributes record) ->
            PointerPanel
                (PointerPanelAttributes
                    { enterMessage = Maybe.map (\msg pointer -> func (msg pointer)) record.enterMessage
                    , leaveMessage = Maybe.map (\msg pointer -> func (msg pointer)) record.leaveMessage
                    , moveMessage = Maybe.map (\msg pointer -> func (msg pointer)) record.moveMessage
                    , downMessage = Maybe.map (\msg pointer -> func (msg pointer)) record.downMessage
                    , child = map func record.child
                    }
                )

        Scroll child ->
            Scroll (map func child)

        TextInput (TextInputAttributes record) ->
            TextInput
                (TextInputAttributes
                    { inputMessage = \inputtedText -> func (record.inputMessage inputtedText)
                    , id = record.id
                    , name = record.name
                    , multiLine = record.multiLine
                    , fontSize = record.fontSize
                    }
                )


type GridCell
    = GridCell { row : Int, column : Int }


panelToHtml : GridCell -> AlignmentOrStretch -> Panel msg -> Html.Styled.Html msg
panelToHtml gridCell alignmentOrStretch panel =
    domDataToHtml
        gridCell
        alignmentOrStretch
        (contentToDomData panel)
        panel


type DomData message
    = DomData
        { node :
            List (Html.Styled.Attribute message)
            -> List (Html.Styled.Html message)
            -> Html.Styled.Html message
        , style : Css.Style
        , attributes : List (Html.Styled.Attribute message)
        , children : List (Html.Styled.Html message)
        , isButtonElement : Bool
        }


domDataToHtml : GridCell -> AlignmentOrStretch -> DomData message -> Panel message -> Html.Styled.Html message
domDataToHtml gridCell alignmentOrStretch (DomData domData) (Panel record) =
    domData.node
        (domData.attributes
            ++ (Html.Styled.Attributes.css
                    [ alignmentOrStretchToCssStyle
                        record.width
                        record.height
                        alignmentOrStretch
                    , gridCellToCssStyle gridCell
                    , Css.batch
                        (if isIncludeScrollInPanel (Panel record) then
                            [ Css.overflow Css.auto ]

                         else
                            []
                        )
                    , styleComputedToCssStyle
                        record.width
                        record.height
                        domData.isButtonElement
                        record.style
                    , domData.style
                    ]
                    :: (case record.style of
                            StyleComputed styleRecord ->
                                case styleRecord.focusAble of
                                    Just idString ->
                                        [ Html.Styled.Attributes.id idString
                                        , Html.Styled.Attributes.tabindex 0
                                        ]

                                    Nothing ->
                                        []
                       )
               )
        )
        domData.children


contentToDomData : Panel message -> DomData message
contentToDomData (Panel record) =
    case record.content of
        Text textAttributes ->
            textToDomData textAttributes

        BitmapImage bitmapImageAttributes ->
            bitmapImageToDomData bitmapImageAttributes

        VectorImage vectorImageAttributes ->
            vectorImageToDomData vectorImageAttributes

        Empty ->
            emptyToDomData

        Depth list ->
            depthToDomData list

        Row panels ->
            rowToDomData record.width panels

        Column panels ->
            columnToDomData record.height panels

        Button buttonAttributes ->
            buttonToDomData buttonAttributes

        Link linkAttributes ->
            linkToDomData linkAttributes

        PointerPanel pointerPanelAttributes ->
            pointerPanelToDomData pointerPanelAttributes

        Scroll panel ->
            scrollBoxToDomData panel

        TextInput textInputAttributes ->
            textInputToDomData textInputAttributes


textToDomData : TextAttributes -> DomData message
textToDomData (TextAttributes record) =
    DomData
        { node = Html.Styled.div
        , style =
            Css.batch
                [ Css.color record.color
                , Css.fontSize (Css.px (toFloat record.size))
                , Css.fontFamilies [ Css.qt record.typeface ]
                , Css.letterSpacing (Css.px record.letterSpacing)
                , Css.lineHeight (Css.num record.lineHeight)
                , Css.overflowWrap Css.breakWord
                , textAlignToStyle record.textAlignment
                ]
        , attributes = []
        , children = [ Html.Styled.text record.text ]
        , isButtonElement = False
        }


bitmapImageToDomData : BitmapImageAttributes -> DomData message
bitmapImageToDomData (BitmapImageAttributes record) =
    DomData
        { node = Html.Styled.img
        , style =
            Css.batch
                ([ Css.property "object-fit"
                    (case record.fitStyle of
                        Contain ->
                            "contain"

                        Cover ->
                            "cover"
                    )
                 , Css.display Css.block
                 ]
                    ++ (case record.rendering of
                            ImageRenderingAuto ->
                                []

                            ImageRenderingPixelated ->
                                [ Css.property "image-rendering" "pixelated" ]
                       )
                )
        , attributes =
            [ Html.Styled.Attributes.src record.blobUrl
            , Html.Styled.Attributes.alt record.alternativeText
            ]
        , children = []
        , isButtonElement = False
        }


vectorImageToDomData : VectorImageAttributes message -> DomData message
vectorImageToDomData (VectorImageAttributes record) =
    DomData
        { node = Html.Styled.div
        , style = Css.batch []
        , attributes = []
        , children =
            [ VectorImage.toHtml
                record.viewBox
                Nothing
                record.elements
            ]
        , isButtonElement = False
        }


emptyToDomData : DomData message
emptyToDomData =
    DomData
        { node = Html.Styled.div
        , style = Css.batch []
        , attributes = []
        , children = []
        , isButtonElement = False
        }


buttonToDomData : ButtonAttributes message -> DomData message
buttonToDomData (ButtonAttributes record) =
    DomData
        { node = Html.Styled.button
        , style = Css.batch []
        , attributes =
            [ Html.Styled.Events.onClick record.clickMessage ]
        , children =
            [ panelToHtml (GridCell { row = 0, column = 0 })
                StretchStretch
                record.child
            ]
        , isButtonElement = True
        }


linkToDomData : LinkAttributes message -> DomData message
linkToDomData (LinkAttributes record) =
    DomData
        { node = Html.Styled.a
        , style = Css.batch []
        , attributes =
            Html.Styled.Attributes.href (Url.toString record.url)
                :: (case record.id of
                        Just idAsString ->
                            [ Html.Styled.Attributes.id idAsString ]

                        Nothing ->
                            []
                   )
        , children =
            [ panelToHtml (GridCell { row = 0, column = 0 })
                StretchStretch
                record.child
            ]
        , isButtonElement = False
        }


depthToDomData : List ( ( Alignment, Alignment ), Panel message ) -> DomData message
depthToDomData children =
    DomData
        { node = Html.Styled.div
        , style =
            Css.batch
                [ Css.property "display" "grid"
                , Css.property "grid-template-rows" "1fr"
                , Css.property "grid-template-columns" "1fr"
                ]
        , attributes = []
        , children =
            List.map
                (\( alignment, panel ) ->
                    panelToHtml
                        (GridCell { row = 0, column = 0 })
                        (Alignment alignment)
                        panel
                )
                children
        , isButtonElement = False
        }


rowToDomData : Size -> List (Panel message) -> DomData message
rowToDomData width children =
    DomData
        { node = Html.Styled.div
        , style =
            Css.batch
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns"
                    (sizeListToGridTemplate width
                        (List.map panelGetWidth children)
                    )
                ]
        , attributes = []
        , children =
            List.indexedMap
                (\index panel ->
                    panelToHtml
                        (GridCell { row = 0, column = index })
                        StretchRow
                        panel
                )
                children
        , isButtonElement = False
        }


columnToDomData : Size -> List (Panel message) -> DomData message
columnToDomData height children =
    DomData
        { node = Html.Styled.div
        , style =
            Css.batch
                [ Css.property "display" "grid"
                , Css.property "grid-template-rows"
                    (sizeListToGridTemplate height
                        (List.map panelGetHeight children)
                    )
                ]
        , attributes = []
        , children =
            List.indexedMap
                (\index panel ->
                    panelToHtml
                        (GridCell { row = index, column = 0 })
                        StretchColumn
                        panel
                )
                children
        , isButtonElement = False
        }


panelGetWidth : Panel message -> Size
panelGetWidth (Panel record) =
    record.width


panelGetHeight : Panel message -> Size
panelGetHeight (Panel record) =
    record.height


pointerPanelToDomData : PointerPanelAttributes message -> DomData message
pointerPanelToDomData (PointerPanelAttributes record) =
    DomData
        { node = Html.Styled.div
        , style = Css.batch []
        , attributes =
            List.concat
                [ case record.enterMessage of
                    Just msg ->
                        [ Html.Styled.Events.on "pointerenter" (Json.Decode.map msg pointerEventDecoder) ]

                    Nothing ->
                        []
                , case record.leaveMessage of
                    Just msg ->
                        [ Html.Styled.Events.on "pointerleave" (Json.Decode.map msg pointerEventDecoder) ]

                    Nothing ->
                        []
                , case record.moveMessage of
                    Just msg ->
                        [ Html.Styled.Events.on "pointermove" (Json.Decode.map msg pointerEventDecoder) ]

                    Nothing ->
                        []
                , case record.downMessage of
                    Just msg ->
                        [ Html.Styled.Events.on "pointerdown" (Json.Decode.map msg pointerEventDecoder) ]

                    Nothing ->
                        []
                ]
        , children =
            [ panelToHtml
                (GridCell { row = 0, column = 0 })
                StretchStretch
                record.child
            ]
        , isButtonElement = False
        }


scrollBoxToDomData : Panel message -> DomData message
scrollBoxToDomData child =
    DomData
        { node = Html.Styled.div
        , style = Css.batch []
        , attributes = []
        , children = [ panelToHtml (GridCell { row = 0, column = 0 }) StretchStretch child ]
        , isButtonElement = False
        }


textInputToDomData : TextInputAttributes message -> DomData message
textInputToDomData (TextInputAttributes attributes) =
    DomData
        { node =
            if attributes.multiLine then
                Html.Styled.textarea

            else
                Html.Styled.input
        , style =
            Css.batch
                [ Css.fontSize (Css.px (toFloat attributes.fontSize))
                , Css.color (Css.rgb 0 0 0)
                , Css.resize Css.none
                ]
        , attributes =
            [ Html.Styled.Attributes.name attributes.name
            , Html.Styled.Attributes.id attributes.id
            , Html.Styled.Events.onInput attributes.inputMessage
            ]
        , children = []
        , isButtonElement = False
        }


styleComputedToCssStyle : Size -> Size -> Bool -> StyleComputed -> Css.Style
styleComputedToCssStyle width height isButtonElement (StyleComputed record) =
    [ [ Css.padding (Css.px (toFloat record.padding))
      , Css.zIndex (Css.int 0)
      , Css.textDecoration Css.none
      ]
    , case width of
        Fix px ->
            [ Css.width (Css.px (toFloat px)) ]

        Stretch maxSizeMaybe ->
            Css.width (Css.pct 100)
                :: (case maxSizeMaybe of
                        Just maxSize ->
                            [ Css.maxWidth (Css.px (toFloat maxSize)) ]

                        Nothing ->
                            []
                   )

        Auto ->
            [ Css.width Css.auto ]
    , case height of
        Fix px ->
            [ Css.height (Css.px (toFloat px)) ]

        Stretch maxSizeMaybe ->
            Css.height (Css.pct 100)
                :: (case maxSizeMaybe of
                        Just maxSize ->
                            [ Css.maxHeight (Css.px (toFloat maxSize)) ]

                        Nothing ->
                            []
                   )

        Auto ->
            [ Css.height Css.auto ]
    , if isButtonElement then
        [ Css.cursor Css.pointer ]

      else
        []
    , case record.border of
        Just border_ ->
            [ borderStyleToStyle border_ ]

        Nothing ->
            if isButtonElement then
                [ Css.border2 Css.zero Css.none ]

            else
                []
    , case record.offset of
        Just ( left, top ) ->
            [ Css.transform
                (Css.translate2
                    (Css.px (toFloat left))
                    (Css.px (toFloat top))
                )
            ]

        Nothing ->
            []
    , case record.pointerImage of
        Just HorizontalResize ->
            [ Css.cursor Css.ewResize ]

        Just VerticalResize ->
            [ Css.cursor Css.ewResize ]

        Nothing ->
            []
    , case record.borderRadius of
        BorderRadiusPx 0 ->
            if isButtonElement then
                [ Css.borderRadius Css.zero ]

            else
                []

        BorderRadiusPercent 0 ->
            if isButtonElement then
                [ Css.borderRadius Css.zero ]

            else
                []

        BorderRadiusPx size ->
            [ Css.borderRadius (Css.px (toFloat size)) ]

        BorderRadiusPercent percent ->
            [ Css.borderRadius (Css.pct (toFloat percent)) ]
    , case record.backGroundColor of
        Just color ->
            [ Css.backgroundColor color ]

        Nothing ->
            if isButtonElement then
                [ Css.backgroundColor Css.transparent ]

            else
                []
    , case record.gap of
        0 ->
            []

        size ->
            [ Css.property "gap" (String.fromInt size ++ "px") ]
    ]
        |> List.concat
        |> Css.batch


borderStyleToStyle : BorderStyle -> Css.Style
borderStyleToStyle (BorderStyle record) =
    [ Css.borderColor record.color
    , Css.borderStyle Css.solid
    , Css.borderWidth4
        (Css.px (toFloat record.width.top))
        (Css.px (toFloat record.width.right))
        (Css.px (toFloat record.width.bottom))
        (Css.px (toFloat record.width.left))
    ]
        |> Css.batch


pointerEventDecoder : Json.Decode.Decoder Pointer
pointerEventDecoder =
    Json.Decode.succeed
        (\pointerId clientX clientY button_ pressure tangentialPressure width_ height_ tiltX tiltY twist pointerType isPrimary buttons eventPhase ->
            Pointer
                { id = pointerId
                , position = ( clientX, clientY )
                , button =
                    case button_ of
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


sizeListToGridTemplate : Size -> List Size -> String
sizeListToGridTemplate sizeAndAlignment panelList =
    let
        itemList : List String
        itemList =
            List.map
                sizeListToGridTemplateItem
                panelList
                ++ (if List.any isStretch panelList then
                        []

                    else
                        case sizeAndAlignment of
                            Stretch _ ->
                                [ "1fr" ]

                            Fix _ ->
                                [ "1fr" ]

                            Auto ->
                                []
                   )
    in
    String.join " " itemList


isStretch : Size -> Bool
isStretch size =
    case size of
        Stretch _ ->
            True

        _ ->
            False


sizeListToGridTemplateItem : Size -> String
sizeListToGridTemplateItem size =
    case size of
        Fix int ->
            String.fromInt int ++ "px"

        Stretch _ ->
            "1fr"

        Auto ->
            "auto"


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
        )


gridCellToCssStyle : GridCell -> Css.Style
gridCellToCssStyle (GridCell record) =
    Css.batch
        [ Css.property "grid-row"
            (String.fromInt (record.row + 1)
                ++ " / "
                ++ String.fromInt (record.row + 2)
            )
        , Css.property "grid-column"
            (String.fromInt (record.column + 1)
                ++ " / "
                ++ String.fromInt (record.column + 2)
            )
        ]


type AlignmentOrStretch
    = Alignment ( Alignment, Alignment )
    | StretchRow
    | StretchColumn
    | StretchStretch


alignmentOrStretchToCssStyle : Size -> Size -> AlignmentOrStretch -> Css.Style
alignmentOrStretchToCssStyle width height alignmentOrStretch =
    Css.batch
        (case alignmentOrStretch of
            Alignment ( x, y ) ->
                [ justifySelf
                    (if isStretch width then
                        "stretch"

                     else
                        case x of
                            Start ->
                                "start"

                            Center ->
                                "center"

                            End ->
                                "end"
                    )
                , Css.alignSelf
                    (if isStretch height then
                        Css.stretch

                     else
                        case y of
                            Start ->
                                Css.start

                            Center ->
                                Css.center

                            End ->
                                Css.end
                    )
                ]

            StretchRow ->
                [ justifySelf "stretch"
                , Css.alignSelf Css.center
                ]

            StretchColumn ->
                [ justifySelf "center"
                , Css.alignSelf Css.stretch
                ]

            StretchStretch ->
                [ justifySelf "stretch"
                , Css.alignSelf Css.stretch
                ]
        )


justifySelf : String -> Css.Style
justifySelf =
    Css.property "justify-self"


isIncludeScrollInPanel : Panel message -> Bool
isIncludeScrollInPanel (Panel record) =
    case record.content of
        Text _ ->
            False

        BitmapImage _ ->
            False

        VectorImage _ ->
            False

        Empty ->
            False

        Depth list ->
            List.any (\( _, panel ) -> isIncludeScrollInPanel panel) list

        Row panels ->
            List.any isIncludeScrollInPanel panels

        Column panels ->
            List.any isIncludeScrollInPanel panels

        Button (ButtonAttributes attributes) ->
            isIncludeScrollInPanel attributes.child

        Link (LinkAttributes attributes) ->
            isIncludeScrollInPanel attributes.child

        PointerPanel (PointerPanelAttributes attributes) ->
            isIncludeScrollInPanel attributes.child

        Scroll _ ->
            True

        TextInput _ ->
            False
