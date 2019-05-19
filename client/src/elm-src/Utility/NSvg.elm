module Utility.NSvg exposing
    ( NSvg, StrokeStyle, FillStyle
    , toHtml, toHtmlWithClass
    , strokeNone, strokeColor, strokeColorWidth
    , strokeColorAndStrokeLineJoinRound
    , fillNone, fillColor
    , rect, rectWithClickEvent, circle, polygon, polygonWithClickEvent, path, line
    , translate
    , strokeWidth
    )

{-| 標準のelm/svgは型がゆるいのでできるだけ間違わないように型をつけた新しいSVGの表現を作った
要素を作ってから移動もできる

@docs NSvg, StrokeStyle, FillStyle

@docs toHtml, toHtmlWithClass

@docs strokeNone, strokeColor, strokeColorWidth

@docs strokeColorAndStrokeLineJoinRound

@docs fillNone, fillColor

@docs rect, rectWithClickEvent, circle, polygon, polygonWithClickEvent, path, line

@docs translate

-}

import Color exposing (Color)
import Html
import Svg as S
import Svg.Attributes as Sa
import Svg.Events as Se


type NSvg msg
    = Rect { x : Int, y : Int, width : Int, height : Int, strokeStyle : StrokeStyle, fillStyle : FillStyle, clickMsg : Maybe msg }
    | Circle { cx : Int, cy : Int, r : Int, strokeStyle : StrokeStyle, fillStyle : FillStyle }
    | Polygon { points : List ( Int, Int ), strokeStyle : StrokeStyle, fillStyle : FillStyle, clickMsg : Maybe msg }
    | Path { d : String, strokeStyle : StrokeStyle, fillStyle : FillStyle, offset : ( Int, Int ) }
    | Line { x0 : Int, y0 : Int, x1 : Int, y1 : Int, strokeStyle : StrokeStyle }


toHtml : { x : Int, y : Int, width : Int, height : Int } -> List (NSvg msg) -> Html.Html msg
toHtml { x, y, width, height } children =
    S.svg
        [ Sa.viewBox
            (String.fromInt x
                ++ " "
                ++ String.fromInt y
                ++ " "
                ++ String.fromInt width
                ++ " "
                ++ String.fromInt height
            )
        ]
        (children |> List.map elementToSvg)


toHtmlWithClass : String -> { x : Int, y : Int, width : Int, height : Int } -> List (NSvg msg) -> Html.Html msg
toHtmlWithClass className { x, y, width, height } children =
    S.svg
        [ Sa.viewBox
            (String.fromInt x
                ++ " "
                ++ String.fromInt y
                ++ " "
                ++ String.fromInt width
                ++ " "
                ++ String.fromInt height
            )
        , Sa.class className
        ]
        (children |> List.map elementToSvg)


elementToSvg : NSvg msg -> S.Svg msg
elementToSvg nSvgElement =
    case nSvgElement of
        Rect { x, y, width, height, strokeStyle, fillStyle, clickMsg } ->
            S.rect
                ([ Sa.x (String.fromInt x)
                 , Sa.y (String.fromInt y)
                 , Sa.width (String.fromInt width)
                 , Sa.height (String.fromInt height)
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                    ++ fillStyleToSvgAttributes fillStyle
                    ++ clickMsgToSvgAttributes clickMsg
                )
                []

        Circle { cx, cy, r, strokeStyle, fillStyle } ->
            S.circle
                ([ Sa.cx (String.fromInt cx)
                 , Sa.cy (String.fromInt cy)
                 , Sa.r (String.fromInt r)
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                    ++ fillStyleToSvgAttributes fillStyle
                )
                []

        Polygon { points, strokeStyle, fillStyle, clickMsg } ->
            S.polygon
                ([ Sa.points (points |> List.map (\( x, y ) -> String.fromInt x ++ "," ++ String.fromInt y) |> String.join " ")
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                    ++ fillStyleToSvgAttributes fillStyle
                    ++ clickMsgToSvgAttributes clickMsg
                )
                []

        Path { d, strokeStyle, fillStyle, offset } ->
            S.path
                ([ Sa.d d ] ++ strokeStyleToSvgAttributes strokeStyle ++ fillStyleToSvgAttributes fillStyle ++ offsetTranslate offset)
                []

        Line { x0, y0, x1, y1, strokeStyle } ->
            S.line
                ([ Sa.x1 (String.fromInt x0)
                 , Sa.y1 (String.fromInt y0)
                 , Sa.x2 (String.fromInt x1)
                 , Sa.y2 (String.fromInt y1)
                 ]
                    ++ strokeStyleToSvgAttributes strokeStyle
                )
                []


{-| 平行移動をtransform属性のtranslateで表現する
-}
offsetTranslate : ( Int, Int ) -> List (S.Attribute msg)
offsetTranslate ( x, y ) =
    if x == 0 && y == 0 then
        []

    else
        [ Sa.transform ("translate(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")") ]


{-|

    線の表現

-}
type StrokeStyle
    = StrokeNone
    | Stroke
        { color : Maybe Color
        , width : Int
        , strokeLineJoin : StrokeLineJoin
        }


{-| 頂点のスタイル
-}
type StrokeLineJoin
    = StrokeLineJoinMiter -- 鋭角 初期値
    | StrokeLineJoinRound -- 円形
    | StrokeLineJoinBevel -- 面取り


{-| 線を描かない
-}
strokeNone : StrokeStyle
strokeNone =
    StrokeNone


{-| 線の色だけ指定。線の太さは初期値の1
-}
strokeColor : Color -> StrokeStyle
strokeColor color =
    Stroke
        { color = Just color
        , width = 1
        , strokeLineJoin = StrokeLineJoinMiter
        }


{-| 線の色と幅を指定
-}
strokeColorWidth : Color -> Int -> StrokeStyle
strokeColorWidth color width =
    Stroke
        { color = Just color
        , width = width
        , strokeLineJoin = StrokeLineJoinMiter
        }


{-| 幅のみ指定。色はCSSで上書きする用
-}
strokeWidth : Int -> StrokeStyle
strokeWidth width =
    Stroke
        { color = Nothing
        , width = width
        , strokeLineJoin = StrokeLineJoinMiter
        }


{-| 線の頂点は丸い線
-}
strokeColorAndStrokeLineJoinRound : Color -> StrokeStyle
strokeColorAndStrokeLineJoinRound color =
    Stroke
        { color = Just color
        , width = 1
        , strokeLineJoin = StrokeLineJoinRound
        }


{-| 線のスタイルをSvgの属性に変換
-}
strokeStyleToSvgAttributes : StrokeStyle -> List (S.Attribute msg)
strokeStyleToSvgAttributes strokeStyle =
    case strokeStyle of
        StrokeNone ->
            []

        Stroke { color, width, strokeLineJoin } ->
            (case color of
                Just c ->
                    [ Sa.stroke (Color.toHexString c) ]

                Nothing ->
                    []
            )
                ++ (if width == 1 then
                        []

                    else
                        [ Sa.strokeWidth (String.fromInt width) ]
                   )
                ++ (case strokeLineJoin of
                        StrokeLineJoinMiter ->
                            []

                        StrokeLineJoinRound ->
                            [ Sa.strokeLinejoin "round" ]

                        StrokeLineJoinBevel ->
                            [ Sa.strokeLinejoin "bevel" ]
                   )


{-|

    塗りの表現

-}
type FillStyle
    = FillNone
    | FillWithColor { color : Color }


{-| 塗りつぶさない
-}
fillNone : FillStyle
fillNone =
    FillNone


{-| 単色で塗りつぶす
-}
fillColor : Color -> FillStyle
fillColor color =
    FillWithColor
        { color = color }


{-| 塗りつぶしのスタイルをSvgの属性に変換
-}
fillStyleToSvgAttributes : FillStyle -> List (S.Attribute msg)
fillStyleToSvgAttributes fillStyle =
    case fillStyle of
        FillNone ->
            [ Sa.fill "none" ]

        FillWithColor { color } ->
            [ Sa.fill (Color.toHexString color) ]


clickMsgToSvgAttributes : Maybe msg -> List (S.Attribute msg)
clickMsgToSvgAttributes msg =
    case msg of
        Just m ->
            [ Se.onClick m ]

        Nothing ->
            []


{-| 四角形
-}
rect : { width : Int, height : Int } -> StrokeStyle -> FillStyle -> NSvg msg
rect { width, height } strokeStyle fillStyle =
    Rect
        { x = 0
        , y = 0
        , width = width
        , height = height
        , strokeStyle = strokeStyle
        , fillStyle = fillStyle
        , clickMsg = Nothing
        }


rectWithClickEvent : { width : Int, height : Int } -> StrokeStyle -> FillStyle -> msg -> NSvg msg
rectWithClickEvent { width, height } strokeStyle fillStyle clickMsg =
    Rect
        { x = 0
        , y = 0
        , width = width
        , height = height
        , strokeStyle = strokeStyle
        , fillStyle = fillStyle
        , clickMsg = Just clickMsg
        }


{-| 正円 半径を指定する
-}
circle : Int -> StrokeStyle -> FillStyle -> NSvg msg
circle r strokeStyle fillStyle =
    Circle
        { cx = 0, cy = 0, r = r, strokeStyle = strokeStyle, fillStyle = fillStyle }


{-| 多角形(始点と終点は自動的につながる) XY座標のリストを指定する
-}
polygon : List ( Int, Int ) -> StrokeStyle -> FillStyle -> NSvg msg
polygon points strokeStyle fillStyle =
    Polygon
        { points = points
        , strokeStyle = strokeStyle
        , fillStyle = fillStyle
        , clickMsg = Nothing
        }


polygonWithClickEvent : List ( Int, Int ) -> StrokeStyle -> FillStyle -> msg -> NSvg msg
polygonWithClickEvent points strokeStyle fillStyle cliskMsg =
    Polygon
        { points = points
        , strokeStyle = strokeStyle
        , fillStyle = fillStyle
        , clickMsg = Just cliskMsg
        }


{-| 汎用的な図形
-}
path : String -> StrokeStyle -> FillStyle -> NSvg msg
path d strokeStyle fillStyle =
    Path
        { d = d
        , strokeStyle = strokeStyle
        , fillStyle = fillStyle
        , offset = ( 0, 0 )
        }


{-| 原点から伸ばした直線
-}
lineFromOrigin : ( Int, Int ) -> StrokeStyle -> NSvg msg
lineFromOrigin ( x, y ) strokeStyle =
    Line
        { x0 = 0
        , y0 = 0
        , x1 = x
        , y1 = y
        , strokeStyle = strokeStyle
        }


line : ( Int, Int ) -> ( Int, Int ) -> StrokeStyle -> NSvg msg
line ( x0, y0 ) ( x1, y1 ) strokeStyle =
    Line
        { x0 = x0
        , y0 = y0
        , x1 = x1
        , y1 = y1
        , strokeStyle = strokeStyle
        }


{-| 図形を移動する。pathは移動できない
-}
translate : { x : Int, y : Int } -> NSvg msg -> NSvg msg
translate { x, y } nSvgElement =
    case nSvgElement of
        Rect rec ->
            Rect
                { rec
                    | x = rec.x + x
                    , y = rec.y + y
                }

        Circle rec ->
            Circle
                { rec
                    | cx = rec.cx + x
                    , cy = rec.cy + y
                }

        Polygon rec ->
            Polygon
                { rec
                    | points = rec.points |> List.map (\( px, py ) -> ( px + x, py + y ))
                }

        Path rec ->
            Path
                { rec
                    | offset = Tuple.mapBoth (\ox -> ox + x) (\oy -> oy + y) rec.offset
                }

        Line rec ->
            Line
                { rec
                    | x0 = rec.x0 + x
                    , y0 = rec.y0 + y
                    , x1 = rec.x1 + x
                    , y1 = rec.y1 + y
                }
