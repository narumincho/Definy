module CommonUi exposing
    ( GutterMsg(..)
    , activeColor
    , button
    , closeIcon
    , codeFontTypeface
    , fontHack
    , fontHackName
    , gitHubIcon
    , googleIcon
    , horizontalGutter
    , maxWidthText
    , miniUserView
    , normalText
    , normalTypeface
    , plusIcon
    , sameLanguageLink
    , stretchText
    , subText
    , tabContainer
    , textColorStyle
    , timeView
    , userView
    , verticalGutter
    )

{-| Definyで使うUIのパネルを定義する
-}

import Css
import Data
import Data.TimeZoneAndName
import Data.UrlData
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import Message
import Time
import Ui
import VectorImage


{-| 基本的な文字色(#ddd rgb(221,221,221))を設定する
-}
textColorStyle : Css.Style
textColorStyle =
    Css.color (Css.rgb 221 221 221)


{-| 選択していることを表す色
-}
activeColor : Css.Color
activeColor =
    Css.rgb 240 153 54


{-| パネルの幅を変更するためにつかむところ | ガター
-}
verticalGutter : Bool -> Html.Styled.Html ()
verticalGutter isResizing =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            (if isResizing then
                [ Css.width (Css.px 2)
                , Css.flexShrink Css.zero
                , Css.after
                    [ Css.property "content" (Css.qt "")
                    , Css.display Css.block
                    , Css.height (Css.pct 100)
                    , Css.width (Css.px 6)
                    , Css.position Css.relative
                    , Css.left (Css.px -2)
                    , Css.zIndex (Css.int 10)
                    , Css.backgroundColor (Css.rgb 255 255 255)
                    ]
                ]

             else
                [ Css.width (Css.px 2)
                , Css.backgroundColor (Css.rgb 68 68 68)
                , Css.flexShrink Css.zero
                , Css.hover
                    [ Css.backgroundColor (Css.rgb 102 102 102) ]
                , Css.after
                    [ Css.property "content" (Css.qt "")
                    , Css.cursor Css.ewResize
                    , Css.display Css.block
                    , Css.height (Css.pct 100)
                    , Css.width (Css.px 12)
                    , Css.position Css.relative
                    , Css.left (Css.px -5)
                    , Css.zIndex (Css.int 10)
                    ]
                ]
            )
        , Html.Styled.Events.onMouseDown ()
        ]
        []


type GutterMsg
    = GutterMsgPointerEnter
    | GutterMsgPointerLeave
    | GutterMsgToResizeMode Ui.Pointer


{-| パネルの高さを変更するためにつかむところ - ガター
-}
horizontalGutter : Bool -> Html.Styled.Html ()
horizontalGutter isResizing =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            (if isResizing then
                [ Css.height (Css.px 2)
                , Css.flexShrink Css.zero
                , Css.after
                    [ Css.property "content" (Css.qt "")
                    , Css.display Css.block
                    , Css.width (Css.pct 100)
                    , Css.height (Css.px 6)
                    , Css.position Css.relative
                    , Css.top (Css.px -2)
                    , Css.zIndex (Css.int 10)
                    , Css.backgroundColor (Css.rgb 255 255 255)
                    ]
                ]

             else
                [ Css.height (Css.px 2)
                , Css.backgroundColor (Css.rgb 68 68 68)
                , Css.flexShrink Css.zero
                , Css.hover
                    [ Css.backgroundColor (Css.rgb 102 102 102) ]
                , Css.after
                    [ Css.property "content" (Css.qt "")
                    , Css.display Css.block
                    , Css.width (Css.pct 100)
                    , Css.height (Css.px 12)
                    , Css.position Css.relative
                    , Css.top (Css.px -5)
                    , Css.zIndex (Css.int 10)
                    ]
                ]
            )
        , Html.Styled.Events.onMouseDown ()
        ]
        []


normalTypeface : String
normalTypeface =
    "'Hiragino Kaku Gothic Pro', 'ヒラギノ角ゴ Pro W3', メイリオ, Meiryo, 'ＭＳ Ｐゴシック', sans-serif"


{-| よく使うテキスト. 文字サイズ,表示する文字を指定する. パネルのサイズは文字の大きさや量によって変わる
-}
normalText : Int -> String -> Ui.Panel message
normalText size text =
    Ui.text
        []
        (Ui.TextAttributes
            { text = text
            , typeface = normalTypeface
            , size = size
            , letterSpacing = 0
            , lineHeight = 1.2
            , color = Css.rgb 200 200 200
            , textAlignment = Ui.TextAlignCenter
            }
        )


{-| 横方向に広がるテキスト. テキスト自体は左詰め
-}
stretchText : Int -> String -> Ui.Panel message
stretchText size text =
    Ui.text
        [ Ui.width Ui.stretch ]
        (Ui.TextAttributes
            { text = text
            , typeface = normalTypeface
            , size = size
            , letterSpacing = 0
            , lineHeight = 1.2
            , color = Css.rgb 200 200 200
            , textAlignment = Ui.TextAlignStart
            }
        )


maxWidthText : Int -> Int -> String -> Ui.Panel message
maxWidthText maxWidth size text =
    Ui.text
        [ Ui.width (Ui.stretchWithMaxSize maxWidth) ]
        (Ui.TextAttributes
            { text = text
            , typeface = normalTypeface
            , size = size
            , letterSpacing = 0
            , lineHeight = 1.2
            , color = Css.rgb 200 200 200
            , textAlignment = Ui.TextAlignStart
            }
        )


{-| 灰色で横に伸び,右寄りのテキスト
-}
subText : String -> Ui.Panel message
subText text =
    Ui.text
        [ Ui.width Ui.stretch ]
        (Ui.TextAttributes
            { text = text
            , typeface = normalTypeface
            , size = 12
            , letterSpacing = 0
            , lineHeight = 1.2
            , color = Css.rgb 100 100 100
            , textAlignment = Ui.TextAlignEnd
            }
        )


{-| 同じ言語のページへのリンク
-}
sameLanguageLink : List Ui.Style -> Message.SubModel -> Data.Location -> Ui.Panel message -> Ui.Panel message
sameLanguageLink styleList subModel location =
    Ui.link
        styleList
        (Data.UrlData.urlDataToUrl
            { clientMode = Message.getClientMode subModel
            , language = Message.getLanguage subModel
            , location = location
            }
        )


codeFontTypeface : String
codeFontTypeface =
    "Hack"


timeView : Message.SubModel -> Data.Time -> Ui.Panel message
timeView subModel time =
    let
        posix =
            timeToPosix time
    in
    case Message.getTimeZoneAndNameMaybe subModel of
        Just timeZoneAndName ->
            timeViewWithTimeZoneAndName timeZoneAndName posix

        Nothing ->
            normalText 16 (utcTimeToString posix)


timeViewWithTimeZoneAndName : Data.TimeZoneAndName.TimeZoneAndName -> Time.Posix -> Ui.Panel message
timeViewWithTimeZoneAndName timeZoneAndName posix =
    Ui.column
        []
        [ Ui.row [ Ui.gap 8 ]
            [ normalText 12 (Data.TimeZoneAndName.getTimeZoneName timeZoneAndName)
            , Ui.row [ Ui.gap 4 ]
                (timeToTimeTermList (Data.TimeZoneAndName.getTimeZone timeZoneAndName) posix
                    |> List.map timeTermView
                )
            ]
        , subText (utcTimeToString posix)
        ]


timeTermView : TimeTerm -> Ui.Panel message
timeTermView timeTerm =
    case timeTerm of
        Number term ->
            Ui.text
                []
                (Ui.TextAttributes
                    { text = term
                    , typeface = normalTypeface
                    , size = 16
                    , letterSpacing = 0
                    , lineHeight = 1
                    , color = Css.rgb 200 200 200
                    , textAlignment = Ui.TextAlignEnd
                    }
                )

        Sign term ->
            Ui.text
                []
                (Ui.TextAttributes
                    { text = term
                    , typeface = normalTypeface
                    , size = 12
                    , letterSpacing = 0
                    , lineHeight = 1
                    , color = Css.rgb 160 160 160
                    , textAlignment = Ui.TextAlignEnd
                    }
                )


timeToPosix : Data.Time -> Time.Posix
timeToPosix time =
    Time.millisToPosix (time.day * 1000 * 60 * 60 * 24 + time.millisecond)


utcTimeToString : Time.Posix -> String
utcTimeToString posix =
    String.concat
        [ String.fromInt (Time.toYear Time.utc posix)
        , "-"
        , monthToString (Time.toMonth Time.utc posix)
        , "-"
        , String.padLeft 2 '0' (String.fromInt (Time.toDay Time.utc posix))
        , "T"
        , String.padLeft 2 '0' (String.fromInt (Time.toHour Time.utc posix))
        , ":"
        , String.padLeft 2 '0' (String.fromInt (Time.toMinute Time.utc posix))
        , ":"
        , String.padLeft 2 '0' (String.fromInt (Time.toSecond Time.utc posix))
        , "."
        , String.padLeft 3 '0' (String.fromInt (Time.toMillis Time.utc posix))
        , "Z"
        ]


timeToTimeTermList : Time.Zone -> Time.Posix -> List TimeTerm
timeToTimeTermList zone posix =
    [ Number (String.fromInt (Time.toYear zone posix))
    , Sign "/"
    , Number (monthToString (Time.toMonth zone posix))
    , Sign "/"
    , Number (String.padLeft 2 '0' (String.fromInt (Time.toDay zone posix)))
    , Sign ""
    , Number (String.padLeft 2 '0' (String.fromInt (Time.toHour zone posix)))
    , Sign ":"
    , Number (String.padLeft 2 '0' (String.fromInt (Time.toMinute zone posix)))
    , Sign ":"
    , Number (String.padLeft 2 '0' (String.fromInt (Time.toSecond zone posix)))
    , Sign "."
    , Number (String.padLeft 3 '0' (String.fromInt (Time.toMillis zone posix)))
    ]


type TimeTerm
    = Number String
    | Sign String


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


tabContainer : a -> List ( a, String ) -> Html.Styled.Html a
tabContainer selected allValues =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.property "display" "grid"
            , Css.boxShadow4 Css.zero (Css.px 2) (Css.px 4) (Css.rgba 0 0 0 0.4)
            , Css.position Css.relative
            , Css.property "grid-template-columns"
                (List.repeat (List.length allValues) "1fr"
                    |> String.join " "
                )
            ]
        ]
        (allValues |> List.map (tabItem selected))


tabItem : a -> ( a, String ) -> Html.Styled.Html a
tabItem selected ( item, text ) =
    Html.Styled.button
        [ Html.Styled.Attributes.css
            [ Css.padding (Css.px 8)
            , Css.textAlign Css.center
            , Css.borderBottom3
                (Css.px 2)
                Css.solid
                (if item == selected then
                    Css.rgb 78 201 176

                 else
                    Css.rgba 0 0 0 0
                )
            , Css.color
                (if item == selected then
                    Css.rgb 238 238 238

                 else
                    Css.rgb 204 204 204
                )
            , Css.hover
                [ Css.backgroundColor (Css.rgb 17 17 17)
                , textColorStyle
                ]
            ]
        , Html.Styled.Events.stopPropagationOn "click" (Json.Decode.succeed ( item, True ))
        ]
        [ Html.Styled.text text ]


fontHack : Css.Style
fontHack =
    let
        font =
            Css.sansSerif
    in
    Css.fontFamily { font | value = fontHackName }


fontHackName : String
fontHackName =
    "Hack"


gitHubIcon : Css.Color -> Ui.Panel msg
gitHubIcon backgroundColor =
    Ui.vectorImage
        [ Ui.width (Ui.fix 48)
        , Ui.height (Ui.fix 48)
        , Ui.borderRadius (Ui.BorderRadiusPx 8)
        , Ui.padding 8
        , Ui.backgroundColor backgroundColor
        ]
        (Ui.VectorImageAttributes
            { fitStyle = Ui.Contain
            , viewBox = { x = 0, y = 0, width = 20, height = 20 }
            , elements =
                [ VectorImage.path
                    "M10 0C4.476 0 0 4.477 0 10c0 4.418 2.865 8.166 6.84 9.49.5.09.68-.218.68-.483 0-.237-.007-.866-.012-1.7-2.782.603-3.37-1.34-3.37-1.34-.454-1.157-1.11-1.464-1.11-1.464-.907-.62.07-.608.07-.608 1.003.07 1.53 1.03 1.53 1.03.893 1.53 2.342 1.087 2.912.83.09-.645.35-1.085.634-1.335-2.22-.253-4.555-1.11-4.555-4.943 0-1.09.39-1.984 1.03-2.683-.105-.253-.448-1.27.096-2.647 0 0 .84-.268 2.75 1.026C8.294 4.95 9.15 4.84 10 4.836c.85.004 1.705.115 2.504.337 1.91-1.294 2.747-1.026 2.747-1.026.548 1.377.204 2.394.1 2.647.64.7 1.03 1.592 1.03 2.683 0 3.842-2.34 4.687-4.566 4.935.36.308.678.92.678 1.852 0 1.336-.01 2.415-.01 2.743 0 .267.18.578.687.48C17.14 18.163 20 14.417 20 10c0-5.522-4.478-10-10-10"
                    VectorImage.strokeNone
                    (VectorImage.fillColor (Css.rgb 0 0 0))
                ]
            }
        )


googleIcon : Css.Color -> Ui.Panel msg
googleIcon backgroundColor =
    Ui.vectorImage
        [ Ui.width (Ui.fix 48)
        , Ui.height (Ui.fix 48)
        , Ui.borderRadius (Ui.BorderRadiusPx 8)
        , Ui.padding 8
        , Ui.backgroundColor backgroundColor
        ]
        (Ui.VectorImageAttributes
            { fitStyle = Ui.Contain
            , viewBox = { x = 0, y = 0, width = 20, height = 20 }
            , elements =
                [ VectorImage.path
                    "M19.6 10.23c0-.82-.1-1.42-.25-2.05H10v3.72h5.5c-.15.96-.74 2.31-2.04 3.22v2.45h3.16c1.89-1.73 2.98-4.3 2.98-7.34z"
                    VectorImage.strokeNone
                    (VectorImage.fillColor (Css.rgb 66 133 244))
                , VectorImage.path
                    "M13.46 15.13c-.83.59-1.96 1-3.46 1-2.64 0-4.88-1.74-5.68-4.15H1.07v2.52C2.72 17.75 6.09 20 10 20c2.7 0 4.96-.89 6.62-2.42l-3.16-2.45z"
                    VectorImage.strokeNone
                    (VectorImage.fillColor (Css.rgb 52 168 83))
                , VectorImage.path
                    "M3.99 10c0-.69.12-1.35.32-1.97V5.51H1.07A9.973 9.973 0 0 0 0 10c0 1.61.39 3.14 1.07 4.49l3.24-2.52c-.2-.62-.32-1.28-.32-1.97z"
                    VectorImage.strokeNone
                    (VectorImage.fillColor (Css.rgb 251 188 5))
                , VectorImage.path
                    "M10 3.88c1.88 0 3.13.81 3.85 1.48l2.84-2.76C14.96.99 12.7 0 10 0 6.09 0 2.72 2.25 1.07 5.51l3.24 2.52C5.12 5.62 7.36 3.88 10 3.88z"
                    VectorImage.strokeNone
                    (VectorImage.fillColor (Css.rgb 234 67 53))
                ]
            }
        )


closeIcon : Ui.Panel message
closeIcon =
    Ui.vectorImage
        [ Ui.padding 8 ]
        (Ui.VectorImageAttributes
            { fitStyle = Ui.Contain
            , viewBox = { x = 0, y = 0, width = 10, height = 10 }
            , elements =
                [ VectorImage.line ( 1, 1 ) ( 9, 9 ) (VectorImage.strokeColor (Css.rgb 0 0 0))
                , VectorImage.line ( 9, 1 ) ( 1, 9 ) (VectorImage.strokeColor (Css.rgb 0 0 0))
                ]
            }
        )


plusIcon : Ui.Panel message
plusIcon =
    Ui.vectorImage
        [ Ui.width (Ui.fix 32)
        , Ui.height (Ui.fix 32)
        ]
        (Ui.VectorImageAttributes
            { fitStyle = Ui.Contain
            , viewBox = { x = 0, y = 0, width = 10, height = 10 }
            , elements =
                [ VectorImage.line ( 0, 5 ) ( 10, 5 ) (VectorImage.strokeColor (Css.rgb 200 200 200))
                , VectorImage.line ( 5, 0 ) ( 5, 10 ) (VectorImage.strokeColor (Css.rgb 200 200 200))
                ]
            }
        )


userView : Message.SubModel -> Data.UserId -> Ui.Panel message
userView subModel userId =
    let
        (Data.UserId userIdAsString) =
            userId
    in
    case Message.getUserSnapshot userId subModel of
        Just (Just userSnapshot) ->
            sameLanguageLink
                [ Ui.width Ui.stretch, Ui.backgroundColor (Css.rgb 20 20 20), Ui.padding 8 ]
                subModel
                (Data.LocationUser userId)
                (Ui.row
                    [ Ui.width Ui.stretch, Ui.gap 8 ]
                    [ case Message.getImageBlobUrl userSnapshot.imageHash subModel of
                        Just blobUrl ->
                            Ui.bitmapImage
                                [ Ui.width (Ui.fix 24)
                                , Ui.height (Ui.fix 24)
                                , Ui.borderRadius (Ui.BorderRadiusPercent 50)
                                ]
                                (Ui.BitmapImageAttributes
                                    { url = blobUrl
                                    , fitStyle = Ui.Contain
                                    , alternativeText = userSnapshot.name ++ "の画像"
                                    , rendering = Ui.ImageRenderingPixelated
                                    }
                                )

                        Nothing ->
                            Ui.empty
                                [ Ui.width (Ui.fix 24), Ui.height (Ui.fix 24) ]
                    , normalText 16 userSnapshot.name
                    , subText userIdAsString
                    ]
                )

        Just Nothing ->
            userNotFoundView

        Nothing ->
            normalText 16 ("userId=" ++ userIdAsString)


userNotFoundView : Ui.Panel message
userNotFoundView =
    normalText 16 "不明なユーザー"


miniUserView : Message.SubModel -> Data.UserId -> Ui.Panel message
miniUserView subModel userId =
    let
        (Data.UserId userIdAsString) =
            userId
    in
    case Message.getUserSnapshot userId subModel of
        Just (Just userSnapshot) ->
            sameLanguageLink
                [ Ui.backgroundColor (Css.rgb 20 20 20), Ui.padding 8 ]
                subModel
                (Data.LocationUser userId)
                (Ui.column
                    [ Ui.gap 8 ]
                    [ Ui.row
                        []
                        [ case Message.getImageBlobUrl userSnapshot.imageHash subModel of
                            Just blobUrl ->
                                Ui.bitmapImage
                                    [ Ui.width (Ui.fix 24)
                                    , Ui.height (Ui.fix 24)
                                    , Ui.borderRadius (Ui.BorderRadiusPercent 50)
                                    ]
                                    (Ui.BitmapImageAttributes
                                        { url = blobUrl
                                        , fitStyle = Ui.Contain
                                        , alternativeText = userSnapshot.name ++ "の画像"
                                        , rendering = Ui.ImageRenderingPixelated
                                        }
                                    )

                            Nothing ->
                                Ui.empty
                                    [ Ui.width (Ui.fix 24), Ui.height (Ui.fix 24) ]
                        , normalText 16 userSnapshot.name
                        ]
                    , subText userIdAsString
                    ]
                )

        Just Nothing ->
            userNotFoundView

        Nothing ->
            normalText 16 ("userId=" ++ userIdAsString)


button : message -> String -> Ui.Panel message
button message text =
    Ui.button
        [ Ui.backgroundColor (Css.rgb 40 40 40)
        , Ui.borderRadius (Ui.BorderRadiusPx 8)

        --, Ui.border
        --    (Ui.BorderStyle
        --        { color = Css.rgb 200 200 200
        --        , width = { top = 1, right = 1, left = 1, bottom = 1 }
        --        }
        --    )
        , Ui.padding 16
        ]
        message
        (Ui.text
            [ Ui.width Ui.auto, Ui.height Ui.auto ]
            (Ui.TextAttributes
                { text = text
                , typeface = normalTypeface
                , size = 24
                , letterSpacing = 0
                , lineHeight = 1
                , color = Css.rgb 200 200 200
                , textAlignment = Ui.TextAlignStart
                }
            )
        )
