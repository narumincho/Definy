module Data.Label exposing
    ( fromString
    , toCapitalString
    )

{-| あらゆるところでものを識別するために使うラベル
先頭1文字は、大文字か小文字のアルファベット(a-z/A-Z)
それ以外は、大文字と小文字のアルファベットと、数字(a-z,A-Z,0-9)
Parserの中間表現でも使う

  - 名前 先頭小文字
  - 型名 先頭大文字
  - 型コンストラクタ名 先頭大文字
  - レコードのフィールド名 先頭小文字

に使われる

長さは1～64文字
26x((26x2+10)^63)=2.1659567799854273e+114パターンの名前をつくることが可能

6bit(64通り)で1文字表現可能
64文字なら384bit,48byte
そのままのasciiで表現するなら64byte
バイト表現は、やはり短い名前が多いので
[(長さ-1)6bit][1文字6bit][1文字6bit]..
a は |000000|000000|
abcは |000010|000000|000001|000010|
と表現できる

-}


fromString : String -> String
fromString text =
    (String.foldl
        fromStringLoop
        { isFirstChar = True, isBeforeSpace = False, text = "" }
        text
    ).text


type alias LoopState =
    { isFirstChar : Bool, isBeforeSpace : Bool, text : String }


fromStringLoop : Char -> LoopState -> LoopState
fromStringLoop char state =
    if state.isFirstChar then
        case toHeadSimpleChar char of
            Just c ->
                { isFirstChar = False
                , isBeforeSpace = False
                , text = String.fromChar (Char.toLower c)
                }

            Nothing ->
                { isFirstChar = True
                , isBeforeSpace = False
                , text = ""
                }

    else
        case toOthersSimpleChar char of
            Just c ->
                { isFirstChar = False
                , isBeforeSpace = False
                , text =
                    state.text
                        ++ String.fromChar
                            (if state.isBeforeSpace then
                                Char.toUpper c

                             else
                                c
                            )
                }

            Nothing ->
                { isFirstChar = False
                , isBeforeSpace = True
                , text = state.text
                }


{-| [a-z] のシンプルな文字に変換する. 失敗したらNothing
-}
toHeadSimpleChar : Char -> Maybe Char
toHeadSimpleChar char =
    let
        f =
            String.contains (String.fromChar char)
    in
    if f "aａ" then
        Just 'a'

    else if f "bｂ" then
        Just 'b'

    else if f "cｃ" then
        Just 'c'

    else if f "dｄ" then
        Just 'd'

    else if f "eｅ" then
        Just 'e'

    else if f "fｆ" then
        Just 'f'

    else if f "gｇ" then
        Just 'g'

    else if f "hｈ" then
        Just 'h'

    else if f "iｉ" then
        Just 'i'

    else if f "jｊ" then
        Just 'j'

    else if f "kｋ" then
        Just 'k'

    else if f "lｌ" then
        Just 'l'

    else if f "mｍ" then
        Just 'm'

    else if f "nｎ" then
        Just 'n'

    else if f "oｏ" then
        Just 'o'

    else if f "pｐ" then
        Just 'p'

    else if f "qｑ" then
        Just 'q'

    else if f "rｒ" then
        Just 'r'

    else if f "sｓ" then
        Just 's'

    else if f "tｔ" then
        Just 't'

    else if f "uｕ" then
        Just 'u'

    else if f "vｖ" then
        Just 'v'

    else if f "wｗ" then
        Just 'w'

    else if f "xｘ" then
        Just 'x'

    else if f "yｙ" then
        Just 'y'

    else if f "zｚ" then
        Just 'z'

    else
        Nothing


toOthersSimpleChar : Char -> Maybe Char
toOthersSimpleChar char =
    case toHeadSimpleChar char of
        Just c ->
            Just c

        Nothing ->
            toCapitalAlphabetAndNumber char


toCapitalAlphabetAndNumber : Char -> Maybe Char
toCapitalAlphabetAndNumber char =
    let
        f =
            String.contains (String.fromChar char)
    in
    if f "AＡ" then
        Just 'A'

    else if f "BＢ" then
        Just 'B'

    else if f "CＣ" then
        Just 'c'

    else if f "DＤ" then
        Just 'D'

    else if f "EＥ" then
        Just 'E'

    else if f "FＦ" then
        Just 'F'

    else if f "GＧ" then
        Just 'G'

    else if f "HＨ" then
        Just 'H'

    else if f "IＩ" then
        Just 'I'

    else if f "JＪ" then
        Just 'J'

    else if f "KＫ" then
        Just 'K'

    else if f "LＬ" then
        Just 'L'

    else if f "MＭ" then
        Just 'M'

    else if f "NＮ" then
        Just 'N'

    else if f "OＯ" then
        Just 'O'

    else if f "PＰ" then
        Just 'P'

    else if f "QＱ" then
        Just 'Q'

    else if f "RＲ" then
        Just 'R'

    else if f "SＳ" then
        Just 'S'

    else if f "TＴ" then
        Just 'T'

    else if f "UＵ" then
        Just 'U'

    else if f "VＶ" then
        Just 'V'

    else if f "WＷ" then
        Just 'W'

    else if f "XＸ" then
        Just 'X'

    else if f "YＹ" then
        Just 'Y'

    else if f "ZＺ" then
        Just 'Z'

    else if f "0０" then
        Just '0'

    else if f "1１" then
        Just '1'

    else if f "2２" then
        Just '2'

    else if f "3３" then
        Just '3'

    else if f "4４" then
        Just '4'

    else if f "5５" then
        Just '5'

    else if f "6６" then
        Just '6'

    else if f "7７" then
        Just '7'

    else if f "8８" then
        Just '8'

    else if f "9９" then
        Just '9'

    else
        Nothing


toCapitalString : String -> String
toCapitalString label =
    case String.uncons label of
        Just ( head, others ) ->
            String.cons (Char.toUpper head) others

        Nothing ->
            label
