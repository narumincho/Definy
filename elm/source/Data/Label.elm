module Data.Label exposing
    ( Head
    , Label
    , Others
    , from
    , fromHead
    , fromString
    , ha
    , hb
    , hc
    , hd
    , he
    , hf
    , hg
    , hh
    , hi
    , hj
    , hk
    , hl
    , hm
    , hn
    , ho
    , hp
    , hq
    , hr
    , hs
    , ht
    , hu
    , hv
    , hw
    , hx
    , hy
    , hz
    , o0
    , o1
    , o2
    , o3
    , o4
    , o5
    , o6
    , o7
    , o8
    , o9
    , oA
    , oB
    , oC
    , oD
    , oE
    , oF
    , oG
    , oH
    , oI
    , oJ
    , oK
    , oL
    , oM
    , oN
    , oO
    , oP
    , oQ
    , oR
    , oS
    , oT
    , oU
    , oV
    , oW
    , oX
    , oY
    , oZ
    , oa
    , ob
    , oc
    , od
    , oe
    , of_
    , og
    , oh
    , oi
    , oj
    , ok
    , ol
    , om
    , on
    , oo
    , op
    , oq
    , or
    , os
    , ot
    , otherToHead
    , othersToChar
    , ou
    , ov
    , ow
    , ox
    , oy
    , oz
    , push
    , toCapitalString
    , toSmallString
    )

import Utility


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
type Label
    = Label Head (List Others)


from : Head -> List Others -> Label
from head othersList =
    Label head (List.take 63 othersList)


fromHead : Head -> Label
fromHead head =
    Label head []


push : Others -> Label -> Label
push others (Label head othersList) =
    if List.length othersList < 63 then
        Label head (othersList ++ [ others ])

    else
        Label head othersList


toCapitalString : Label -> String
toCapitalString (Label (Head head) others) =
    String.fromList
        (alphabetToCapitalChar head
            :: (others |> List.map othersToChar)
        )


toSmallString : Label -> String
toSmallString (Label (Head head) others) =
    String.fromList
        (alphabetToSmallChar head
            :: (others |> List.map othersToChar)
        )


othersToChar : Others -> Char
othersToChar others =
    case others of
        Capital alphabet ->
            alphabetToCapitalChar alphabet

        Small alphabet ->
            alphabetToSmallChar alphabet

        Digits digits ->
            digitsToChar digits


alphabetToCapitalChar : Alphabet -> Char
alphabetToCapitalChar alphabet =
    case alphabet of
        A ->
            'A'

        B ->
            'B'

        C ->
            'C'

        D ->
            'D'

        E ->
            'E'

        F ->
            'F'

        G ->
            'G'

        H ->
            'H'

        I ->
            'I'

        J ->
            'J'

        K ->
            'K'

        L ->
            'L'

        M ->
            'M'

        N ->
            'N'

        O ->
            'O'

        P ->
            'P'

        Q ->
            'Q'

        R ->
            'R'

        S ->
            'S'

        T ->
            'T'

        U ->
            'U'

        V ->
            'V'

        W ->
            'W'

        X ->
            'X'

        Y ->
            'Y'

        Z ->
            'Z'


alphabetToSmallChar : Alphabet -> Char
alphabetToSmallChar alphabet =
    case alphabet of
        A ->
            'a'

        B ->
            'b'

        C ->
            'c'

        D ->
            'd'

        E ->
            'e'

        F ->
            'f'

        G ->
            'g'

        H ->
            'h'

        I ->
            'i'

        J ->
            'j'

        K ->
            'k'

        L ->
            'l'

        M ->
            'm'

        N ->
            'n'

        O ->
            'o'

        P ->
            'p'

        Q ->
            'q'

        R ->
            'r'

        S ->
            's'

        T ->
            't'

        U ->
            'u'

        V ->
            'v'

        W ->
            'w'

        X ->
            'x'

        Y ->
            'y'

        Z ->
            'z'


digitsToChar : Digits -> Char
digitsToChar digits =
    case digits of
        N0 ->
            '0'

        N1 ->
            '1'

        N2 ->
            '2'

        N3 ->
            '3'

        N4 ->
            '4'

        N5 ->
            '5'

        N6 ->
            '6'

        N7 ->
            '7'

        N8 ->
            '8'

        N9 ->
            '9'


otherToHead : Others -> Maybe Head
otherToHead others =
    case others of
        Capital alphabet ->
            Just (Head alphabet)

        Small alphabet ->
            Just (Head alphabet)

        Digits _ ->
            Nothing


type Head
    = Head Alphabet


type Others
    = Capital Alphabet
    | Small Alphabet
    | Digits Digits


ha : Head
ha =
    Head A


hb : Head
hb =
    Head B


hc : Head
hc =
    Head C


hd : Head
hd =
    Head D


he : Head
he =
    Head E


hf : Head
hf =
    Head F


hg : Head
hg =
    Head G


hh : Head
hh =
    Head H


hi : Head
hi =
    Head I


hj : Head
hj =
    Head J


hk : Head
hk =
    Head K


hl : Head
hl =
    Head L


hm : Head
hm =
    Head M


hn : Head
hn =
    Head N


ho : Head
ho =
    Head O


hp : Head
hp =
    Head P


hq : Head
hq =
    Head Q


hr : Head
hr =
    Head R


hs : Head
hs =
    Head S


ht : Head
ht =
    Head T


hu : Head
hu =
    Head U


hv : Head
hv =
    Head V


hw : Head
hw =
    Head W


hx : Head
hx =
    Head X


hy : Head
hy =
    Head Y


hz : Head
hz =
    Head Z


oA : Others
oA =
    Capital A


oB : Others
oB =
    Capital B


oC : Others
oC =
    Capital C


oD : Others
oD =
    Capital D


oE : Others
oE =
    Capital E


oF : Others
oF =
    Capital F


oG : Others
oG =
    Capital G


oH : Others
oH =
    Capital H


oI : Others
oI =
    Capital I


oJ : Others
oJ =
    Capital J


oK : Others
oK =
    Capital K


oL : Others
oL =
    Capital L


oM : Others
oM =
    Capital M


oN : Others
oN =
    Capital N


oO : Others
oO =
    Capital O


oP : Others
oP =
    Capital P


oQ : Others
oQ =
    Capital Q


oR : Others
oR =
    Capital R


oS : Others
oS =
    Capital S


oT : Others
oT =
    Capital T


oU : Others
oU =
    Capital U


oV : Others
oV =
    Capital V


oW : Others
oW =
    Capital W


oX : Others
oX =
    Capital X


oY : Others
oY =
    Capital Y


oZ : Others
oZ =
    Capital Z


oa : Others
oa =
    Small A


ob : Others
ob =
    Small B


oc : Others
oc =
    Small C


od : Others
od =
    Small D


oe : Others
oe =
    Small E


of_ : Others
of_ =
    Small F


og : Others
og =
    Small G


oh : Others
oh =
    Small H


oi : Others
oi =
    Small I


oj : Others
oj =
    Small J


ok : Others
ok =
    Small K


ol : Others
ol =
    Small L


om : Others
om =
    Small M


on : Others
on =
    Small N


oo : Others
oo =
    Small O


op : Others
op =
    Small P


oq : Others
oq =
    Small Q


or : Others
or =
    Small R


os : Others
os =
    Small S


ot : Others
ot =
    Small T


ou : Others
ou =
    Small U


ov : Others
ov =
    Small V


ow : Others
ow =
    Small W


ox : Others
ox =
    Small X


oy : Others
oy =
    Small Y


oz : Others
oz =
    Small Z


o0 : Others
o0 =
    Digits N0


o1 : Others
o1 =
    Digits N1


o2 : Others
o2 =
    Digits N2


o3 : Others
o3 =
    Digits N3


o4 : Others
o4 =
    Digits N4


o5 : Others
o5 =
    Digits N5


o6 : Others
o6 =
    Digits N6


o7 : Others
o7 =
    Digits N7


o8 : Others
o8 =
    Digits N8


o9 : Others
o9 =
    Digits N9


type Alphabet
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z


type Digits
    = N0
    | N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9


fromString : String -> Maybe Label
fromString string =
    String.uncons string
        |> Maybe.andThen
            (\( head, others ) ->
                case
                    ( headFromChar head
                    , others
                        |> String.toList
                        |> Utility.takeAllWithFilter othersFromChar
                    )
                of
                    ( Just h, Just o ) ->
                        Just (Label h o)

                    ( _, _ ) ->
                        Nothing
            )


headFromChar : Char -> Maybe Head
headFromChar char =
    case char of
        'a' ->
            Just ha

        'b' ->
            Just hb

        'c' ->
            Just hc

        'd' ->
            Just hd

        'e' ->
            Just he

        'f' ->
            Just hf

        'g' ->
            Just hg

        'h' ->
            Just hh

        'i' ->
            Just hi

        'j' ->
            Just hj

        'k' ->
            Just hk

        'l' ->
            Just hl

        'm' ->
            Just hm

        'n' ->
            Just hn

        'o' ->
            Just ho

        'p' ->
            Just hp

        'q' ->
            Just hq

        'r' ->
            Just hr

        's' ->
            Just hs

        't' ->
            Just ht

        'u' ->
            Just hu

        'v' ->
            Just hv

        'w' ->
            Just hw

        'x' ->
            Just hx

        'y' ->
            Just hy

        'z' ->
            Just hz

        'A' ->
            Just ha

        'B' ->
            Just hb

        'C' ->
            Just hc

        'D' ->
            Just hd

        'E' ->
            Just he

        'F' ->
            Just hf

        'G' ->
            Just hg

        'H' ->
            Just hh

        'I' ->
            Just hi

        'J' ->
            Just hj

        'K' ->
            Just hk

        'L' ->
            Just hl

        'M' ->
            Just hm

        'N' ->
            Just hn

        'O' ->
            Just ho

        'P' ->
            Just hp

        'Q' ->
            Just hq

        'R' ->
            Just hr

        'S' ->
            Just hs

        'T' ->
            Just ht

        'U' ->
            Just hu

        'V' ->
            Just hv

        'W' ->
            Just hw

        'X' ->
            Just hx

        'Y' ->
            Just hy

        'Z' ->
            Just hz

        _ ->
            Nothing


othersFromChar : Char -> Maybe Others
othersFromChar char =
    case char of
        'a' ->
            Just oa

        'b' ->
            Just ob

        'c' ->
            Just oc

        'd' ->
            Just od

        'e' ->
            Just oe

        'f' ->
            Just of_

        'g' ->
            Just og

        'h' ->
            Just oh

        'i' ->
            Just oi

        'j' ->
            Just oj

        'k' ->
            Just ok

        'l' ->
            Just ol

        'm' ->
            Just om

        'n' ->
            Just on

        'o' ->
            Just oo

        'p' ->
            Just op

        'q' ->
            Just oq

        'r' ->
            Just or

        's' ->
            Just os

        't' ->
            Just ot

        'u' ->
            Just ou

        'v' ->
            Just ov

        'w' ->
            Just ow

        'x' ->
            Just ox

        'y' ->
            Just oy

        'z' ->
            Just oz

        'A' ->
            Just oA

        'B' ->
            Just oB

        'C' ->
            Just oC

        'D' ->
            Just oD

        'E' ->
            Just oE

        'F' ->
            Just oF

        'G' ->
            Just oG

        'H' ->
            Just oH

        'I' ->
            Just oI

        'J' ->
            Just oJ

        'K' ->
            Just oK

        'L' ->
            Just oL

        'M' ->
            Just oM

        'N' ->
            Just oN

        'O' ->
            Just oO

        'P' ->
            Just oP

        'Q' ->
            Just oQ

        'R' ->
            Just oR

        'S' ->
            Just oS

        'T' ->
            Just oT

        'U' ->
            Just oU

        'V' ->
            Just oV

        'W' ->
            Just oW

        'X' ->
            Just oX

        'Y' ->
            Just oY

        'Z' ->
            Just oZ

        '0' ->
            Just o0

        '1' ->
            Just o1

        '2' ->
            Just o2

        '3' ->
            Just o3

        '4' ->
            Just o4

        '5' ->
            Just o5

        '6' ->
            Just o6

        '7' ->
            Just o7

        '8' ->
            Just o8

        '9' ->
            Just o9

        _ ->
            Nothing
