module SubData exposing (getUserImage)

import Data
import Dict


getUserImage : Dict.Dict String String -> Data.User -> Maybe String
getUserImage imageFileBlobDict userPublic =
    let
        (Data.FileHash hash) =
            userPublic.imageHash
    in
    Dict.get hash imageFileBlobDict
