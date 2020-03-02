
module Data.Unicode where

import WikiPrelude


isChinese :: Char -> Bool
isChinese c = 
    let x = fromEnum c
    in case x of 
        x | 19968 <= x && x < 40960 -> True
        x | 13312 <= x && x < 19904 -> True
        x | 131072 <= x && x < 173792 -> True
        x | 173824 <= x && x < 177984 -> True
        x | 177984 <= x && x < 178208 -> True
        _ -> False