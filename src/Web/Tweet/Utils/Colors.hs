module Web.Tweet.Utils.Colors where

import Text.PrettyPrint.ANSI.Leijen

toRed :: String -> String
toRed = show . red . text

toYellow :: String -> String
toYellow = show . dullyellow . text

toGreen :: String -> String
toGreen = show . dullgreen . text
