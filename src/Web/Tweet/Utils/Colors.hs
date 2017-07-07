-- | Helper functions to color strings
module Web.Tweet.Utils.Colors where

import Text.PrettyPrint.ANSI.Leijen

--  😎

-- | Make a string red
toRed :: String -> String
toRed = show . dullred . text

-- | Make a string yellow
toYellow :: String -> String
toYellow = show . dullyellow . text

-- | Make a string green
toGreen :: String -> String
toGreen = show . dullgreen . text

-- | Make a string blue
toBlue :: String -> String
toBlue = show . underline . dullblue . text

-- | Make a string blue; no underlining.
toPlainBlue :: String -> String
toPlainBlue = show . dullblue . text
