module Format (green, yellow) where

import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

docToString :: ANSI.Doc -> String
docToString doc = ANSI.displayS (ANSI.renderPretty 0.4 80 doc) ""


green :: String -> String
green = colour . ANSI.green . ANSI.text

yellow :: String -> String
yellow = colour . ANSI.yellow . ANSI.text

colour :: ANSI.Doc -> String
colour = docToString
