{-# LANGUAGE OverloadedStrings #-}

module Goi.Util.Color ( bgBlack
                      , bgBlue
                      , bgCyan
                      , bgGreen
                      , bgMagenta
                      , bgRed
                      , bgWhite
                      , bgYellow
                      , colorize
                      , colorQuote
                      , fgBlack
                      , fgBlue
                      , fgCyan
                      , fgGreen
                      , fgMagenta
                      , fgRed
                      , fgWhite
                      , fgYellow ) where

import Data.Text (Text)
import qualified Data.Text as T

----------

colorize :: Text -> Text -> Text
colorize c t = T.concat [ c, t, reset, erase ]

colorQuote :: Text -> Text -> Text -> Text
colorQuote c t = (<> erase) . T.replace t (c <> t <> reset)

----------

-- TODO: Append "\STX" if using Haskeline for output. See "https://github.com/judah/haskeline/wiki/ControlSequencesInPrompt".

bgBlack :: Text
bgBlack = "\ESC[40m"

bgBlue :: Text
bgBlue = "\ESC[44m"

bgCyan :: Text
bgCyan = "\ESC[46m"

bgGreen :: Text
bgGreen = "\ESC[42m"

bgMagenta :: Text
bgMagenta = "\ESC[45m"

bgRed :: Text
bgRed = "\ESC[41m"

bgWhite :: Text
bgWhite = "\ESC[47m"

bgYellow :: Text
bgYellow = "\ESC[43m"

----------

fgBlack :: Text
fgBlack = "\ESC[30m"

fgBlue :: Text
fgBlue = "\ESC[34m"

fgCyan :: Text
fgCyan = "\ESC[36m"

fgGreen :: Text
fgGreen = "\ESC[32m"

fgMagenta :: Text
fgMagenta = "\ESC[35m"

fgRed :: Text
fgRed = "\ESC[31m"

fgWhite :: Text
fgWhite = "\ESC[37m"

fgYellow :: Text
fgYellow = "\ESC[33m"

----------

erase :: Text -- Erase from cursor to end of line.
erase = "\ESC[K"

reset :: Text -- Reset colors.
reset = "\ESC[0m"
