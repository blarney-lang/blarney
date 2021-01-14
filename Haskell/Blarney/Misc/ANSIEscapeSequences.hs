{-|
Module      : Blarney.Misc.ANSIEscapeSequences
Description : Subset of ANSI escape sequences used for rich terminal output
Copyright   : (c) Alexandre Joannou, 2021
License     : MIT
Stability   : experimental

This module implements a few helpers to assist with rich text output in blarney.
In no way is it aiming to be a complete set of commands. In fact, only some of
the Control Sequence Introducer subset of commands are provided, and the limited
information used was entirely leveraged from
https://en.wikipedia.org/wiki/ANSI_escape_code.

-}

module Blarney.Misc.ANSIEscapeSequences (
  -- * Control Sequence Introducer
  csi_pfx
  -- * ANSI control sequences
, cuu
, cud
, cuf
, cub
, cnl
, cpl
, cha
, cup
, ed
, el
, su
, sd
, hvp
, sgr
  -- * Select Graphic Rendition options
, sgr_rst
, sgr_bold
, sgr_faint
, sgr_italic
, sgr_underline
, sgr_slow_blink
, sgr_rapid_blink
, sgr_reverse
, sgr_conceal
, sgr_crossed_out
, sgr_fg_color
, sgr_bg_color
, sgr_fg_color_8
, sgr_bg_color_8
, sgr_fg_color_24
, sgr_bg_color_24
  -- * String wrapper
, bold
, faint
, italic
, underline
, slow_blink
, rapid_blink
, crossed_out
, red
, green
, yellow
, blue
, magenta
, cyan
, white
) where

import Prelude
import Data.List

-- | CSI command prefix
csi_pfx = "\x1b["

-- ANSI control sequences
-- | cursor up by n
cuu n = csi_pfx ++ show n ++ "A"
-- | cursor down by n
cud n = csi_pfx ++ show n ++ "B"
-- | cursor forward by n
cuf n = csi_pfx ++ show n ++ "C"
-- | cursor back by n
cub n = csi_pfx ++ show n ++ "D"
-- | cursor to start of n-th next line
cnl n = csi_pfx ++ show n ++ "E"
-- | cursor to start of n-th previous line
cpl n = csi_pfx ++ show n ++ "F"
-- | cursor to column n
cha n = csi_pfx ++ show n ++ "G"
-- | cursor to row i column j
cup i j = csi_pfx ++ show i ++ ";" ++ show j ++ "H"
-- | erase in display
ed n = csi_pfx ++ show n ++ "J"
-- | erase in line
el n = csi_pfx ++ show n ++ "K"
-- | scroll up
su n = csi_pfx ++ show n ++ "S"
-- | scroll down
sd n = csi_pfx ++ show n ++ "T"
-- | horizontal vertical pos
hvp i j = csi_pfx ++ show i ++ ";" ++ show j ++ "f"
-- | select graphic rendition
sgr args = csi_pfx ++ intercalate ";" args ++ "m"

-- sgr options
-- | reset graphic rendition
sgr_rst = "0"
-- | set graphic rendition to bold
sgr_bold = "1"
-- | set graphic rendition to faint
sgr_faint = "2"
-- | set graphic rendition to italics
sgr_italic = "3"
-- | set graphic rendition to underlined
sgr_underline = "4"
-- | set graphic rendition to slowly blinking
sgr_slow_blink = "5"
-- | set graphic rendition to rapidly blinking
sgr_rapid_blink = "6"
-- | set graphic rendition to swapped foreground and background colors
sgr_reverse = "7"
-- | set graphic rendition to hidden
sgr_conceal = "8"
-- | set graphic rendition to crossed out
sgr_crossed_out = "9"
-- | set the foreground color
sgr_fg_color n | n >= 30 && n <= 37 || n >= 90 && n <= 97 = show n
           | otherwise = error $ "Blarney.Misc.ANSIEscapeSequences: " ++
                                 "unsupported fg_color " ++ show n
-- | set the background color
sgr_bg_color n | n >= 40 && n <= 47 || n >= 100 && n <= 107 = show n
           | otherwise = error $ "Blarney.Misc.ANSIEscapeSequences: " ++
                                 "unsupported bg_color " ++ show n
-- | set the 8-bit foreground color, argument value:
--     0-  7:  standard colors (as in ESC [ 30–37 m)
--     8- 15:  high intensity colors (as in ESC [ 90–97 m)
--    16-231:  6 × 6 × 6 cube (216 colors): 16 + 36 × r + 6 × g + b
--             (0 ≤ r, g, b ≤ 5)
--   232-255:  grayscale from black to white in 24 steps
sgr_fg_color_8 n = "38;5;"++ show n
-- | same as 'fg_color_8' for background color
sgr_bg_color_8 n = "48;5;"++ show n
-- | set the 24-bit foreground color, with 8-bit red + green + blue component
sgr_fg_color_24 r g b = "38;2;"++ intercalate ";" (show <$> [r,g,b])
-- | set the 24-bit background color, with 8-bit red + green + blue component
sgr_bg_color_24 r g b = "48;2;"++ intercalate ";" (show <$> [r,g,b])

-- String wrapper
bold str = sgr [sgr_bold] ++ str ++ sgr [sgr_rst]
faint str = sgr [sgr_faint] ++ str ++ sgr [sgr_rst]
italic str = sgr [sgr_italic] ++ str ++ sgr [sgr_rst]
underline str = sgr [sgr_underline] ++ str ++ sgr [sgr_rst]
slow_blink str = sgr [sgr_slow_blink] ++ str ++ sgr [sgr_rst]
rapid_blink str = sgr [sgr_rapid_blink] ++ str ++ sgr [sgr_rst]
crossed_out str = sgr [sgr_crossed_out] ++ str ++ sgr [sgr_rst]
red str = sgr [sgr_fg_color 31] ++ str ++ sgr [sgr_rst]
green str = sgr [sgr_fg_color 32] ++ str ++ sgr [sgr_rst]
yellow str = sgr [sgr_fg_color 33] ++ str ++ sgr [sgr_rst]
blue str = sgr [sgr_fg_color 34] ++ str ++ sgr [sgr_rst]
magenta str = sgr [sgr_fg_color 35] ++ str ++ sgr [sgr_rst]
cyan str = sgr [sgr_fg_color 36] ++ str ++ sgr [sgr_rst]
white str = sgr [sgr_fg_color 37] ++ str ++ sgr [sgr_rst]
