module Data.String.ANSI
  ( ANSIColor(..), toColor
  , toBlue, toGreen, toMagenta, toRed, toCyan, toYellow
  ) where

data ANSIColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White

toRed :: String -> String
toRed = toColor Red

toGreen :: String -> String
toGreen = toColor Green

toYellow :: String -> String
toYellow = toColor Yellow

toBlue :: String -> String
toBlue = toColor Blue

toMagenta :: String -> String
toMagenta = toColor Magenta

toCyan :: String -> String
toCyan = toColor Cyan

toColor :: ANSIColor -> String -> String
toColor color s = colorStr ++ s ++ resetStr
  where colorStr = case color of
          Black   -> "\ESC[90m"
          Red     -> "\ESC[91m"
          Green   -> "\ESC[92m"
          Yellow  -> "\ESC[93m"
          Blue    -> "\ESC[94m"
          Magenta -> "\ESC[95m"
          Cyan    -> "\ESC[96m"
          White   -> "\ESC[97m"
        resetStr = "\ESC[0m"
