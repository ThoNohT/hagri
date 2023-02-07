module TimeFormat (FormatSpecifier, parse, compile) where

import Control.Applicative (many)
import Data.Fixed (Pico, div', divMod', mod')
import Data.List (intersperse, singleton)
import Data.Maybe (fromJust)
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime, ZonedTime (..), isLeapYear, timeZoneOffsetString, toGregorian)
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.IO (unsafePerformIO)
import Shared.Parser (Parser, char, check, choice, escaped, pChar, pString, pSubString, runParser)
import Shared.Util (takeLast)
import Text.Printf (printf)

-- | The different types of calendars that can be worked with.
data Calendar = Gregorian | WeekYear deriving (Eq, Show)

-- | The ways in which an acronym can be displayed.
data AcronymFormat
  = Dots -- Dots after the letters.
  | NoDots -- No dots.
  deriving (Eq, Show)

-- | The ways in which a year can be displayed.
data YearFormat
  = Y5 -- Five digits.
  | Y4 -- Four digits.
  | Y3 -- At least 3 digits.
  | Y2 -- Exactly 2 digits.
  | Y1 -- At most 2 digits.
  deriving (Eq, Show)

-- | The ways in which a number with at most 2 digits can be displayed.
data TwoFormat
  = NoPad -- Don't pad to 2 digits.
  | Pad2 -- Pad to two digits.
  deriving (Eq, Show)

-- | The ways in which something that has a textual and numeric representation can be displayed.
data TextNumFormat a
  = Full -- Full textual format.
  | Short -- Short textual format.
  | Numeric a -- Numeric format.
  deriving (Eq, Show)

-- The ways in which a numeric day of the week can be displayed.
data DoWFormat
  = MondayFirst
  | SundayFirst
  deriving (Eq, Show)

-- | The ways in which an hour can be displayed.
data HourFormat
  = H24 -- 24 hour format.
  | H12 -- 12 hour format.
  deriving (Eq, Show)

-- | The ways in which a time offset can be decorated.
data OffsetDecoration
  = SeparateColon -- Separate hours and minutes with a colon.
  | NoDecoration -- Don't decorate.
  deriving (Eq, Show)

-- | The ways in which a timezone can be displayed.
data TimezoneFormat
  = ZOrOffset OffsetDecoration -- Z for UTC, or the offset.
  | Offset OffsetDecoration -- The offset.
  | TzName OffsetDecoration -- The name of the timezone, or offset if not available.
  deriving (Eq, Show)

-- | The ways in which the AM and PM specifiers can be displayed.
data AmPmFormat
  = TwoLetters AcronymFormat -- With two letters.
  | OneLetter -- With a single letter.
  deriving (Eq, Show)

-- | The possible contents of a time format specifier.
data FormatSpecifier
  = V String -- A verbatim string.
  | Period AcronymFormat -- Period of an era.
  | Year Calendar YearFormat -- Year.
  | MoY (TextNumFormat TwoFormat) -- Month of year.
  | WoY TwoFormat -- Week of year.
  | DoY TwoFormat -- Day of year.
  | DoM TwoFormat -- Day of month.
  | DoW (TextNumFormat DoWFormat) -- Day of week.
  | HoD TwoFormat HourFormat -- Hour of day.
  | MoH TwoFormat -- Minute of Hour.
  | SoM TwoFormat -- Second of Minute.
  | DaS Int -- Decimals after the second, to the specified precision.
  | TZ TimezoneFormat -- Timezone specifier.
  | AmPm AmPmFormat -- AM or PM.
  deriving (Eq, Show)

-- | Parses a string into a list of time format specifiers.
parse :: String -> [FormatSpecifier]
parse = fst . fromJust . runParser parser

-- | The Parser for FormatSpecifiers.
parser :: Parser [FormatSpecifier]
parser = foldr merge [] <$> many formatSpecifier
 where
  merge next [] = [next]
  merge (V v1) ((V v2) : xs) = V (v1 ++ v2) : xs
  merge next acc = next : acc

  formatSpecifier =
    let
      from res txt = res <$ pString txt

      -- Wraps a parser in delimiters
      delim p = pChar '{' *> p <* pChar '}'
     in
      choice
        [ V . singleton <$> escaped
        , -- Period
          delim $ Period Dots `from` "gg"
        , delim $ Period NoDots `from` "g"
        , -- Year
          delim $ Year Gregorian Y5 `from` "yyyyy"
        , delim $ Year Gregorian Y4 `from` "yyyy"
        , delim $ Year Gregorian Y3 `from` "yyy"
        , delim $ Year Gregorian Y2 `from` "yy"
        , delim $ Year Gregorian Y1 `from` "y"
        , delim $ Year WeekYear Y5 `from` "YYYYY"
        , delim $ Year WeekYear Y4 `from` "YYYY"
        , delim $ Year WeekYear Y3 `from` "YYY"
        , delim $ Year WeekYear Y2 `from` "YY"
        , delim $ Year WeekYear Y1 `from` "Y"
        , -- Month
          delim $ MoY Full `from` "MMMM"
        , delim $ MoY Short `from` "MMM"
        , delim $ MoY (Numeric Pad2) `from` "MM"
        , delim $ MoY (Numeric NoPad) `from` "M"
        , -- Week
          delim $ WoY Pad2 `from` "WW"
        , delim $ WoY NoPad `from` "W"
        , -- Day
          delim $ DoY Pad2 `from` "DD"
        , delim $ DoY NoPad `from` "D"
        , delim $ DoM Pad2 `from` "dd"
        , delim $ DoM NoPad `from` "d"
        , delim $ DoW Full `from` "ww"
        , delim $ DoW Short `from` "w"
        , delim $ DoW (Numeric MondayFirst) `from` "wm"
        , delim $ DoW (Numeric SundayFirst) `from` "ws"
        , -- Hour
          delim $ HoD Pad2 H12 `from` "hh"
        , delim $ HoD NoPad H12 `from` "h"
        , delim $ HoD Pad2 H24 `from` "HH"
        , delim $ HoD NoPad H24 `from` "H"
        , -- Minute
          delim $ MoH Pad2 `from` "mm"
        , delim $ MoH NoPad `from` "m"
        , -- Second
          delim $ SoM Pad2 `from` "ss"
        , delim $ SoM NoPad `from` "s"
        , -- Second decimals
          delim $ DaS <$> (length <$> pSubString "ffffffff")
        , --  Time zone
          delim $ TZ (ZOrOffset SeparateColon) `from` "ZZ"
        , delim $ TZ (ZOrOffset NoDecoration) `from` "Z"
        , delim $ TZ (Offset SeparateColon) `from` "OO"
        , delim $ TZ (Offset NoDecoration) `from` "O"
        , delim $ TZ (TzName SeparateColon) `from` "TZZ"
        , delim $ TZ (TzName NoDecoration) `from` "TZ"
        , -- AM/PM
          delim $ AmPm (TwoLetters Dots) `from` "ttt"
        , delim $ AmPm (TwoLetters NoDots) `from` "tt"
        , delim $ (AmPm OneLetter) `from` "t"
        , -- Anything else verbatim
          V . singleton <$> char
        ]

compile :: ZonedTime -> [FormatSpecifier] -> String
compile time = concatMap compile'
 where
  -- Gather necessary information about the current time.
  (gYear, month, dom) = toGregorian . localDay . zonedTimeToLocalTime $ time
  (wYear, week, dow) = toWeekDate . localDay . zonedTimeToLocalTime $ time
  doy = monthAndDayToDayOfYear (isLeapYear gYear) month dom
  TimeOfDay{todSec, todMin, todHour} = localTimeOfDay . zonedTimeToLocalTime $ time
  (sec :: Int, ps' :: Pico) = divMod' todSec 1
  ps :: Int = div' (ps' * 1000000000000) 1
  tz = zonedTimeZone time
  offset = timeZoneMinutes tz
  (oMin, oSec) = offset `divMod` 60

  offsetString :: OffsetDecoration -> String
  offsetString NoDecoration = printf "%+.02d%02d" oMin oSec
  offsetString SeparateColon = printf "%+.02d:%02d" oMin oSec

  twoPad :: TwoFormat -> Int -> String
  twoPad NoPad x = printf "%d" x
  twoPad Pad2 x = printf "%02d" x

  transformHours :: HourFormat -> Int -> Int
  transformHours H12 x = ((x - 1) `mod` 12) + 1
  transformHours H24 x = x

  addDots :: AcronymFormat -> String -> String
  addDots NoDots x = x
  addDots Dots x = intersperse '.' x <> "."

  compile' :: FormatSpecifier -> String
  compile' (V v) = v
  compile' (Period dots) = addDots dots $ if gYear >= 0 then "AD" else "BC"
  compile' (Year cal format) =
    let year = case cal of Gregorian -> gYear; WeekYear -> wYear
     in case format of
          Y5 -> printf "%05d" gYear
          Y4 -> printf "%04d" gYear
          Y3 -> printf "%03d" gYear
          Y2 -> takeLast 2 $ printf "%02d" gYear
          Y1 -> printf "%d" (gYear `mod` 100)
  compile' (MoY Full) = formatTime defaultTimeLocale "%B" time
  compile' (MoY Short) = formatTime defaultTimeLocale "%b" time
  compile' (MoY (Numeric pad)) = twoPad pad month
  compile' (WoY pad) = twoPad pad week
  compile' (DoY pad) = twoPad pad doy
  compile' (DoM pad) = twoPad pad dom
  compile' (DoW Full) = formatTime defaultTimeLocale "%A" time
  compile' (DoW Short) = formatTime defaultTimeLocale "%a" time
  compile' (DoW (Numeric MondayFirst)) = printf "%d" dow
  compile' (DoW (Numeric SundayFirst)) = printf "%d" $ (dow `mod` 7) + 1
  compile' (HoD pad hf) = twoPad pad $ transformHours hf todHour
  compile' (MoH pad) = twoPad pad $ todMin
  compile' (SoM pad) = twoPad pad $ sec
  compile' (DaS d) = take d $ printf "%12d" ps
  compile' (TZ (ZOrOffset decoration)) = if offset == 0 then "Z" else offsetString decoration
  compile' (TZ (Offset decoration)) = offsetString decoration
  compile' (TZ (TzName SeparateColon)) = formatTime defaultTimeLocale "%Z" time
  compile' (TZ (TzName NoDecoration)) = formatTime defaultTimeLocale "%EZ" time
  compile' (AmPm OneLetter) = if todHour < 12 then "A" else "P"
  compile' (AmPm (TwoLetters dots)) = addDots dots $ if todHour < 12 then "AM" else "PM"
