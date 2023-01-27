module Help (displayHelp) where

import Data.Char (toLower)
import Settings (HelpType (..))
import System.Environment (getProgName)

usage :: String
usage =
  "usage: %% options\n\
  \\n\
  \\n\
  \%Options:\n\
  \-i or --in-file <filename>:\n    The filename to read the input text from.\n\
  \-o or --out-file <filename>:\n    The filename to write the output text to. If not specified, the output is written to standard output.\n\
  \-v or --value <pattern>:\n    The pattern to use as input text.\n\
  \-t or --tag-filter <filter>:\n    A filter pattern to let git filter tag names by.\n\
  \-h or --help [subject]:\n    Show this help if no subject is provided, or help about the specified subject if it is provided.\n\
  \\n\
  \--in-file and --value must not both be specified, when neither are specified the input is read from standard input.\n\
  \\n\
  \The current git information will be substituted in the input text.\n\
  \The replaced text will be returned via the specified channel.\n\
  \\n\
  \\n\
  \%The following information can be retrieved:\n\
  \- Current tag.\n\
  \    %% --help tag for help on usage of this placeholder type.\n\
  \- Distance to the current tag (number of commits since the tag).\n\
  \    %% --help dist for help on usage of this placeholder type.\n\
  \- Revision hash (short version, 7 characters).\n\
  \    %% --help hash for help on usage of this placeholder type.\n\
  \- Dirty marker (indicates whether there are local changes).\n\
  \    %% --help dirty for help on usage of this placeholder type.\n\
  \- The current date and time.\n\
  \    %% --help date for help on usage of this placeholder type.\n\
  \    %% --help date-format for help on the available date formatting strings.\n\
  \\n\
  \\n\
  \%Placeholders:\n\
  \All placeholders are defined as a placeholder type followed by a number of named parameters. All parameters are optional.\n\
  \The syntax for a placeholder is as follows:\n\
  \<placeholder> ::= [hagri:<type><parameters>]\n\
  \<parameters>  ::= nothing | :<parameter><parameters>\n\
  \<parameter>   ::= <name>=<value>\n\
  \<name>        ::= tag | dist | hash | dirty | date\n\
  \<value>       ::= <text> | <replacement>\n\
  \\n\
  \Where <text> is a verbatim text that is used literally, and <replacement> is a text that contains wildcard characters ('*'), these wildcard characters are replaced by whatever the placeholder represents.\n\
  \\n\
  \Note that ':' and ']' will terminate a parameter. To use reserved characters inside a parameter value, they can be escaped using a backslash ('\\').\n\
  \If a parameter is defined multiple times, the leftmost occurence of the parameter will be used.\n\
  \\n\
  \For example, the following placeholder: [hagri:hash:len=10:len=11:nt=*:at=-*:ot=\\[--\\]]\n\
  \Will write: - The hash with length 10 if not on a tag.\n\
  \            - The hash prefixed with '-' if after a tag.\n\
  \            - '[--]' if on a tag."

tag :: String
tag =
  "%The tag placeholder.\n\
  \Displays the latest tag that matches the specified filter (if provided), that is before or at the current commit.\n\
  \\n\
  \%Available parameters:\n\
  \- ot:    A replacement pattern to use when on a tag. If not provided, parameter at is used.\n\
  \- at:    A replacement pattern to use when after a tag. If not provided, this placeholder is replaced with the tag itself.\n\
  \- nt:    A string to use when no tag is defined. If not provided, this placeholder is replaced with an empty string.\n\
  \- nv:    A string to use when the working directory is not inside a git repository. If not provided, parameter nt is used. If nt is not provided either, this placeholder is replaced with 'NOT_VERSIONED'.\n\
  \- start: Provides an index to use as a start for the substring of the tag. If not provided, the tag will be used from its first character.\n\
  \- len:   Provides a number to use as a length for the substring of the tag. If not provided, the tag will be used until its last character.\n\
  \\n\
  \%Example:\n\
  \Given the tag 'abcd',\n\
  \the placeholder '[hagri:tag:start=1:len=2:nt=+*:at=-*]'\n\
  \Will write: - '+*' when not on or after a tag.\n\
  \            - '-bc' when after a tag.\n\
  \            - '-bc' if on a tag (it falls back to the at placeholder)."

dist :: String
dist =
  "%The dist placeholder.\n\
  \Displays the distance of the current commit (in commits ahead of) the tag that matches the specified filter (if provided), that is before or at the current commit. Will be 0 if the current commit is on a tag that matches the filter.\n\
  \\n\
  \%Avalable parameters:\n\
  \- ot: A replacement pattern to use when on a tag. If not provided, parameter at is used.\n\
  \- at: A replacement pattern to use when after a tag. If not provided, this placeholder is replaced with the tag distance itself.\n\
  \- nt: A string to use when no tag is defined. If not provided, this placeholder is replaced with an empty string.\n\
  \- nv: A string to use when the working directory is not inside a git repository. If not provided, parameter nt is used. If nt is not provided either, this placeholder is replaced with 'NOT_VERSIONED'.\n\
  \\n\
  \%Example:\n\
  \The placeholder '[hagri:dist:nt=no tag:at=* after:ot=on tag]'\n\
  \Will write: - 'no tag' when not on or after a tag.\n\
  \            - '3 after' when (3 commits) after a tag.\n\
  \            - 'on tag' if on a tag."

hash :: String
hash =
  "%The hash placeholder.\n\
  \Displays the hash of the current commit.\n\
  \\n\
  \%Available parameters:\n\
  \- ot:  A replacement pattern to use when on a tag. If not provided, parameter at is used.\n\
  \- at:  A replacement pattern to use when after a tag. If not provided, parameter nt is used.\n\
  \- nt:  A replacement pattern to use when no tag is defined. If not provided, this placeholder is replaced the hash itself.\n\
  \- nv:  A string to use when the working directory is not inside a git repository. If not provided, this placeholder is replaced with 'NOT_VERSIONED'.\n\
  \- start: Provides an index to use as a start for the substring of the hash. If not provided, the hash will be used from its first character.\n\
  \- len: Provides a number to use as a length for the substring of the hash. If not provided, the default length of 7 characters is used.\n\
  \\n\
  \%Example:\n\
  \Given the tag 'abcdefgh',\n\
  \the placeholder '[hagri:hash:len=4:at=-*:nt=-:nv=?]'\n\
  \Will write: - '-' when not on or after a tag.\n\
  \            - '-abcd' when after a tag.\n\
  \            - 'abcd' if on a tag.\n\
  \            - '?' if not in a git repository."

dirty :: String
dirty =
  "%The dirty placeholder.\n\
  \Displays a string that indicates whether or not there are local changes to the checkout of the repository.\n\
  \\n\
  \- yes: A string to use when the repository is dirty, i.e. there are local changes. If not provided, this placeholder is replaced with 'dirty'.\n\
  \- no:  A string to use when the repository is clean, i.e. there are no local changes. If not provided, this placeholder is replaced with 'clean'.\n\
  \- nv:  A string to use when the working directory is not inside a git repository. If not provided, the value for a clean repository is used (parameter no).\n\
  \\n\
  \%Example:\n\
  \The placeholder '[hagri:dirty:yes=local changes:no=:nv=norep]'\n\
  \Will write: - 'local changes' when the there are local changes.\n\
  \            - '' when there are no local changes.\n\
  \            - 'norep' if not in a git repository."

date :: String
date =
  "%The date placeholder.\n\
  \Displays the current date and/or time.\n\
  \\n\
  \- format: The format to use for the date. If not provided this placeholder will be replaced by a standard short date string.\n\
  \          The default format is '{yyyy}-{MM}-{dd}', showing the full year, the month and day both padded to 2 characters.\n\
  \\n\
  \%Examples:\n\
  \The placeholder [hagri:date] will show a date like '2016-03-24'.\n\
  \The placeholder [hagri:date:format={yyyy}-{MM}-{dd}T{HH}\\:{mm}\\:{ss}{OO}] will show a date like '2016-03-24T13:20:01-04:00'.\n\
  \The placeholder [hagri:date:format={MMM}/{dd}/{yy} {hh}\\:{mm}\\:{ss} {tt}] will show a date like 'Mar/24/16 1:20:01 AM'.\n\
  \\n\
  \There are many more possible ways to format dates.\n\
  \Use %% --help date-format to show which formatting options are available for the date."

dateFormat :: String
dateFormat =
  "%Date formatting.\n\
  \The date format consists of a sequence of identifiers, each eclosed in curly brackets ('{' and '}').\n\
  \Any character can be escaped to prevent starting an identifier, by prefixing it with a backslash ('\\').\n\
  \Below is a list of all formatting identifiers that are available:\n\
  \\n\
  \%Time period:\n\
  \- {gg}      : The current period, i.e. 'B.C.' or 'A.D.'.\n\
  \- {g}       : The current period, without dots, i.e. 'BC' or 'AD'.\n\
  \%Year:\n\
  \- {yyyyy}   : The current year, as 5 digits, e.g. '02015'.\n\
  \- {yyyy}    : The current year, as 4 digits, e.g. '2015'.\n\
  \- {yyy}     : The current year, with at least 3 digits, e.g. '812', or '2015'.\n\
  \- {yy}      : The last two digits of the current year, e.g. '15'.\n\
  \- {y}       : The last two digits of the current year, with leading zero stripped, e.g. '7' or '15'.\n\
  \- {YYYYY}   : The current week year, as 5 digits, e.g. '02015'.\n\
  \- {YYYY}    : The current week year, as 4 digits, e.g. '2015'.\n\
  \- {YYY}     : The current week year, with at least 3 digits, e.g. '812', or '2015'.\n\
  \- {YY}      : The last two digits of the current week year, e.g. '15'.\n\
  \- {Y}       : The last two digits of the current week year, with leading zero stripped, e.g. '7' or '15'.\n\
  \%Month:\n\
  \- {MMMM}    : The full name of the month, e.g. 'February'.\n\
  \- {MMM}     : The abbreviated name of the month, e.g. 'Feb'.\n\
  \- {MM}      : The number of the month, padded to 2 digits, e.g. '02'.\n\
  \- {M}       : The number of the month, without padding, e.g. '2'.\n\
  \%Week:\n\
  \- {MM}      : The week of the year, padded to 2 digits, e.g. '08'.\n\
  \- {M}       : The week of the year, without padding, e.g. '8'.\n\
  \%Day:\n\
  \- {DD}      : The day of the year, padded to 2 digits, e.g. '04' or '213'.\n\
  \- {D}       : The day of the year, not padded, e.g. '4' or '213'.\n\
  \- {dd}      : The day of the month, padded to 2 digits, e.g. '03' or '13'.\n\
  \- {d}       : The day of the month, not padded, e.g. '3' or '13'.\n\
  \- {ww}      : The full name of the day of the week, e.g. 'Wednesday'.\n\
  \- {w}       : The abbreviated name of the day of the week, e.g. 'Wed'.\n\
  \- {wm}      : The numeric day of the week, where Monday is day 1, e.g. '3'.\n\
  \- {ws}      : The numeric day of the week, where Sunday is day 1, e.g. '4'.\n\
  \\n\
  \%Hour:\n\
  \- {hh}      : The hour of the day, in 12 hour format and padded to 2 digits, e.g. '05'.\n\
  \- {h}       : The hour of the day, in 12 hour format and not padded, e.g. '5'.\n\
  \- {HH}      : The hour of the day, in 24 hour format and padded to 2 digits, e.g. '17'.\n\
  \- {H}       : The hour of the day, in 24 hour format and not padded, e.g. '17'.\n\
  \%Minute:\n\
  \- {mm}      : The minute of the hour, padded to 2 digits, e.g. '09'.\n\
  \- {m}       : The minute of the hour, not padded, e.g. '9'.\n\
  \%Second:\n\
  \- {ss}      : The second of the minute, padded to 2 digits, e.g. '02'.\n\
  \- {s}       : The second of the minute, not padded, e.g. '2'.\n\
  \%Sub-second:\n\
  \- {ffffffff}: Hundred-millionths of a second, e.g. '12345678'.\n\
  \- {fffffff} : Ten-millionths of a second, e.g. '1234567'.\n\
  \- {ffffff}  : Millionths of a second, e.g. '123456'.\n\
  \- {fffff}   : Hundred-thousandths of a second, e.g. '12345'.\n\
  \- {ffff}    : Ten-thousandths of a second, e.g. '1234'.\n\
  \- {fff}     : Thousandths of a second, e.g. '123'.\n\
  \- {ff}      : Hundredths of a second, e.g. '12'.\n\
  \- {f}       : Tenths of a second, e.g. '1'.\n\
  \%Time zone:\n\
  \- {ZZ}      : 'Z' if on UTC, or the offset from UTC, separated by colons, e.g. 'Z' or '+03:00:'.\n\
  \- {Z}       : 'Z' if on UTC, or the offset from UTC, not separated, e.g. 'Z' or '+0300:'.\n\
  \- {OO}      : The offset from UTC, separated by colons, e.g. '+03:00:'.\n\
  \- {O}       : The offset from UTC, not separated, e.g. '+0300:'.\n\
  \- {TZZ}     : The name of the time zone, or the offset from UTC, separated by colons, e.g. 'EAT' or '+03:00'.\n\
  \- {TZ}      : The name of the time zone, or the offset from UTC, not separated, e.g. 'EAT' or '+0300'.\n\
  \%AM/PM:\n\
  \- {ttt}     : 'A.M.' or 'P.M.', including dots.\n\
  \- {tt}      : 'AM' or 'PM', including dots.\n\
  \- {t}       : 'A' or 'P' for AM or PM respectively."

-- | Displays the help message belonging to the specified topic. Or the main usage string.
displayHelp :: HelpType -> IO ()
displayHelp helpType = do
  programName <- getProgName
  let output = case helpType of
        Main -> usage
        Topic topic
          | (toLower <$> topic) == "tag" -> tag
          | (toLower <$> topic) == "dist" -> dist
          | (toLower <$> topic) == "hash" -> hash
          | (toLower <$> topic) == "dirty" -> dirty
          | (toLower <$> topic) == "date" -> date
          | (toLower <$> topic) == "date-format" -> dateFormat
          | otherwise -> usage
  putStrLn $ markupText programName $ output

markupText :: String -> String -> String
markupText programName str = unlines $ map markupLine $ replace "%%" programName <$> lines str
 where
  markupLine ('%' : line) = "\ESC[1m" ++ line ++ "\ESC[0m"
  markupLine line = line

  replace :: String -> String -> String -> String
  replace needle replacement haystack = go haystack
   where
    len = length needle

    go :: String -> String
    go [] = []
    go str | take len str == needle = replacement ++ (go $ drop len str)
    go (x : xs) = x : go xs
