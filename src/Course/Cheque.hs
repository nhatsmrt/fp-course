{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars = process . preprocess
  where
    contains :: Chars -> Char -> Bool
    contains str c = case str of
      Nil -> False
      h :. t -> h == c || (contains t c)

    isDigit x = contains "0123456789" x
    isNonZeroDigit x = contains "123456789" x

    preprocess str = filter (\x -> x == '.' || isDigit x) str

    preprocessIntegerPart = (filter isDigit) . (dropWhile (not . isNonZeroDigit))

    -- Split the string by the dot
    splitByDot str = case break ((==) '.') str of
      (integerPart, h :. t) -> (integerPart, t)
      (integerPart, Nil) -> (integerPart, "")

    -- Convert String to Optional (List Digit)
    toDigits :: Chars -> List Digit
    toDigits str = (sequence (fromChar <$> str)) ?? Nil

    -- Augment fractional part
    normalizeFractionalPart :: Chars -> Chars
    normalizeFractionalPart fractionalPart = case fractionalPart of
      Nil -> "00"
      h :. Nil -> h :. "0"
      h1 :. h2 :. _ -> h1 :. h2 :. Nil -- truncate

    preprocessFractionalPart = normalizeFractionalPart . (filter isDigit)

    -- Chunk the digits
    chunkReversed :: List Digit -> List Digit3
    chunkReversed lst = case lst of
      Nil -> Nil
      h :. Nil -> (D1 h) :. Nil
      h1 :. h2 :. Nil -> (D2 h2 h1) :. Nil
      h1 :. h2 :. h3 :. t -> (D3 h3 h2 h1) :. (chunkReversed t)

    chunk = chunkReversed . reverse

    ties tens = case tens of
      Zero -> "zeroty" -- never used
      One -> "onety" -- never used
      Two -> "twenty"
      Three -> "thirty"
      Four -> "forty"
      Five -> "fifty"
      Six -> "sixty"
      Seven -> "seventy"
      Eight -> "eighty"
      Nine -> "ninety"

    teens dig = case dig of
      Zero -> "ten"
      One -> "eleven"
      Two -> "twelve"
      Three -> "thirteen"
      Four -> "fourteen"
      Five -> "fifteen"
      Six -> "sixteen"
      Seven -> "seventeen"
      Eight -> "eighteen"
      Nine -> "nineteen"


    processSingle :: Digit -> Chars
    processSingle x = case x of
      Zero -> ""
      _ -> showDigit x

    processTens :: Digit -> Digit -> Chars
    processTens x y = case (x, y) of
      (Zero, _) -> processSingle y
      (One, _) -> teens y
      -- x is not zero
      (_, Zero) -> (ties x)
      (_, _) -> (ties x) ++ "-" ++ (showDigit y)

    processHundreds :: Digit -> Digit -> Digit -> Chars
    processHundreds x y z = case (x, y, z) of
      (Zero, _, _) -> processTens y z
      _ -> let tens = processTens y z in let hundreds = (showDigit x) ++ " hundred" in
        if length tens > 0 then hundreds ++ " and " ++ tens else hundreds

    processChunk :: Digit3 -> Chars
    processChunk digits = case digits of
      D1 x -> processSingle x
      D2 x y -> processTens x y
      D3 x y z -> processHundreds x y z

    join :: List Chars -> Chars -> Chars
    join lst sep = case lst of
      Nil -> ""
      h1 :. Nil -> h1
      h1 :. h2 :. t -> h1 ++ sep ++ (join (h2 :. t) sep)

    processChunks :: List Digit3 -> Chars
    processChunks chunks = case processChunk <$> chunks of
      Nil -> "zero"
      h :. t ->
        let chunksWithPower = zip (h :. t) illion in
        let filteredEmptyChunks = filter (\(ch, power) -> length ch > 0) chunksWithPower in
        let augmentedFilteredEmptyChunks = if length filteredEmptyChunks == 0 then ("zero", "") :. Nil else filteredEmptyChunks in
        let concatChunkPower = (\(ch, power) -> if length power > 0 then ch ++ " " ++ power else ch) <$> augmentedFilteredEmptyChunks in
        let inOrderedProcessedChunk = reverse concatChunkPower in
        join inOrderedProcessedChunk " "


    processPart :: (Chars -> Chars) -> Chars -> Chars
    processPart preprocessFn = processChunks . chunk . toDigits . preprocessFn

    eq :: Eq a => List a -> List a -> Bool
    eq lst1 lst2 = case (lst1, lst2) of
      (Nil, Nil) -> True
      (h1 :. t1, h2 :. t2) -> (h1 == h2) && (eq t1 t2)
      _ -> False

    processIntegerPart :: Chars -> Chars
    processIntegerPart integerPart =
      let processedIntegerPart = processPart preprocessIntegerPart integerPart in
      let dollar = if eq processedIntegerPart "one" then "dollar" else "dollars" in
      processedIntegerPart ++ " " ++ dollar

    processFractionalPart :: Chars -> Chars
    processFractionalPart fractionalPart =
      let processedFractionalPart = processPart preprocessFractionalPart fractionalPart in
      let cents = if eq processedFractionalPart "one" then "cent" else "cents" in
      processedFractionalPart ++ " " ++ cents

    process :: Chars -> Chars
    process str =
      let (integerPart, fractionalPart) = splitByDot str in
      (processIntegerPart integerPart) ++ " and " ++ (processFractionalPart fractionalPart)
