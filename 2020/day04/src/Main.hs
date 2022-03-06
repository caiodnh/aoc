module Main where

import Data.List.Split 
import Data.Char ( isDigit )

type Field = (String, String)
type Passport = [Field]

readField :: String -> Field
readField = fmap tail . splitAt 3

fieldName :: Field -> String
fieldName = fst

readPassport :: String -> Passport
readPassport = map readField . words

readInput :: String -> [Passport]
readInput = map readPassport . splitOn "\n\n"

validatePassport :: Passport -> Bool 
validatePassport passport = allFields
  where
    fields :: [String]
    fields = map fieldName passport
    requiredFields :: [String]
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    allFields :: Bool
    allFields = foldr f True requiredFields
      where 
        f requiredField bool = requiredField `elem` fields && bool

validateField :: Field -> Bool 
validateField (name, entry)
  = case name of
      "byr" -> length entry == 4 &&
                      all isDigit entry &&
                      1920 <= read entry &&
                      read entry <= 2002
      "iyr" -> length entry == 4 &&
                      all isDigit entry &&
                      2010 <= read entry &&
                      read entry <= 2020
      "eyr" -> length entry == 4 &&
                      all isDigit entry &&
                      2020 <= read entry &&
                      read entry <= 2030
      "hgt" -> testHGT entry 
      "hcl" -> testHCL entry
      "ecl" -> entry `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      "pid" -> length entry == 9 && all isDigit entry 
      "cid" -> True
      _ -> False
      where
        testHCL ""     = False
        testHCL (c:cs) = c == '#' && all (`elem` (['0'..'9']++['a'..'f'])) cs
        testHGT cs = let (n, unit) = span isDigit cs in
          all isDigit n && case unit of
            "cm" -> 150 <= read n && read n <= 193
            "in" -> 59 <= read n && read n <= 76
            _ -> False

validatePassport2 :: Passport -> Bool 
validatePassport2 passport = allFields && all validateField passport
  where
    fields :: [String]
    fields = map fieldName passport
    requiredFields :: [String]
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    allFields :: Bool
    allFields = foldr f True requiredFields
      where 
        f requiredField bool = requiredField `elem` fields && bool

howManyValid :: [Passport] -> Int 
howManyValid = length . filter (== True) . map validatePassport2

solve1 :: String -> Int
solve1 = howManyValid . readInput


main :: IO ()
main = readFile "input/actual.txt" >>= print . solve1