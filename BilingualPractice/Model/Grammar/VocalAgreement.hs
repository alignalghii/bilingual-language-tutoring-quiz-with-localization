module BilingualPractice.Model.Grammar.VocalAgreement where

import Data.List (isInfixOf)
import Data.Bool (bool)


vocalAgreement2 :: String -> (String, String) -> String
vocalAgreement2 stem = (stem <>) . bool fst snd (lowHigh stem)

lowHigh :: String -> Bool
lowHigh txt = any (flip isInfixOf txt) ["e", "é", "i", "í", "ö", "ő", "ü", "ű", "E", "É", "I", "Í", "Ö", "Ő", "Ü", "Ű"]
