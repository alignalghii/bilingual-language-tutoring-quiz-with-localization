module Data.ReflectionX where

import Data.List (find)


allInhabitants :: (Enum a, Bounded a) => [a]
allInhabitants = enumFrom minBound

fieldInhabitants :: (Bounded a, Enum a) =>  (record -> a) -> [a]
fieldInhabitants = const allInhabitants

invertFunction :: (Enum a, Bounded a, Eq b) => (a -> b) -> b -> Maybe a
invertFunction f b = find ((== b) . f) allInhabitants
