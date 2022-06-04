module Data.Property where

import Data.List
import Data.Maybe


type PropertyPredicate a = a -> Bool

type PropertyAssignment a b = (a, b)
type PropertyMapping a b = [PropertyAssignment a b]

(*&&*), (*||*) :: PropertyPredicate a -> PropertyPredicate a -> PropertyPredicate a
p *&&* q = \a -> p a && q a
p *||* q = \a -> p a || q a

propNot :: PropertyPredicate a -> PropertyPredicate a
propNot prop = not . prop

propertyConjunction, propertyDisjunction :: [PropertyPredicate a] -> PropertyPredicate a
propertyConjunction = foldr (*&&*) $ const True
propertyDisjunction = foldr (*||*) $ const False

mapProperties :: Eq key => PropertyMapping key value -> [key]-> [value]
mapProperties = mapMaybe . flip lookup

matchField :: Eq value => (record -> value) -> value -> PropertyPredicate record
matchField selector value = (== value) . selector

--selectorPropertiesDisjunction :: (Eq key, Eq value) => (record -> value) -> PropertyMapping key value -> [key] -> PropertyPredicate record
--selectorPropertiesDisjunction selector mapping keys = propertyDisjunction $ matchField selector <$> mapProperties mapping keys

--selectorsPropertyCNF :: (Eq key, Eq value) => PropertyMapping (record -> value) (PropertyMapping key value) -> [key] -> PropertyPredicate record
--selectorsPropertyCNF selectorMappings keys = selectorPropertiesDisjunction $ map (flip id keys . uncurry selectorPropertiesConjunction) selectorMappings
