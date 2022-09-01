module Framework.Form where

import Data.Property (PropertyPredicate, matchField, mapProperties, propertyConjunction, propertyDisjunction)
import Data.ReflectionX (fieldInhabitants)
import Data.String (IsString)
import Control.Arrow ((&&&))


class FormParamable a where
    formParam :: IsString string => a -> string

type CheckList record = [(String, PropertyPredicate record)]

type SetLogFunPredicate a = [PropertyPredicate a] -> PropertyPredicate a

makeCheckList :: (Bounded val, Enum val, Eq val, FormParamable val) => (record -> val) -> CheckList record
makeCheckList selector = (formParam &&& matchField selector) <$> fieldInhabitants selector

useCheckList :: SetLogFunPredicate record -> CheckList record -> [String] -> PropertyPredicate record
useCheckList contract checkList = contract . mapProperties checkList

-- throughCheckList :: (Bounded v, Enum v, Eq v, FormParamable v) => SetLogFunPredicate rec -> (rec -> v) -> [String] -> PropertyPredicate rec
-- throughCheckList contract = useCheckList contract . makeCheckList

useCheckMatrixNormalForm :: SetLogFunPredicate r -> SetLogFunPredicate r -> [CheckList r] -> [String] -> PropertyPredicate r
useCheckMatrixNormalForm externalContract internalContract checkLists names = externalContract $ flip (useCheckList internalContract) names <$> checkLists

useCheckMatrixCNF :: [CheckList r] -> [String] -> PropertyPredicate r
useCheckMatrixCNF = useCheckMatrixNormalForm propertyConjunction propertyDisjunction
