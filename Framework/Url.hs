{-# LANGUAGE OverloadedStrings #-}

module Framework.Url where

import Network.Wai (Request, pathInfo, queryString)
import Network.HTTP.Types.URI (queryToQueryText)
import Data.ListX (updateFirstOrLeave, preIntercalate)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Bool (bool)
import Data.Text (Text, unpack)
import Data.String (IsString, fromString)


data Url = U {path :: String, queryItems :: [QueryAssignment]}

instance Show Url where
    show (U path queryItems) = if null queryItems
                              then path
                              else path ++ "?" ++ intercalate "&" (show <$> queryItems)

instance IsString Url where
    fromString url = case splitOn "?" url of
        []               -> error "Impossible situation"
        [path]           -> U path []
        [path, queryStr] -> U path $ map fromString $ splitOn "&" queryStr
        _                -> error "Ambivalent querystring"


data QueryAssignment = QA {name, value :: String}

instance Show QueryAssignment where
    show (QA name value) = name ++ "=" ++ value

instance IsString QueryAssignment where
    fromString str = case splitOn "=" str of
        []            -> error "impossible situation"
        [name, value] -> QA name value
        _             -> "Invalid assignment"

unQA :: QueryAssignment -> (String, String)
unQA (QA name value) = (name, value)


addParam :: Url -> QueryAssignment -> Url
addParam (U path queryItems) item = U path (addItem queryItems item)

addItem :: [QueryAssignment] -> QueryAssignment -> [QueryAssignment]
addItem queryItems newItem = uncurry QA <$> updateFirstOrLeave (unQA newItem) (unQA <$> queryItems)

reqToUrl :: Request -> Url
reqToUrl req = U (reqToPath req) (reqToQueryItems req)

reqToPath :: Request -> String
reqToPath = preIntercalate "/" . map unpack . pathInfo

reqToQueryItems :: Request -> [QueryAssignment]
reqToQueryItems = map (uncurry maybeAssign) . queryToQueryText . queryString

maybeAssign :: Text -> Maybe Text -> QueryAssignment
maybeAssign key (Just value) = QA (unpack key) (unpack value)
maybeAssign _   _            = error "Invalid querystring"
