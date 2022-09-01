{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.CommonSnippets where

import BilingualPractice.Language (Language (..))
import Data.String (IsString)


appTitleSnippet :: IsString string => Language -> string
appTitleSnippet En = "Hungarian-English word and sentence practice quiz-sets"
appTitleSnippet Hu = "Magyar-angol szó- és mondatgyakorló"

backHomeLinkTextSnippet :: IsString string => Language -> string
backHomeLinkTextSnippet En = "Back to the main page"
backHomeLinkTextSnippet Hu = "Vissza a főoldalra"

portfolioLinkTextSnippet :: IsString string => Language -> string
portfolioLinkTextSnippet En = "https://alignalghii.github.io"
portfolioLinkTextSnippet Hu = "https://alignalghii.github.io/index.hu.html"

askLinguisticalUnitSnippet, askDifficultyLevelSnippet :: IsString string => Language -> string
askLinguisticalUnitSnippet En = "Word or sentence?"
askLinguisticalUnitSnippet Hu = "Szó vagy mondat?"
askDifficultyLevelSnippet En = "Difficulty level"
askDifficultyLevelSnippet Hu = "Nehézségi szint"
