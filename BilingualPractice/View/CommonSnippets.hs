{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.CommonSnippets where

import BilingualPractice.Language (Language (..))
import Data.String (IsString)


portfolioLinkTextSnippet :: IsString string => Language -> string
portfolioLinkTextSnippet En = "https://alignalghii.github.io"
portfolioLinkTextSnippet Hu = "https://alignalghii.github.io/index.hu.html"
