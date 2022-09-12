module BilingualPractice.View.StyleHelper where

import Text.Blaze.Html5 (Html, q, em)


qEm :: Html -> Html
qEm = q . em
