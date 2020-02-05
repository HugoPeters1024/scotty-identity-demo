
{-# LANGUAGE OverloadedStrings #-}

module Page where

import Prelude hiding (head)
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R

bootstrap :: Html
bootstrap = H.link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"


createPage :: String -> Html -> Html
createPage title content = H.docTypeHtml $ do
  head $ do
    H.title (toHtml title)
    bootstrap
  body $ do
    nav $ do
      ul $ do
        li "Home"
        li "About"
      H.form ! action "/logout" $ do
        H.input ! type_ "submit" ! value "Logout"
    H.div ! class_ "content" $ do
      content
