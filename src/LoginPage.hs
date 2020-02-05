{-# LANGUAGE OverloadedStrings #-}

module LoginPage where

import Prelude hiding (head)
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
import Page

loginPage :: Html
loginPage = createPage "login" $ H.form ! action "/login" ! method "post" $ do
   H.input ! type_ "text" ! name "username" 
   H.br
   H.input ! type_ "text" ! name "password"
   H.br
   H.input ! type_ "submit" ! value "Submit"
