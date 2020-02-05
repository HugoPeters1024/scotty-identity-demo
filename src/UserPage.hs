{-# LANGUAGE OverloadedStrings #-}

module UserPage where

import Prelude hiding (head)
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
import Page
import User
import Post

userPage :: User ->  [Post] -> Html
userPage user posts = do
    let username = u_userName user
    createPage username $ do
      h2 (toHtml username)
      h1 "Posts:" 
      table $ forM_ posts renderPost

renderPost :: Post -> Html
renderPost p = (tr . td . toHtml . p_post) p ! class_ "post"
