{-# LANGUAGE OverloadedStrings #-}

module UserPage where

import Prelude hiding (head)
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text as R
import User
import Post

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
    H.div ! class_ "content" $ do
      content

bootstrap :: Html
bootstrap = H.link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"

userPage :: User ->  [Post] -> Html
userPage user posts = do
    let username = u_userName user
    createPage username $ do
      h2 (toHtml username)
      h1 "Posts:" 
      table $ forM_ posts renderPost

renderPost :: Post -> Html
renderPost p = (tr . td . toHtml . p_post) p ! class_ "post"
