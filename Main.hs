{-# LANGUAGE OverloadedStrings, RecordWildCards
    , ScopedTypeVariables #-} 
module Main where
import Data.Aeson
import Data.Aeson.Annotation.Html 
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5

main = do
    x :: Maybe Value <- fmap decode BL.getContents 
    maybe (error "No parse") 
      (putStrLn . renderHtml . toHtml) x
