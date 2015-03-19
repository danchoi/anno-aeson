{-# LANGUAGE OverloadedStrings, RecordWildCards
    , ScopedTypeVariables #-} 
module Main where
import Data.Aeson
import Data.Aeson.Annotation.Html 
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5
import qualified Data.Text.Lazy.IO as TL

main = do
    x :: Maybe Value <- fmap decode BL.getContents 
    maybe (error "No parse") 
      (TL.putStrLn . renderHtml . toHtml) x
