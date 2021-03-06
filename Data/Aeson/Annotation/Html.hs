{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Data.Aeson.Annotation.Html where
import Text.Blaze
import Text.Blaze.Html5 (Html, (!), toHtml, toValue) 
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Aeson
import qualified Data.Vector as V
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import qualified Data.HashMap.Strict as H (toList)
import Data.Scientific
import Network.URI

instance ToMarkup Value where
  toMarkup (Object v) = fromCompound ("{", "}") fromPair (H.toList v)
  toMarkup (Array v) = fromCompound ("[", "]") toMarkup (V.toList v)
  toMarkup (String v) = handleString v
  toMarkup (Number v) = wrapspan "number" 
        $ either show show
            (floatingOrInteger v)
  toMarkup (Bool True) = wrapspan "bool" ("true" :: Text)
  toMarkup (Bool False) = wrapspan "bool" ("false" :: Text)
  toMarkup Null = wrapspan "null" ("null" :: Text)

wrapspan :: ToMarkup a => Text -> a -> Html
wrapspan jsonType x = H.span 
    ! A.class_ (toValue $ "json-val json-val-" <> jsonType)
    $ toMarkup x

handleString :: Text -> Html
handleString v | isURI (T.unpack v) = linkHref v
    
handleString v = wrapspan "string" (show v)

indented = A.style "margin-left: 1.0em"

fromCompound :: (Text, Text)
             -> (a -> Html)
             -> [a]
             -> Html
fromCompound (delimL,delimR) fromItem items = do
    mconcat
      [ (toMarkup delimL)
      , if null items then mempty
          else do 
            H.div ! indented $ 
              items' <> H.br <> "   "  -- indent
      , (toMarkup delimR)
      ]
  where
      items' = mconcat . intersperse ("," <> H.br) $
                  map (\item -> fromItem item) items

fromPair :: (Text, Value) -> Html
fromPair (k@"href", String v) = (toMarkup k) <> ": " <> (linkHref v)
fromPair (k,v) = (toMarkup k) <> ": " <> (toMarkup v)

linkHref :: Text -> Html
linkHref v = 
   let a = H.a ! A.class_ "json-val json-val-string" 
               ! A.href (toValue v) $ toHtml v
   in "\"" <> a <> "\""




