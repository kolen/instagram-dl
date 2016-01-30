{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    (
      saveImageFromPage
    ) where

import Network.HTTP.Conduit (simpleHttp, parseUrl, http, responseBody,
                             newManager, tlsManagerSettings)
import Network.HTTP.Types.URI (decodePathSegments, extractPath)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, element, fromDocument, descendant,
                        attribute, (>=>))
import Safe (headMay, lastMay)
import Control.Exception
import Data.Typeable

ogValues :: String -> Cursor -> [T.Text]
ogValues prop = descendant
  >=> element "meta"
  >=> attributeIs "property" (T.pack $ "og:" ++ prop)
  >=> attribute "content"

ogValue :: String -> Cursor -> Maybe T.Text
ogValue prop = headMay . ogValues prop

cursorForUrl :: String -> IO Cursor
cursorForUrl u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

downloadImage :: String -> String -> IO ()
downloadImage url file = do
  request <- parseUrl url
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    responseBody response C.$$+- sinkFile file

-- |Determines filename for image from page content
filenameFromURL :: T.Text -> Maybe T.Text
filenameFromURL = lastMay . decodePathSegments . extractPath . encodeUtf8

data SaveImageException = CantFindImageURLException
  deriving (Show, Typeable)

instance Exception SaveImageException

saveImageFromPage :: String -> IO ()
saveImageFromPage pageUrl = do
  cursor <- cursorForUrl pageUrl
  let imageURL = ogValue "image" cursor
  let filename = imageURL >>= filenameFromURL
  case (imageURL, filename) of
    (Just imageURL', Just filename') ->
      downloadImage (T.unpack imageURL') (T.unpack filename')
    (_, _) ->
      throwIO CantFindImageURLException
