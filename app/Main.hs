{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Console.CmdArgs

data InstagramDownload = InstagramDownload {url :: String}
  deriving (Show, Data, Typeable)

instagramDownload :: InstagramDownload
instagramDownload = InstagramDownload
  {url = def &= argPos 0 &= typ "URL"}
  &= program "instagram-dl"
  &= help "Download image from instagram"

run :: InstagramDownload -> IO ()
run (InstagramDownload imageURL) = saveImageFromPage imageURL

main :: IO ()
main = run =<< cmdArgs instagramDownload
