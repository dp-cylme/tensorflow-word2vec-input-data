{-# LANGUAGE LambdaCase #-}

-- | Downloads the data set from Matt Mahoney's web site and packages them as data files.
module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Distribution.Simple
       (UserHooks(..), defaultMainWithHooks, simpleUserHooks)
import Distribution.PackageDescription
       (GenericPackageDescription(packageDescription), dataDir)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP as HTTP
import qualified Network.URI as URI


main :: IO ()
main = defaultMainWithHooks downloadingDataFiles


downloadingDataFiles :: UserHooks
downloadingDataFiles =
    hooks
    { confHook = \gh@(g,_) c ->
                      downloadFiles g >> confHook hooks gh c
    }
  where
    hooks = simpleUserHooks
    downloadFiles :: GenericPackageDescription -> IO ()
    downloadFiles g = do
      let dir = dataDir (packageDescription g)
      mapM_ (maybeDownload dir) fileInfos

maybeDownload :: FilePath -> (String, String) -> IO ()
maybeDownload dataDir (basename,sha256) = do
    let filePath = dataDir </> basename
    exists <- doesFileExist filePath
    when (not exists) $
        do let url = urlPrefix ++ basename
           hPutStrLn stderr ("Downloading " ++ url)
           httpDownload url filePath
    verify filePath sha256


httpDownload :: String -> FilePath -> IO ()
httpDownload url outFile = do
    let uri = fromMaybe
              (error ("Can't be: invalid URI " ++ url))
              (URI.parseURI url)
    result <- HTTP.simpleHTTP (HTTP.defaultGETRequest_ uri)
    HTTP.getResponseCode result >>= \case
        (2, 0, 0) -> HTTP.getResponseBody result >>= B.writeFile outFile
        s -> error ( "Failed to download " ++ url ++ " error code " ++ show s
                     ++ helpfulMessage
                    )

verify :: FilePath -> String -> IO ()
verify filePath hash = do
    let sha256 = Hash.hashlazy :: B.ByteString -> Hash.Digest Hash.SHA256
    computed <- show . sha256 <$> B.readFile filePath
    when (hash /= computed) $
        error ( "Incorrect checksum for " ++ filePath
                 ++ "\nexpected " ++ hash
                 ++ "\ncomputed " ++ computed
                 ++ helpfulMessage
              )


urlPrefix = "http://mattmahoney.net/dc/"

fileInfos =
    [ ( "text8.zip"
      , "a6640522afe85d1963ad56c05b0ede0a0c000dddc9671758a6cc09b7a38e5232")]

helpfulMessage =
    unlines
        ("" :
         "" :
         "Please download the following URLs manually and put them in data/" :
         [ urlPrefix ++ h
         | (h,_) <- fileInfos ])
