{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the digests a package names a database's sources by ('Database.Digest').
module DigestSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Hspec

import Database.Digest (sourcesDigest)

spec :: Spec
spec = describe "sourcesDigest" $ do
    it "digests a directory as the command its package describes does" $
        withSources $ \dir -> do
            command <- readProcess "sh" ["-c", "cd '" <> dir <> "' && find . -type f -printf '%P\\n' | LC_ALL=C sort | xargs -d '\\n' sha256sum | sha256sum"] ""
            sourcesDigest dir `shouldReturn` T.pack (takeWhile (/= ' ') command)

    it "changes when one byte of one file changes" $
        withSources $ \dir -> do
            before <- sourcesDigest dir
            BS.writeFile (dir </> "Process" </> "b.xml") "<b/>!"
            sourcesDigest dir `shouldNotReturn` before

    it "digests a file as its bytes" $
        withSources $ \dir -> do
            command <- readProcess "sha256sum" [dir </> "a.csv"] ""
            sourcesDigest (dir </> "a.csv") `shouldReturn` T.pack (takeWhile (/= ' ') command)
  where
    withSources :: (FilePath -> IO ()) -> IO ()
    withSources k = withSystemTempDirectory "digest" $ \dir -> do
        BS.writeFile (dir </> "a.csv") "a;b\n"
        BS.writeFile (dir </> "B.csv") "upper first, byte by byte\n"
        createDirectory (dir </> "Process")
        BS.writeFile (dir </> "Process" </> "b.xml") "<b/>"
        k dir
