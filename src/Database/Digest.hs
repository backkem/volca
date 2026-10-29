{-# LANGUAGE OverloadedStrings #-}

{- | The SHA-256 digests a package names a database's sources by, computed so
that a reader can compute them again with standard tools.
-}
module Database.Digest (
    sourcesDigest,
    fileDigest,
    textDigest,
) where

import Control.Exception (evaluate)
import Crypto.Hash (Digest, SHA256, hash, hashlazy)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesDirectoryExist)
import System.FilePath (makeRelative, (</>))

import Database.Upload (listDirectoryRecursive)

{- | A file: the digest of its bytes. A directory: the digest of the listing
@sha256sum@ prints for its files, their paths relative to it and sorted byte
by byte, the same as

> find . -type f -printf '%P\n' | LC_ALL=C sort | xargs -d '\n' sha256sum | sha256sum

shortcut: a path holding a backslash or a newline is printed escaped by
@sha256sum@ and not here; escape it if a database ever ships one.
shortcut: the files are read at every package; keep the digest beside the
upload when packaging a database of several gigabytes shows in its time.
-}
sourcesDigest :: FilePath -> IO Text
sourcesDigest path = do
    isDirectory <- doesDirectoryExist path
    if not isDirectory
        then fileDigest path
        else do
            files <- sortOn (T.encodeUtf8 . T.pack) . map (makeRelative path) <$> listDirectoryRecursive path
            listing <- traverse (\file -> (\digest -> digest <> "  " <> T.pack file <> "\n") <$> fileDigest (path </> file)) files
            pure (textDigest (T.concat listing))

-- | The digest of a file's bytes, read in chunks.
fileDigest :: FilePath -> IO Text
fileDigest path = BL.readFile path >>= evaluate . T.pack . show . (hashlazy :: BL.ByteString -> Digest SHA256)

-- | The digest of a text's UTF-8 bytes.
textDigest :: Text -> Text
textDigest text = T.pack (show (hash (T.encodeUtf8 text) :: Digest SHA256))
