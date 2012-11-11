module Distribution.Client.Sumfile (
    sumfileParse,
    sumfileVerify,
    sumfileWrite,
    sumfileCompute,
    SumMap,
    hashFile
    ) where

import Distribution.Client.Sumfile.Digest (hashlazy)

import Control.Applicative ((<$>))
import Control.Monad (foldM, liftM)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Word

import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getDirectoryContents)

import Distribution.Simple.Utils ( writeUTF8File )
import Text.Printf (printf)

type Digest = String
type SumMap = Map.Map FilePath Digest

-- | Parse a SUM file.
sumfileParse :: FilePath -> IO SumMap
sumfileParse filepath = foldl add Map.empty . lines <$> readFile filepath
    where add acc line = case break (== ' ') line of
                              (_,"")               -> acc
                              (digest,(' ':' ':f)) -> Map.insert f digest acc
                              _                    -> acc

-- | verify all the files match the digest in the sum file.
-- on success, nothing is return.
-- on failure, the reason of failure is returned.
sumfileVerify :: FilePath -> SumMap -> IO (Maybe String)
sumfileVerify targetDir summap = listDirWithPrefix "" targetDir >>= verify
    where verify []     = return Nothing
          verify (f:fs)
            | f == "SUMS" = verify fs
            | otherwise   = do h <- hashFile (targetDir </> f)
                               case Map.lookup f summap of
                                    Nothing                         -> return $ Just ("missing file from SUMS: " ++ f)
                                    Just hexpected | h == hexpected -> verify fs
                                                   | otherwise      -> return $ Just ("unmatching digest from file: " ++ f)

-- | Write a SUM file using the map in argument.
sumfileWrite :: FilePath -> SumMap -> IO ()
sumfileWrite sumFilepath summap = writeUTF8File sumFilepath $ unlines $ map marshallLines $ Map.toList summap
    where marshallLines (filepath, digest) = digest ++ "  " ++ filepath

-- | Compute a SUM map of every files in a directory.
sumfileCompute :: FilePath -> IO SumMap
sumfileCompute targetDir = listDirWithPrefix "" targetDir >>= foldM createSumMap Map.empty
    where createSumMap acc f = hashFile f >>= \digest -> return $ Map.insert f digest acc

-- | Create a list of all the files in a directory,
-- including all subdirectories.
listDirWithPrefix :: FilePath -> FilePath -> IO [FilePath]
listDirWithPrefix prefix dir = do
    contents <- getDirectoryContents dir
    concat `fmap` foldM sumUp [] contents
  where sumUp :: [[FilePath]] -> FilePath -> IO [[FilePath]]
        sumUp acc filepath
            | filepath == "."  = return acc
            | filepath == ".." = return acc
            | otherwise        = do isDir <- doesDirectoryExist (dir </> filepath)
                                    if isDir
                                        then liftM (:acc) $ listDirWithPrefix (prefix </> filepath) (dir </> filepath)
                                        else return ([prefix </> filepath]:acc)

-- | Hash a file, returning an hexadecimal digest
hashFile :: FilePath -> IO Digest
hashFile f = (hex . hashlazy 256) `fmap` BS.readFile f
    where hex :: B.ByteString -> String
          hex = concatMap toHex . B.unpack
          toHex :: Word8 -> String
          toHex x = printf "%02x" x
