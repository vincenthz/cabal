-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Unpack
-- Copyright   :  (c) Andrea Vezzosi 2008
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Unpack (

    -- * Commands
    unpack,

  ) where

import Distribution.Package
         ( PackageId, packageId, packageName )
import Distribution.Simple.Setup
         ( fromFlag, fromFlagOrDefault )
import Distribution.Simple.Utils
         ( notice, die, info, writeFileAtomic )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)

import Distribution.Client.Setup
         ( GlobalFlags(..), UnpackFlags(..) )
import Distribution.Client.Types
import Distribution.Client.Targets
import Distribution.Client.Dependency
import Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
        ( getSourcePackages )
import Distribution.Client.Sumfile (sumfileParse, sumfileVerify)

import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import Control.Monad
         ( unless, when, forM_ )
import Data.Monoid
         ( mempty )
import System.FilePath
         ( (</>), (<.>), addTrailingPathSeparator )


unpack :: Verbosity
       -> [Repo]
       -> GlobalFlags
       -> UnpackFlags
       -> [UserTarget] 
       -> IO ()
unpack verbosity _ _ _ [] =
    notice verbosity "No packages requested. Nothing to do."

unpack verbosity repos globalFlags unpackFlags userTargets = do
  mapM_ checkTarget userTargets

  sourcePkgDb   <- getSourcePackages verbosity repos

  pkgSpecifiers <- resolveUserTargets verbosity
                     (fromFlag $ globalWorldFile globalFlags)
                     (packageIndex sourcePkgDb)
                     userTargets

  pkgs <- either (die . unlines . map show) return $
            resolveWithoutDependencies
              (resolverParams sourcePkgDb pkgSpecifiers)

  unless (null prefix) $
         createDirectoryIfMissing True prefix

  forM_ pkgs $ \pkg -> do
    location <- fetchPackage verbosity (packageSource pkg)
    let pkgid = packageId pkg
        descOverride | usePristine = Nothing
                     | otherwise   = packageDescrOverride pkg
    case location of
      LocalTarballPackage tarballPath ->
        unpackPackage verbosity prefix pkgid descOverride tarballPath

      RemoteTarballPackage _tarballURL tarballPath ->
        unpackPackage verbosity prefix pkgid descOverride tarballPath

      RepoTarballPackage _repo _pkgid tarballPath ->
        unpackPackage verbosity prefix pkgid descOverride tarballPath

      LocalUnpackedPackage _ ->
        error "Distribution.Client.Unpack.unpack: the impossible happened."

  where
    resolverParams sourcePkgDb pkgSpecifiers =
        --TODO: add commandline constraint and preference args for unpack

        standardInstallPolicy mempty sourcePkgDb pkgSpecifiers

    prefix = fromFlagOrDefault "" (unpackDestDir unpackFlags)
    usePristine = fromFlagOrDefault False (unpackPristine unpackFlags)

checkTarget :: UserTarget -> IO ()
checkTarget target = case target of
    UserTargetLocalDir       dir  -> die (notTarball dir)
    UserTargetLocalCabalFile file -> die (notTarball file)
    _                             -> return ()
  where
    notTarball t =
        "The 'unpack' command is for tarball packages. "
     ++ "The target '" ++ t ++ "' is not a tarball."

unpackPackage :: Verbosity -> FilePath -> PackageId
              -> PackageDescriptionOverride
              -> FilePath  -> IO ()
unpackPackage verbosity prefix pkgid descOverride pkgPath = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    Tar.extractTarGzFile prefix pkgdirname pkgPath
    existsSum <- doesFileExist (pkgdir </> "SUMS")
    if existsSum
      then do sumfile <- sumfileParse (pkgdir </> "SUMS")
              ret     <- sumfileVerify pkgdir sumfile
              case ret of
                Nothing  -> return ()
                Just err -> die err
      else do -- FIXME: when the sum file is missing,
              -- there should be a flag to either ignore or not the problem,
              -- or the user should be ask if this is OK.
              return ()

    case descOverride of
      Nothing     -> return ()
      Just pkgtxt -> do
        let descFilePath = pkgdir </> display (packageName pkgid) <.> "cabal"
        info verbosity $
          "Updating " ++ descFilePath
                      ++ " with the latest revision from the index."
        writeFileAtomic descFilePath pkgtxt
