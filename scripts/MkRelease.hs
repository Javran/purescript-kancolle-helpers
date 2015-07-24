{-# LANGUAGE OverloadedStrings #-}
module MkRelease where

import Prelude hiding (FilePath)
import Control.Arrow
import Turtle
import Filesystem.Path.CurrentOS
import qualified Control.Foldl as FL
import Control.Applicative
import Control.Monad
import Text.Printf
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either

-- | guess project home directory
--   make sure to run this somewhere under the project home
guessProjectHome :: FilePath -> IO FilePath
guessProjectHome fPath = do
    guard $ fPath /= root fPath
    b <- testfile (fPath </> "bower.json")
    if b
      then return fPath
      else guessProjectHome (parent fPath)

touchTempBuildDir :: IO FilePath
touchTempBuildDir = do
    tmpDir <- decodeString <$> getTemporaryDirectory
    let dst = tmpDir </> "psbuild/kancolle-helpers"
    mktree dst
    return dst

toText' :: FilePath -> T.Text
toText' = either id id . toText

main :: IO ()
main = do
    cwd <- pwd
    prjHome <- guessProjectHome cwd
    putStrLn $ "Project home in: " <> encodeString prjHome
    (ExitSuccess, npmBin) <- second T.strip <$> shellStrict "npm bin" ""
    let uglifyJsBin = fromText npmBin </> "uglifyjs"
    True <- testfile uglifyJsBin
    putStrLn $ "Found uglifyjs in: " <> encodeString uglifyJsBin
    buildDir <- touchTempBuildDir
    -- sync files
    let excludeArgs = map ("--exclude=" <>)
                          [".git/", ".cabal-sandbox/", "output/"]
    (ExitSuccess, _) <-
         procStrict "rsync" ("-r" : excludeArgs <> [toText' prjHome, toText' buildDir]) ""
    cd buildDir
    -- build and optimize
    (ExitSuccess, _) <- shellStrict "pulp build" ""
    (ExitSuccess, jsContent) <- shellStrict "psc-bundle 'output/*/*.js' \
                                            \-m KanColle.Expedition \
                                            \-m KanColle.DamageAnalysis \
                                            \ " ""
    (ExitSuccess, jsOptimized) <- procStrict (toText' uglifyJsBin) ["-c", "-m"] (return jsContent)
    cd cwd
    let targetFile = "KanColleHelpers.js"
    -- write to file
    T.writeFile targetFile jsOptimized
    T.putStrLn $ "Saved to: " <> toText' (cwd </> decodeString targetFile)
    return ()
