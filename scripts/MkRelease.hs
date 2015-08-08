{-# LANGUAGE OverloadedStrings #-}
module MkRelease where

import Prelude hiding (FilePath)
import Control.Arrow
import Turtle
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | guess project home directory
--   make sure to run this somewhere under the project home
guessProjectHome :: FilePath -> IO FilePath
guessProjectHome fPath = do
    guard $ fPath /= root fPath
    b <- testfile (fPath </> "bower.json")
    if b
      then return fPath
      else guessProjectHome (parent fPath)

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
    let buildDir = prjHome
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
                                            \-m KanColle.Expedition.Requirement \
                                            \-m KanColle.DamageAnalysis \
                                            \-m KanColle.Expedition.Evaluate \
                                            \ " ""
    (ExitSuccess, jsOptimized) <- procStrict (toText' uglifyJsBin) ["-c", "-m"] (return jsContent)
    cd cwd
    let targetFile = "KanColleHelpers.js"
        targetFileNode = "KanColleHelpersN.js"
    -- write to file
    T.writeFile targetFile jsOptimized
    T.putStrLn $ "Saved to: " <> toText' (cwd </> decodeString targetFile)
    T.writeFile targetFileNode (jsOptimized <> "\nmodule.exports = PS;\n")
    T.putStrLn $ "Saved to: " <> toText' (cwd </> decodeString targetFileNode)
    return ()
