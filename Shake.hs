#!/usr/local/bin/stack runhaskell

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    action $ do 
        assignments <- filter ((/= "template") . takeDirectory) <$> getDirectoryFiles "" ["**/assignment.tex"]

        need $ map (-<.> "pdf") assignments

    "**/*.pdf" %> \out ->
        unit $ cmd "latexmk" "-pdf" (Cwd $ takeDirectory out)
