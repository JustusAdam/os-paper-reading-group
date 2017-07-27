#!/usr/local/bin/stack runhaskell

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


buildPresentation = do
    need ["presentation/presentation.md", "presentation/template.tex"]
    unit $ cmd "pandoc" "presentation.md" "-s" "--template" "template.tex" "-t" "beamer" "-o" "presentation.pdf" (Cwd "presentation")


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    action $ do 
        assignments <- filter ((/= "template") . takeDirectory) <$> getDirectoryFiles "" ["**/assignment.tex"]

        need $ "presentation/presentation.pdf" : map (-<.> "pdf") assignments

    "*/*.pdf" %> \out ->
        if out == "presentation/presentation.pdf"
            then buildPresentation
            else do
                need ["main.bib", out -<.> "tex"]
                unit $ cmd "latexmk" "-pdf" (Cwd $ takeDirectory out)
