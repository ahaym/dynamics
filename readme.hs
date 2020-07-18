#!/usr/bin/runghc

{--
 - Generate README.md with all of the pictures for ahaym/dynamics.
 - Also moves all image files to the correct folder.
 - Run me in the project root with all the images!
--}

import System.Directory
import System.Process

mkLink file = "![](img/" ++ file ++ ")"

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

main = do
   template <- lines <$> readFile "template.md"
   callCommand "mv *.svg img/"
   contents <- listDirectory "./img"
   writeFile "README.md" . unlines $ template ++ (mkLink <$> contents)
