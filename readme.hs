#!/usr/bin/runghc

{--
 - Generate README.md with all of the pictures for ahaym/dynamics.
 - Run me in the project root with all the images!
--}

import System.Directory

mkLink file = "![](" ++ file ++ ")"

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

main = do
   template <- lines <$> readFile "template.md"
   contents <- filter (\s -> lastN 3 s == "png") <$> listDirectory "."
   writeFile "README.md" . unlines $ template ++ (mkLink <$> contents)
