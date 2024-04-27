module Main where

import UU.Parsing
import Scanner(scanner)
import ParserClasses
import GenerateHtml

main :: IO ()
main = do input <- readFile "slide.p5"
          print input
          let token = scanner input
          putStrLn(show token)
          tree <- parseIO pSlides token
          putStrLn (show tree)
          writeFile "output.html" (parserConStr tree)
          -- putStrLn (show (parserConStr tree))
