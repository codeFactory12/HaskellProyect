{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module GenerateHtml where
import Scanner
import AbstractGrammar
import UU.Parsing
import ParserClasses
import Data.List (isSuffixOf)

evalSlides :: Slides -> String
evalSlides (Slides slides) =  genreStyle ++ "<body>\n<div class = \"reveal\">\n <div class = \"slides\">\n" ++ concatMap evalSlide slides ++ "</div>\n</div>\n"++revealInitialize++"</body>\n</hmtl>\n"

evalSlide :: Slide -> String
evalSlide (Slide title body) = "<section>\n" ++ splitInit  ++ evalTitle title ++ evalBody body ++ splitEnd ++ "</section>\n"

evalBody :: BodySlide -> String
evalBody (BodySlide xs) = concatMap evalText xs

evalTitle :: TitleSlide -> String
evalTitle (TitleSlide str) = identText ++ "<h1>" ++ str ++ "</h1>\n"

evalText :: MarckdownBlock -> String
evalText (MdParagraph str) = identText ++ "<p>\n" ++ identText ++concatMap evalP str ++ identText ++ "</p>\n"
evalText (MdH1 str) = identText ++ "<h2>" ++ str ++ "</h2>\n"
evalText (MdH2 str) = identText ++ "<h3>" ++ str ++ "</h3>\n"
evalText (MdH3 str) = identText ++ "<h4>" ++ str ++ "</h4>\n"
evalText (MdH4 str) = identText ++ "<h5>" ++ str ++ "</h5>\n"
evalText (MdH5 str) = identText ++ "<h6>" ++ str ++ "</h6>\n"
evalText (MdList str) = identText ++ "<li>" ++ str ++ "</li>\n"
evalText (MdLink str) = if isImg str then identText ++ "<div class = \"image\">\n<img src=" ++ str ++ ">\n</div>\n" else identText ++ "<a href=" ++ str ++">"++ str  ++ "</a>\n"
evalText (MdCode str) = let (language, code) = languageCode str in identText ++ "<pre><code class=\""++ language ++"\">" ++ identText ++ code ++ identText ++  "</code></pre>\n"
evalText (MdNumberedList str) = numberedList str
evalText (MdMatrix str) = "<table>\n" ++ concatMap evalRow str ++ "</table>\n"
evalText (MdSplit str) = "</div>\n<div class=\"column\">\n"

evalRow :: Row -> String
evalRow (MdRow str) = "<tr>\n" ++ concatMap evalCell str ++ "</tr>\n"
evalRow (MdEndRow str) = ""

evalCell :: Cell -> String
evalCell (MdCell str) = "<th>" ++ str ++ "</th>\n"

evalP :: Paragraph -> String
evalP (MdText str) = str
evalP (MdBold str) = identText ++ "\n<b>" ++ str ++ "</b>\n"
evalP (MdItalic str) = identText ++ "\n<i>" ++ str ++ "</i>\n"
evalP (MdBreak str) = "</p>\n<p>"
evalP (MdHighlight str) = "\n<mark>" ++ str ++ "</mark>\n"

parserConStr :: Slides -> String
parserConStr (Slides slides) = evalSlides (Slides slides)

idenHtml :: Int -> String
idenHtml 0 = ""
idenHtml n = " " ++ idenHtml (n - 1)

isImg :: String -> Bool
isImg str = ".png" `isSuffixOf` str || ".jpg" `isSuffixOf` str || ".gif" `isSuffixOf` str

languageCode :: String -> (String,String)
languageCode [] = ("","")
languageCode xs = span (/= '\n') xs

numberedList :: String -> String
numberedList str =
  identText ++ "<ol>\n" ++ concatMap generateLi (lines str) ++ identText ++"</ol>\n"

generateLi :: String -> String
generateLi line = identText ++"<li>" ++ firstWord line ++ "</li>\n"

firstWord :: String -> String
firstWord = takeWhile (/= '\n')

splitInit = "<div class = \"split\">\n" ++
        "<div class=\"column\">\n"

splitEnd = "</div>\n" ++ "</div>\n"

identText = idenHtml 4
genreStyle = "<!doctype html>\n<html lang=\"en\">\n<head>\n" ++
    applyIdent highlights ++
    applyIdent reveal ++
    applyIdent slideStyle ++
    "</head>\n"

highlights =   identText ++ "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/panda-syntax-dark.min.css\">\n" ++
    identText ++ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js\"></script>\n" ++
    identText ++ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/go.min.js\"></script>\n" ++
    identText ++"<script>hljs.highlightAll();</script>\n"

slideStyle = "<style>\n" ++
        "section{\n" ++
        "    text-align: left;\n" ++
        "}\n" ++                      
        "h1{\n" ++
        "    text-align: center;\n" ++
        "}\n" ++
        ".image{\n" ++
        "    text-align: center;\n" ++
        "}\n" ++
        "a{\n" ++
        "    font-size: 30px;\n" ++
        "}\n" ++
        "p{\n" ++
        "    text-align: justify;\n" ++
        "}\n" ++
        "mark{\n" ++
        "    background-color: aqua;\n" ++
        "}\n" ++
        "th{\n" ++
        "    font-size: 30px;\n" ++
        "}\n" ++
        ".column{\n" ++
        "    flex: 1;\n" ++
        "    padding: 20px;\n" ++
        "}\n" ++
        ".split {\n" ++
        "display: flex;\n" ++ 
        "}\n" ++
    "</style>\n"

revealInitialize = "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/js/reveal.min.js\"></script>\n" ++
    "<script> Reveal.initialize( {  width: 960,\n" ++
    "    height: 1200, slideNumber: 'c/t', transition: 'convex'});\n" ++ 
    "</script>\n"

reveal =  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/css/reveal.min.css\">\n" ++
    "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/css/theme/solarized.css\" id=\"theme\">\n"

applyIdent :: String -> String
applyIdent [] = []
applyIdent (x:xs) | x == '\n' = '\n' : identText ++ applyIdent xs
                  |otherwise = x:applyIdent xs
