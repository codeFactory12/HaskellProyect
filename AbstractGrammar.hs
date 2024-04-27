{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module AbstractGrammar where

type Strings = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String
type Link = String
type List = String
type Code = String
type Bold = String
type Text = String
type Italic = String
type NumList = String
type Break = String
type Highlight = String
type Split = String

data Slides = Slides [Slide]
    deriving Show

data Slide = Slide TitleSlide BodySlide
    deriving Show

data Cad = Cad Strings
    deriving Show

data TitleSlide = TitleSlide Strings
    deriving Show

data BodySlide = BodySlide [MarckdownBlock]
    deriving Show

data MarckdownBlock = MdParagraph [Paragraph]
                    | MdH1 H1
                    | MdH2 H2
                    | MdH3 H3
                    | MdH4 H4
                    | MdH5 H5
                    | MdList List
                    | MdLink Link
                    | MdCode Code
                    | MdNumberedList NumList
                    | MdMatrix [Row]
                    | MdSplit Split
                    deriving Show

data Row = MdRow [Cell]
           |MdEndRow String  
        deriving Show

data Cell = MdCell String
        deriving Show 

data Paragraph = MdText Text 
                | MdBold Bold
                | MdItalic Italic
                | MdBreak Break
                | MdHighlight Highlight
        deriving Show