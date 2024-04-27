module ParserClasses where
 
import UU.Parsing
import Scanner
import AbstractGrammar

pSlides = Slides <$> pList pSlide
 
pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"
 
pTitleSlide = TitleSlide <$ pKeyword "! " <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"
 
pMarckdownBlock = MdParagraph <$> pList1 pParagraph
               <|> MdH1 <$ pKeyword "#" <*> pStrings
               <|> MdH2 <$ pKeyword "##" <*> pStrings
               <|> MdH3 <$ pKeyword "###" <*> pStrings
               <|> MdH4 <$ pKeyword "####" <*> pStrings
               <|> MdH5 <$ pKeyword "#####" <*> pStrings
               <|> MdList <$ pKeyword "######" <*> pStrings
               <|> MdLink <$ pKeyword "%" <*> pStrings
               <|> MdCode <$ pOpenCodeBlock "<&" <*> pStrings <* pEndCodeBlock ">&"
               <|> MdNumberedList <$ pOpenNumListBlock "*$" <*> pStrings <* pEndNumListBlock "$*"
               <|> MdMatrix <$> pList1 pRow
               <|> MdSplit <$> pKeyword "||"

pRow = MdRow <$> pList1 pCell
       <|> MdEndRow <$> pKeyword "|\n" 

pCell = MdCell <$ pKeyword "|" <*> pStrings

pParagraph = MdText <$> pStrings
            <|> MdBold <$> pBold
            <|> MdItalic <$> pItalic
            <|> MdBreak <$> pKeyword "\n\n"
            <|> MdHighlight <$> pHighlight

 
instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword

pBold :: Parser Token String
pBold = getBold <$> pSym (Token Bold "  " 0 0)

pItalic :: Parser Token String
pItalic = getItalic <$> pSym (Token Italic " " 0 0)

pHighlight :: Parser Token String
pHighlight = getHighlight <$> pSym (Token Highlight "" 0 0)

getItalic :: Token -> String
getItalic (Token Italic v _ _) = v

getBold :: Token -> String
getBold (Token Bold v _ _) = v

getHighlight :: Token -> String
getHighlight (Token Highlight v _ _) = v

pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
pOpenCodeBlock :: String -> Parser Token String
pOpenCodeBlock = tSym OpenCodeBlock

pEndCodeBlock :: String -> Parser Token String
pEndCodeBlock = tSym EndCodeBlock

pOpenNumListBlock :: String -> Parser Token String
pOpenNumListBlock = tSym OpenNumberedList

pEndNumListBlock :: String -> Parser Token String
pEndNumListBlock = tSym EndNumberedList

pStrings :: Parser Token String
pStrings = tStr