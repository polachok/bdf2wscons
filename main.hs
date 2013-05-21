{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char
import Numeric

toNextLine = skipWhile (/='\n') >> endOfLine

glyph :: Parser (Maybe (BS.ByteString, Int, [Int]))
glyph = do
        string "STARTCHAR" >> skipSpace
        name <- takeWhile1 (/='\n') 
        toNextLine
        code <- (string "ENCODING" >> some space >> decimal)
        toNextLine `manyTill` (try $ string "BITMAP")
        toNextLine
        points <- (some (hexadecimal <* endOfLine))
        string "ENDCHAR" >> toNextLine
        return $ Just (name, code, points)

notGlyph = toNextLine *> return Nothing

property = do
    name <- some (letter_ascii <|> char '_')
    skipSpace
    value <- BS.unpack <$> takeWhile1 (/= '\n')
    endOfLine
    return (name, value)

parseBdf :: Parser (String, (Int, Int), [(BS.ByteString, Int, [Int])])
parseBdf = do
    ps <- property `manyTill` (string "ENDPROPERTIES")
    gs <- many (glyph <|> notGlyph)
    let size = maybe (error "size unknown") ((\[w,h] -> (read w, read h)) . take 2 . words) $ lookup "FONTBOUNDINGBOX" ps
    let name = maybe "unknown" (filter (/= '"')) $ lookup "FAMILY_NAME" ps
    let gs' = [x | Just x <- gs]
    return (name, size, gs')

showGlyph (name, code, points) =
    "\t/* " ++ (BS.unpack name) ++ " */\n\t" ++ 
    foldr (\x s -> "0x" ++ showHex x (",\n\t" ++ s)) "" points

output name (w,h) cs = 
    let nameSize = name ++ (show w) ++ "x" ++ (show h) in
    [
     "static u_char "++nameSize++"_data[];\n",
     "struct wsdisplay_font " ++ nameSize ++ "= {"
    ] ++ (map ("\t" ++) 
    [
     "\"" ++ name ++ "\", /* typeface name */",
     "0, /* index */",
     "' ', /* firstchar */",
     "256 - ' ', /* numchars */",
     "WSDISPLAY_FONTENC_ISO, /* encoding */",
     show w ++ ", /* width */",
     show h ++ ", /* height */",
     "1, /* stride */",
     "WSDISPLAY_FONTORDER_L2R, /* bit order */",
     "WSDISPLAY_FONTORDER_L2R, /* byte order */",
     "NULL, /* cookie */",
     nameSize ++ "_data /* data */",
     "};"]) ++
    ["static u_char "++nameSize ++ "_data[] = {"] ++
     (map showGlyph $ filter (\(_, code, _) -> code >= ord ' ' && code < 256) cs) ++ ["};"]

main = do
    [file] <- getArgs
    s <- BS.readFile file 
    case parseOnly parseBdf s of 
        Left s -> error s
        Right (n, s, gs) -> mapM putStrLn $ output n s gs
