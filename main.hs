{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Char
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric

toNextLine = skipWhile (/='\n') >> endOfLine

glyph :: Parser (Maybe (Int, (BS.ByteString, [Int])))
glyph = do
        string "STARTCHAR" >> skipSpace
        name <- takeWhile1 (/='\n') 
        toNextLine
        code <- (string "ENCODING" >> some space >> decimal)
        toNextLine `manyTill` (try $ string "BITMAP")
        toNextLine
        points <- (some (hexadecimal <* endOfLine))
        string "ENDCHAR" >> toNextLine
        return $ Just (code, (name, points))

notGlyph = toNextLine *> return Nothing

property = do
    name <- some (letter_ascii <|> char '_')
    skipSpace
    value <- BS.unpack <$> takeWhile1 (/= '\n')
    endOfLine
    return (name, value)

parseBdf :: Parser (String, (Int, Int), [(Int, (BS.ByteString, [Int]))])
parseBdf = do
    ps <- property `manyTill` (string "ENDPROPERTIES")
    gs <- many (glyph <|> notGlyph)
    let size = maybe (error "size unknown") ((\[w,h] -> (read w, read h)) . take 2 . words) $ lookup "FONTBOUNDINGBOX" ps
    let name = maybe "unknown" (filter (/= '"')) $ lookup "FAMILY_NAME" ps
    let gs' = [x | Just x <- gs]
    return (name, size, gs')

drawChar :: Int -> String
drawChar x = map (\b -> if b then '#' else '.') $ map (testBit x) [8, 7..0]

showGlyph :: (Int, (BS.ByteString, [Int])) -> String
showGlyph (code, (name, points)) =
    "\t/* " ++ (BS.unpack name) ++ " */\n\t" ++ 
    foldr (\x s -> "0x" ++ showHex x (",\t/* " ++ drawChar x ++ " */\n\t" ++ s)) "" points

output name (w,h) cs = 
    let nameSize = name ++ (show w) ++ "x" ++ (show h)
        lastCode = (\(code, _) -> code) . last $ cs 
        fillBlanks cs = map (\x -> (,) x $ Map.findWithDefault (BS.pack (show x), replicate h 0) x $ Map.fromList cs) [(ord ' ')..lastCode] in
    [
     "static u_char "++nameSize++"_data[];\n",
     "struct wsdisplay_font " ++ nameSize ++ "= {"
    ] ++ (map ("\t" ++) 
    [
     "\"" ++ name ++ "\", /* typeface name */",
     "0, /* index */",
     "' ', /* firstchar */",
     (show lastCode) ++ " - ' ', /* numchars */",
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
     (map showGlyph $ fillBlanks $ filter (\(code, _) -> code >= ord ' ') cs) ++ ["};"]

main = do
    [file] <- getArgs
    s <- BS.readFile file 
    case parseOnly parseBdf s of 
        Left s -> error s
        Right (n, s, gs) -> mapM putStrLn $ output n s gs
