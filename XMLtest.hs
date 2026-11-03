import System.IO
import XMLparser

isWhitespaceText :: XML -> Bool
isWhitespaceText (Text s) = all (`elem` " \t\n\r") s
isWhitespaceText _        = False

prettyPrint :: Int -> XML -> String
prettyPrint indent (Text s) = 
    if all (`elem` " \t\n\r") s 
        then "" 
        else replicate indent ' ' ++ s
prettyPrint indent (Element name attrs children) =
    let padding = replicate indent ' '
        
        renderAttr (k, v) = k ++ "=\"" ++ v ++ "\""
        attrStr = if null attrs 
                  then "" 
                  else " " ++ unwords (map renderAttr attrs)
        
        childIndent = indent + 4
        
        meaningfulChildren = filter (not . isWhitespaceText) children
        
        renderedChildren = concatMap (\c -> 
            let printed = prettyPrint childIndent c
            in if null printed then "" else printed ++ "\n"
            ) meaningfulChildren
            
    in padding ++ "<" ++ name ++ attrStr ++ ">\n" 
       ++ renderedChildren 
       ++ padding ++ "</" ++ name ++ ">"

main :: IO ()
main = do
    sample <- readFile' "test.xml"
    case xmlDocument sample of
        Just parsedXML -> putStrLn (prettyPrint 0 parsedXML)
        Nothing             -> putStrLn "Parsing failed!"