module Data.Thing.Thing(Thing(..)) where
{
    import Import;

    data Thing = MkThing String [(String,Thing)] deriving Eq;
    
    goodChar :: Char -> Bool;
    goodChar '#' = False;
    goodChar '\\' = False;
    goodChar ':' = False;
    goodChar '"' = False;
    goodChar '{' = False;
    goodChar '}' = False;
    goodChar c = not (isSpace c);
    
    escapeChar :: Char -> String;
    escapeChar '\\' = "\\\\";
    escapeChar '"' = "\"";
    escapeChar c = [c];
    
    showString :: String -> String;
    showString s | all goodChar s = s;
    showString s = "\"" ++ (concat (fmap escapeChar s)) ++ "\"";
    
    showField :: (String,Thing) -> String;
    showField ("",t) = show t;
    showField (label,t) = (showString label) ++ ":" ++ (show t);
    
    showFields :: [(String,Thing)] -> String;
    showFields [] = ""; -- never
    showFields (field:fields) = "{" ++ (showField field) ++ (concat (fmap (\f -> " " ++ (showField f)) fields)) ++ "}";
    
    instance Show Thing where
    {
        show (MkThing head []) = showString head;
        show (MkThing "" fields) = showFields fields;
        show (MkThing head fields) = (showString head) ++ (showFields fields);
    };
    

    readEscapedChar :: ReadP Char;
    readEscapedChar = do
    {
        _ <- char '\\';
        c <- get;
        case c of
        {
            'n' -> return '\n';
            't' -> return '\t';
            'r' -> return '\r';
            'f' -> return '\f';
            c -> return c;
        };
    };

    readQuotedChar :: ReadP Char;
    readQuotedChar = readEscapedChar <++ (satisfy ('"' /=));

    readUnquotedChar :: ReadP Char;
    readUnquotedChar = readEscapedChar <++ (satisfy goodChar);

    readString :: ReadP String;
    readString = (do
    {
        _ <- char '"';
        s <- many readQuotedChar;
        _ <- char '"';
        return s;
    }) <++ (many readUnquotedChar);

    readWS :: ReadP ();
    readWS = skipSpaces;

    readWSAndChar :: Char -> ReadP ();
    readWSAndChar c = do
    {
        readWS;
        _ <- char c;
        return ();
    };
    
    readWSAndString :: ReadP String;
    readWSAndString = readWS >> readString;
    
    readLabel :: ReadP String;
    readLabel = option "" (do
    {
        label <- readWSAndString;
        readWSAndChar ':';
        return label;
    });
    
    readField :: ReadP (String,Thing);
    readField = do
    {
        label <- readLabel;
        value ::Thing <- readPrec_to_P readPrec 0;
        return (label,value);
    };
    
    readBlock :: ReadP [(String,Thing)];
    readBlock = do
    {
        readWSAndChar '{';
        fields <- many readField;
        readWSAndChar '}';
        return fields;
    };
    
    instance Read Thing where
    {
        readPrec = readP_to_Prec (\_ -> (do
        {
            head <- readWSAndString;
            fields <- option [] readBlock;
            readWS;
            return (MkThing head fields);
        }) <++ (do
        {
            fields <- readBlock;
            readWS;
            return (MkThing "" fields);
        }));
        readListPrec = readListPrecDefault;
    };
}
