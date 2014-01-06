module Data.Argo.Read.Lexical where
{
    import Import hiding (many,(<|>),optional);
    import Text.Parsec.String;
    import Text.Parsec;


    readSeparated :: Parser () -> Parser a -> Parser [a];
    readSeparated int ra = (do
    {
        first <- ra;
        mrest <- optionMaybe (do
        {
            int;
            -- allow extra comma at end
            readSeparated int ra;
        });
        return (first:fromMaybe [] mrest);
    }) <|> (return []);

    readCommaSeparated :: Parser a -> Parser [a];
    readCommaSeparated = readSeparated (readCharAndWS ',');

    readEscapedChar :: Parser Char;
    readEscapedChar = do
    {
        _ <- char '\\';
        c <- anyToken;
        case c of
        {
            'n' -> return '\n';
            't' -> return '\t';
            'r' -> return '\r';
            'f' -> return '\f';
            _ -> return c;
        };
    };

    readQuotedString :: Parser String;
    readQuotedString = do
    {
        _ <- char '"';
        s <- many readQuotedChar;
        readCharAndWS '"';
        return s;
    } where
    {
        readQuotedChar :: Parser Char;
        readQuotedChar = readEscapedChar <|> (satisfy ('"' /=));
    };

    goodChar :: Char -> Bool;
    goodChar '#' = False;   -- comment
    goodChar '\\' = False;  -- char escape
    goodChar ':' = False;   -- objects/functions
    goodChar '"' = False;   -- string
    goodChar '{' = False;   -- objects/functions
    goodChar '}' = False;   -- objects/functions
    goodChar '|' = False;   -- functions
    goodChar '[' = False;   -- arrays
    goodChar ']' = False;   -- arrays
    goodChar '(' = False;   -- expression grouping
    goodChar ')' = False;   -- expression grouping
    goodChar ',' = False;   -- separator
    goodChar ';' = False;   -- array first/rest separator
    goodChar '@' = False;   -- pattern "and"
    goodChar '=' = False;   -- bindings
    goodChar '!' = False;   -- actions
    goodChar '$' = False;   -- library references
    goodChar '.' = False;   -- object field specifier
    goodChar c = not (isSpace c);

    goodREChar :: Char -> Bool;
    goodREChar '/' = False;
    goodREChar '?' = False;
    goodREChar '*' = False;
    goodREChar '+' = False;
    goodREChar c = goodChar c;

    readWS :: Parser ();
    readWS = do
    {
        spaces;
        optional (do
        {
            readComment;
            readWS;
        });
    } where
    {
        isLineBreak :: Char -> Bool;
        isLineBreak '\n' =  True;
        isLineBreak '\r' =  True;
        isLineBreak _ = False;

        readComment :: Parser ();
        readComment = do
        {
            _ <- char '#';
            _ <- many (satisfy (\c -> not (isLineBreak c)));
            _ <- satisfy isLineBreak;
            return ();
        };
    };

    readCharAndWS :: Char -> Parser ();
    readCharAndWS c = do
    {
        _ <- char c;
        readWS;
    };
}
