module Data.Argo.Read where
{
    import Import;
    import Data.Argo.Expression;
    
    type ArgoExpression v = MonoValueExpression String v Identity;
    type ArgoPatternExpression v = MonoPatternExpression String v v ();
    
    class ValueRead v where
    {
        valueConstant :: String -> Maybe v;
        valueIsConstant :: String -> Maybe (v -> Bool);
        valueFromString :: String -> v;
        valueIsString :: String -> v -> Bool;
        valueFromNumber :: Rational -> v;
        valueIsNumber :: Rational -> v -> Bool;
        valueFromFunction :: (v -> v) -> v;
        valueApply :: v -> v -> v;
    };
    
    readText :: forall v m. (ValueRead v, Monad m) => String -> m (ArgoExpression v v);
    readText s = case readP_to_S readExpressionWS s of
    {
        [(a,"")] -> return a;
        [(_,s)] -> fail ("unrecognised: " ++ s);
        [] -> fail ("invalid");
        _:_ -> fail ("ambiguous");
    } where
    {
        readp :: Read a => ReadP a;
        readp = readPrec_to_P readPrec minPrec;
    
        readWS :: ReadP ();
        readWS = skipSpaces;

        readWSAndChar :: Char -> ReadP ();
        readWSAndChar c = do
        {
            readWS;
            _ <- char c;
            return ();
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

        goodChar :: Char -> Bool;
        goodChar '#' = False;
        goodChar '\\' = False;
        goodChar ':' = False;
        goodChar '"' = False;
        goodChar '{' = False;
        goodChar '}' = False;
        goodChar c = not (isSpace c);
        
        firstChar :: Char -> Bool;
        firstChar = isAlpha;
        
        readIdentifierChar :: ReadP Char;
        readIdentifierChar = readEscapedChar <++ (satisfy goodChar);

        readQuotedString :: ReadP String;
        readQuotedString = do
        {
            readWSAndChar '"';
            s <- many readQuotedChar;
            _ <- char '"';
            return s;
        };

        readIdentifier :: ReadP String;
        readIdentifier = do
        {
            readWS;
            first <- satisfy firstChar;
            rest <- many readIdentifierChar;
            return (first:rest);
        };

        readNumber :: ReadP Rational;
        readNumber = do
        {
            readWS;
            (i :: Integer) <- readp;
            return (fromIntegral i);
        };

        readPattern :: ReadP (ArgoPatternExpression v);
        readPattern = do
        {
            s <- readQuotedString;
            return (patternMatch (valueIsString s));
        } <++ do
        {
            n <- readNumber;
            return (patternMatch (valueIsNumber n));
        } <++ do
        {
            i <- readIdentifier;
            return (case valueIsConstant i of
            {
                Just fv -> patternMatch fv;
                Nothing -> monoPatternSymbol i;
            });
        };

        readField :: ReadP (ArgoPatternExpression v,ArgoExpression v v);
        readField = do
        {
            pattern <- readPattern;
            readWSAndChar ':';
            result <- readExpression;
            return (pattern,result);
        };

        assembleFunction :: [(ArgoPatternExpression v,ArgoExpression v v)] -> ArgoExpression v (v -> v);
        assembleFunction [] = pure (\_ -> error "no match in {...}");
        assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
        {
            Just r -> r;
            Nothing -> vv v;
        }) (monoPatternBind pat exp) (assembleFunction ps);

        readFunction :: ReadP (ArgoExpression v v);
        readFunction = do
        {
            readWSAndChar '{';
            fields <- many readField;
            readWSAndChar '}';
            return (fmap valueFromFunction (assembleFunction fields));
        };
        
        readTerm :: ReadP (ArgoExpression v v);
        readTerm = do
        {
            s <- readQuotedString;
            return (pure (valueFromString s));
        } <++ do
        {
            n <- readNumber;
            return (pure (valueFromNumber n));
        } <++ do
        {
            i <- readIdentifier;
            return (case valueConstant i of
            {
                Just v -> pure v;
                Nothing -> monoValueSymbol i;
            });
        } <++ do
        {
            readWSAndChar '(';
            exp <- readExpression;
            readWSAndChar ')';
            return exp;
        } <++ readFunction;
        
        readExpression :: ReadP (ArgoExpression v v);
        readExpression = do
        {
            (f:args) <- some readTerm;
            return (applyArgs f args);
        } where
        {
            applyArgs f [] = f;
            applyArgs f (a:args) = applyArgs (liftA2 valueApply f a) args;
        };
        
        readExpressionWS :: ReadP (ArgoExpression v v);
        readExpressionWS = do
        {
            exp <- readExpression;
            readWS;
            return exp;
        };
    };
}
