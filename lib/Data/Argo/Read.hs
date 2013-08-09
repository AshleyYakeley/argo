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
    
    readText :: forall v m. (Show v,ValueRead v, Monad m) => String -> m (ArgoExpression v v);
    readText input = case readP_to_S readExpressionToEnd input of
    {
        [(a,"")] -> return a;
        [(_,s)] -> fail ("unrecognised: " ++ s);
        [] -> fail "invalid";
        ps@(_:_) -> fail ("ambiguous: " ++ (intercalate "," (fmap (showExpr . fst) ps)));
    } where
    {
        showExpr :: ArgoExpression v v -> String;
        showExpr (MkExpression NilListType (Identity nr)) = show (nr ());
        showExpr exp = "(" ++ (intercalate "," (expressionSymbols exp)) ++ ") -> value";
    
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
                _ -> return c;
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

        manyMaximal :: ReadP a -> ReadP [a];
        manyMaximal p =  many1Maximal p <++ return [];

        many1Maximal :: ReadP a -> ReadP [a];
        many1Maximal p = liftA2 (:) p (manyMaximal p);


        readQuotedString :: ReadP String;
        readQuotedString = do
        {
            readWSAndChar '"';
            s <- manyMaximal readQuotedChar;
            _ <- char '"';
            return s;
        };

        readIdentifier :: ReadP String;
        readIdentifier = do
        {
            readWS;
            first <- satisfy firstChar;
            rest <- manyMaximal readIdentifierChar;
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
            pat <- readPattern;
            readWSAndChar ':';
            result <- readExpression;
            return (pat,result);
        };

        argoBind :: ArgoPatternExpression v -> ArgoExpression v r -> ArgoExpression v (v -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind pat exp));

        assembleFunction :: [(ArgoPatternExpression v,ArgoExpression v v)] -> ArgoExpression v (v -> v);
        assembleFunction [] = pure (\_ -> error "no match in {...}");
        assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
        {
            Just r -> r;
            Nothing -> vv v;
        }) (argoBind pat exp) (assembleFunction ps);

        readFunction :: ReadP (ArgoExpression v v);
        readFunction = do
        {
            readWSAndChar '{';
            fields <- manyMaximal readField;
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
        
        readExpressionToEnd :: ReadP (ArgoExpression v v);
        readExpressionToEnd = do
        {
            exp <- readExpression;
            readWS;
            eof;
            return exp;
        };
    };
}
