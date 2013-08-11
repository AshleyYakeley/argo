module Data.Argo.Read where
{
    import Import;
    import Data.Argo.Expression;
    
    data Reference = LibReference String | SymbolReference String deriving (Eq);
    
    partitionReferences :: [Reference] -> ([String],[String]);
    partitionReferences [] = ([],[]);
    partitionReferences (LibReference s:rest) = case partitionReferences rest of
    {
        (a,b) -> (s:a,b);
    };
    partitionReferences (SymbolReference s:rest) = case partitionReferences rest of
    {
        (a,b) -> (a,s:b);
    };
    
    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
    };
    
    type ArgoExpression v = MonoValueExpression Reference v Identity;
    type ArgoPatternExpression v q = MonoPatternExpression String v q ();
    
    class SubValue value t where
    {
        toValue :: t -> value;
        fromValueMaybe :: value -> Maybe t;
    };

    isValue :: (Eq t,SubValue value t) => t -> value -> Bool;
    isValue t v = fromValueMaybe v == Just t;

    fromValue :: (SubValue value t) => value -> t;
    fromValue v = case fromValueMaybe v of
    {
        Just t -> t;
        Nothing -> error "wrong type";
    };
    
    class
    (
        SubValue v (),
        SubValue v Bool,
        SubValue v Rational,
        SubValue v String,
        SubValue v [v],
        SubValue v (v -> v)
    ) => ValueRead v where
    {
        valueTypeName :: v -> String;
    };

    evaluateWithLibs :: (Show v,ValueRead v,Applicative m,Monad m) => v -> (String -> m (Maybe String)) -> String -> m v;
    evaluateWithLibs stdlib libReader = evaluateSource lookup where
    {
        lookup "" = return stdlib;
        lookup libname = do
        {
            mlibtext <- libReader libname;
            case mlibtext of
            {
                Just libtext -> evaluateWithLibs stdlib libReader libtext;
                Nothing -> fail ("not found: $" ++ (show libname));
            };
        };
    };

    evaluateSource :: (Show v,ValueRead v,Applicative m,Monad m) => (String -> m v) -> String -> m v;
    evaluateSource libLookup s = do
    {
        expr <- readText s;
        (Identity r) <- monoEvaluateExpression resolve expr;
        return r;
    } where
    {
        resolve (SymbolReference sym) = fail ("undefined: " ++ sym);
        resolve (LibReference libname) = libLookup libname;
    };
    
    readText :: forall v m. (Show v,ValueRead v,Monad m) => String -> m (ArgoExpression v v);
    readText input = case readP_to_S readExpressionToEnd input of
    {
        [(a,"")] -> return a;
        [(_,s)] -> fail ("parser: unrecognised: " ++ s);
        [] -> fail "parser: invalid";
        ps@(_:_) -> fail ("parser: ambiguous: " ++ (intercalate "," (fmap (showExpr . fst) ps)));
    } where
    {
        showExpr :: ArgoExpression v v -> String;
        showExpr (MkExpression NilListType (Identity nr)) = show (nr ());
        showExpr exp = "(" ++ (intercalate "," (fmap show (expressionSymbols exp))) ++ ") -> value";
    
        readp :: Read a => ReadP a;
        readp = readPrec_to_P readPrec minPrec;

        manyMaximal :: ReadP a -> ReadP [a];
        manyMaximal p =  many1Maximal p <++ return [];

        many1Maximal :: ReadP a -> ReadP [a];
        many1Maximal p = liftA2 (:) p (manyMaximal p);

        readIntercalate :: ReadP () -> ReadP a -> ReadP [a];
        readIntercalate int ra = (do
        {
            first <- ra;
            rest <- manyMaximal (do
            {
                int;
                ra;
            });
            return (first:rest);
        }) <++ (return []);

        isLineBreak :: Char -> Bool;
        isLineBreak '\n' =  True;
        isLineBreak '\r' =  True;
        isLineBreak _ = False;

        readComment :: ReadP ();
        readComment = do
        {
            _ <- char '#';
            _ <- manyMaximal (satisfy (\c -> not (isLineBreak c)));
            _ <- satisfy isLineBreak;
            return ();
        };

        readWS :: ReadP ();
        readWS = do
        {
            skipSpaces;
            (do
            {
                readComment;
                readWS;
            }) <++ (return ());
        };

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
        goodChar '[' = False;
        goodChar ']' = False;
        goodChar ',' = False;
        goodChar ';' = False;
        goodChar '@' = False;
        goodChar c = not (isSpace c);
        
        firstChar :: Char -> Bool;
        firstChar = isAlpha;
        
        readIdentifierChar :: ReadP Char;
        readIdentifierChar = readEscapedChar <++ (satisfy goodChar);

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

        readUnderscored :: ReadP (v -> Bool);
        readUnderscored = do
        {
            readWSAndChar '_';
            typename <- manyMaximal readIdentifierChar;
            return (case typename of
            {
                "" -> \_ -> True;
                _ -> \v -> typename == (valueTypeName v);
            });
        };

        readNumber :: ReadP Rational;
        readNumber = do
        {
            readWS;
            (i :: Integer) <- readp;
            return (fromIntegral i);
        };

        readArrayPatternContents :: ReadP (ArgoPatternExpression v v);
        readArrayPatternContents = do
        {
            pats <- readIntercalate (readWSAndChar ',') readPattern;
            mextra <- optional (do
            {
                readWSAndChar ';';
                readPattern;
            });
            return (subPattern fromValueMaybe (listPat mextra pats));
        } where
        {
            maybeSplit (a:b) = Just (a,b);
            maybeSplit [] = Nothing;
            
            listPat mextra (pat1:patr) = subPattern maybeSplit (patternMatchPair pat1 (listPat mextra patr));
            listPat Nothing [] = patternMatch null;
            listPat (Just extra) [] = subPattern (Just . toValue) extra;
        };

        readConstExpression :: ReadP v;
        readConstExpression = do
        {
            exp <- readExpression;
            case evalExpression exp of
            {
                Right (Identity v) -> return v;
                Left _ -> mzero;
            };
        };

        readPatternField :: ReadP (ArgoPatternExpression v (v -> v));
        readPatternField = do
        {
            -- arg <- readExpression;
            arg <- readConstExpression;
            readWSAndChar ':';
            pat <- readPattern;
            return (subPattern (\f -> Just (f arg)) pat);
        };

        readFunctionPatternContents :: ReadP (ArgoPatternExpression v v);
        readFunctionPatternContents = do
        {
            fields <- readIntercalate (readWSAndChar ',') readPatternField;
            return (subPattern fromValueMaybe (matchAll fields));
        };

        readSinglePattern :: ReadP (ArgoPatternExpression v v);
        readSinglePattern = do
        {
            s <- readQuotedString;
            return (patternMatch (isValue s));
        } <++ do
        {
            n <- readNumber;
            return (patternMatch (isValue n));
        } <++ do
        {
            i <- readIdentifier;
            return (case i of
            {
                "null" -> patternMatch (isValue ());
                "true" -> patternMatch (isValue True);
                "false" -> patternMatch (isValue False);
                _ -> monoPatternSymbol i;
            });
        } <++ do
        {
            match <- readUnderscored;
            return (patternMatch match);
        } <++ do
        {
            readWSAndChar '[';
            pat <- readArrayPatternContents;
            readWSAndChar ']';
            return pat;
        } <++ do
        {
            readWSAndChar '{';
            pat <- readFunctionPatternContents;
            readWSAndChar '}';
            return pat;
        };

        readPattern :: ReadP (ArgoPatternExpression v v);
        readPattern = do
        {
            pat1 <- readSinglePattern;
            patr <- manyMaximal (do
            {
                readWSAndChar '@';
                readSinglePattern;
            });
            return (matchAll (pat1:patr));
        };

        readField :: ReadP (ArgoPatternExpression v v,ArgoExpression v v);
        readField = do
        {
            pat <- readPattern;
            readWSAndChar ':';
            result <- readExpression;
            return (pat,result);
        };

        argoBind :: ArgoPatternExpression v v -> ArgoExpression v r -> ArgoExpression v (v -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind (monoWitMap SymbolReference pat) exp));

        assembleFunction :: [(ArgoPatternExpression v v,ArgoExpression v v)] -> ArgoExpression v (v -> v);
        assembleFunction [] = pure (\_ -> toValue ());
        assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
        {
            Just r -> r;
            Nothing -> vv v;
        }) (argoBind pat exp) (assembleFunction ps);

        readFunction :: ReadP (ArgoExpression v v);
        readFunction = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readField;
            readWSAndChar '}';
            return (fmap toValue (assembleFunction fields));
        };
        
        readArrayContents :: ReadP (ArgoExpression v v);
        readArrayContents = do
        {
            exps <- readIntercalate (readWSAndChar ',') readExpression;
            mextra <- optional (do
            {
                readWSAndChar ';';
                readExpression;
            });
            return (fmap toValue (
            let
            {
                main = sequenceA exps;
            } in
            case mextra of
            {
                Just extra -> liftA2 (\m me -> case fromValueMaybe me of
                {
                    Just e -> m ++ e;
                    Nothing -> error "non-array after semicolon";
                }) main extra;
                Nothing -> main;
            }));
        };
        
        readTerm :: ReadP (ArgoExpression v v);
        readTerm = do
        {
            s <- readQuotedString;
            return (pure (toValue s));
        } <++ do
        {
            n <- readNumber;
            return (pure (toValue n));
        } <++ do
        {
            i <- readIdentifier;
            return (case i of
            {
                "null" -> pure (toValue ());
                "true" -> pure (toValue True);
                "false" -> pure (toValue False);
                _ -> monoValueSymbol (SymbolReference i);
            });
        } <++ do
        {
            readWSAndChar '[';
            exp <- readArrayContents;
            readWSAndChar ']';
            return exp;
        } <++ do
        {
            readWSAndChar '(';
            exp <- readExpression;
            readWSAndChar ')';
            return exp;
        } <++ readFunction <++ do
        {
            readWSAndChar '$';
            libname <- readQuotedString;
            return (monoValueSymbol (LibReference libname));
        };
        
        readExpression :: ReadP (ArgoExpression v v);
        readExpression = do
        {
            (f:args) <- some readTerm;
            return (applyArgs f args);
        } where
        {
            applyArgs expf [] = expf;
            applyArgs expf (expa:args) = applyArgs (liftA2 (\f a -> case fromValueMaybe f of
            {
                Just ff -> ff a;
                Nothing -> error "non-function application";
            }) expf expa) args;
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
