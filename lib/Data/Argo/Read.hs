module Data.Argo.Read where
{
    import Import;
    import Data.Argo.Expression;
    import Data.Argo.MonoExpression;
    
    data Reference = ThisReference | StdReference | LibReference String | SymbolReference String deriving (Eq);
   
    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
        show ThisReference = "$this";
        show StdReference = "$std";
    };
    
    type ArgoExpression v = MonoValueExpression Reference v Identity;
    type ArgoPatternExpression v q = MonoPatternExpression String v q ();
    
    class SubValue value t where
    {
        toValue :: t -> value;
        fromValueMaybe :: value -> Maybe t;
    };

    data ValueType v t = MkValueType
    {
        valueTypeName' :: String,
        readValueType :: ReadP (ArgoExpression v t)
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

    evaluateWithLibs :: (Show v,ValueRead v,Applicative m,MonadFix m) => v -> (String -> m (Maybe String)) -> String -> m v;
    evaluateWithLibs stdlib libReader = evaluateSource stdlib lookup where
    {
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

    evaluateSource :: (ValueRead v,Applicative m,MonadFix m) => v -> (String -> m v) -> String -> m v;
    evaluateSource stdlib libLookup s = mfix (\this -> let
    {
        resolve (SymbolReference sym) = fail ("undefined: " ++ sym);
        resolve (LibReference libname) = libLookup libname;
        resolve ThisReference = return this;
        resolve StdReference = return stdlib;
    } in do
    {
        expr <- readText s;
        (Identity r) <- monoEvaluateExpression resolve expr;
        return r;
    });
    
    readText :: forall v m. (ValueRead v,Monad m) => String -> m (ArgoExpression v v);
    readText input = case readP_to_S readExpressionToEnd input of
    {
        [(a,"")] -> return a;
        [(_,s)] -> fail ("parser: unrecognised: " ++ s);
        [] -> fail "parser: invalid";
        _:_ -> fail "parser: ambiguous";
    } where
    {
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

        readArrayPattern :: ReadP (ArgoPatternExpression v v);
        readArrayPattern = do
        {
            readWSAndChar '[';
            pats <- readIntercalate (readWSAndChar ',') readPattern;
            mextra <- optional (do
            {
                readWSAndChar ';';
                readPattern;
            });
            readWSAndChar ']';
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
            (Identity v) <- monoEvaluateExpression (\_ -> mzero) exp;
            return v;
        };

        readFunctionPattern :: ReadP (ArgoPatternExpression v (v -> v));
        readFunctionPattern = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readPatternField;
            readWSAndChar '}';
            return ( (matchAll fields));
        } where
        {
            readPatternField :: ReadP (ArgoPatternExpression v (v -> v));
            readPatternField = do
            {
                -- arg <- readExpression;
                arg <- readConstExpression;
                readWSAndChar ':';
                pat <- readPattern;
                return (subPattern (\f -> Just (f arg)) pat);
            };
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
        } <++
        readArrayPattern <++
        fmap (subPattern fromValueMaybe) readFunctionPattern;

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


        readFunction :: ReadP (ArgoExpression v (v -> v));
        readFunction = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readField;
            readWSAndChar '}';
            return (assembleFunction fields);
        } where
        {
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
        };
        
        readArray :: ReadP (ArgoExpression v [v]);
        readArray = do
        {
            readWSAndChar '[';
            exps <- readIntercalate (readWSAndChar ',') readExpression;
            mextra <- optional (do
            {
                readWSAndChar ';';
                readExpression;
            });
            readWSAndChar ']';
            return (
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
            });
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
        } <++
        fmap (fmap toValue) readArray <++ do
        {
            readWSAndChar '(';
            exp <- readExpression;
            readWSAndChar ')';
            return exp;
        } <++ 
        fmap (fmap toValue) readFunction <++ do
        {
            readWSAndChar '$';
            name <- readIdentifier;
            case name of
            {
                "this" -> return (monoValueSymbol ThisReference);
                "std" -> return (monoValueSymbol StdReference);
                _ -> mzero;
            };
        } <++ 
        fmap (fmap toValue) readFunction <++ do
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
