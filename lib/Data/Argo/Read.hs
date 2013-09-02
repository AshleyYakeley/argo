module Data.Argo.Read where
{
    import Import;
    import Data.Argo.Expression;
    import Data.Argo.MonoExpression;
    import Data.Argo.SubValue;
    import qualified Control.Monad.Trans.State;
    
    data Reference = ThisReference | LibReference String | SymbolReference String deriving (Eq);
   
    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
        show ThisReference = "$this";
    };
    
    type ArgoExpression v = MonoValueExpression Reference v Identity;
    type ArgoPatternExpression v q = MonoPatternExpression String v q ();
    
    class
    (
        SubValue v (),
        SubValue v Bool,
        SubValue v Rational,
        SubValue v String,
        SubValue v [v],
        SubValue v (v -> v),
        Show v
    ) => ValueRead v where
    {
        valueTypeName :: v -> String;
    };

    -- The recursive library lookup magic happens here.
    evaluateWithLibs :: forall v m. (Show v,ValueRead v,Applicative m,MonadFix m,?context::String) => (String -> m (Maybe (Either String v))) -> String -> m v;
    evaluateWithLibs libReader source = mdo
    {
        (v,dict) <- runStateT (evaluateSource (lookup dict) source) (\_ -> Nothing);
        return v;
    } where
    {
        lookup :: (?context::String) => (String -> Maybe v) -> String -> StateT (String -> Maybe v) m v;
        lookup dict libname = let {?context = ?context ++ ": $" ++ (show libname)} in do
        {
            curdict <- Control.Monad.Trans.State.get;
            case curdict libname of
            {
                Just _ -> return ();
                Nothing -> do
                {
                    mlibtext <- lift (libReader libname);
                    case mlibtext of
                    {
                        Just lib -> mdo
                        {
                            put (\libname' -> if libname == libname' then Just r else curdict libname');
                            r <- case lib of
                            {
                                Left libtext -> evaluateSource (lookup dict) libtext;
                                Right libval -> return libval;
                            };
                            return ();
                        };
                        Nothing -> failC "not found";
                    };
                };
            };
            return (case dict libname of
            {
                Just r' -> r';
                Nothing -> errorC "shouldn't happen: lookup failed";
            });
        };
    };

    evaluateSource :: (ValueRead v,Applicative m,MonadFix m,?context::String) => (String -> m v) -> String -> m v;
    evaluateSource libLookup s = mfix (\this -> let
    {
        resolve (SymbolReference sym) = failC ("undefined: " ++ sym);
        resolve (LibReference libname) = libLookup libname;
        resolve ThisReference = return this;
    } in do
    {
        expr <- readText s;
        Identity r <- monoEvaluateExpression resolve expr;
        return r;
    });
    
    readText :: forall v m. (ValueRead v,Monad m,?context::String) => String -> m (ArgoExpression v v);
    readText input = let
    {
        parseResult = readP_to_S readExpressionToEnd input;
        result = let
        {?context = ?context ++ ": parser"} in case parseResult of
        {
            [(a,"")] -> return a;
            [(_,s)] -> failC ("unrecognised: " ++ s);
            [] -> failC "invalid";
            _:_ -> failC "ambiguous";
        };
    } in result where
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
        goodChar '(' = False;
        goodChar ')' = False;
        goodChar ',' = False;
        goodChar ';' = False;
        goodChar '@' = False;
        goodChar '=' = False;
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
            _ <- optional (readWSAndChar ',');
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
            _ <- optional (readWSAndChar ',');
            readWSAndChar '}';
            return ( (matchAll fields));
        } where
        {
            readPatternField :: ReadP (ArgoPatternExpression v (v -> v));
            readPatternField = do
            {
                marg <- optional (do
                {
                    -- arg <- readExpression;
                    arg <- readConstExpression;
                    readWSAndChar ':';
                    return arg;
                });
                pat <- readPattern;
                return (subPattern (\f -> Just (f (fromMaybe (toValue ()) marg))) pat);
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

        argoBind :: ArgoPatternExpression v v -> ArgoExpression v r -> ArgoExpression v (v -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind (monoWitMap SymbolReference pat) exp));

        readFunction :: ReadP (ArgoExpression v (v -> v));
        readFunction = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readField;
            _ <- optional (readWSAndChar ',');
            readWSAndChar '}';
            return (assembleFunction fields);
        } where
        {
            readField :: ReadP (ArgoPatternExpression v v,ArgoExpression v v);
            readField = do
            {
                mpat <- optional (do
                {
                    pat <- readPattern;
                    readWSAndChar ':';
                    return pat;
                });
                result <- readExpression;
                return (fromMaybe (patternMatch (isValue ())) mpat,result);
            };

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
            _ <- optional (readWSAndChar ',');
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
                    Nothing -> errorC "non-array after semicolon";
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
                _ -> mzero;
            };
        } <++ 
        fmap (fmap toValue) readFunction <++ do
        {
            readWSAndChar '$';
            libname <- readQuotedString;
            return (monoValueSymbol (LibReference libname));
        };
        
        readExpressionNoLet :: ReadP (ArgoExpression v v);
        readExpressionNoLet = do
        {
            (f:args) <- some readTerm;
            return (applyArgs f args);
        } where
        {
            applyArgs expf [] = expf;
            applyArgs expf (expa:args) = applyArgs (liftA2 applyValue expf expa) args;
        };
        
        readExpression :: ReadP (ArgoExpression v v);
        readExpression = do
        {
            patExpr <- readPattern;
            readWSAndChar '=';
            bindExpr <- readExpression;
            readWSAndChar ',';
            valExpr <- readExpression;
            return (liftA2 (\vmv v -> fromMaybe (errorC "unmatched binding") (vmv v)) (argoBind patExpr valExpr) bindExpr);
        } <++ readExpressionNoLet;
        
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
