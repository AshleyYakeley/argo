module Data.Argo.Read where
{
    import Import;
    import Data.Argo.Number;
    import Data.Argo.Expression;
    import Data.Argo.MonoExpression;
    import Data.Argo.Value;
    import qualified Control.Monad.Trans.State;
    
    data Reference = ThisReference | LibReference String | SymbolReference String deriving (Eq);
   
    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
        show ThisReference = "$this";
    };
    
    type ArgoExpression = MonoValueExpression Reference Value Identity;
    type ArgoPatternExpression q = MonoPatternExpression String Value q ();

    -- The recursive library lookup magic happens here.
    evaluateWithLibs :: forall m. (Applicative m,MonadFix m,?context::String) => (String -> m (Maybe (Either String Value))) -> String -> m Value;
    evaluateWithLibs libReader source = mdo
    {
        (v,dict) <- runStateT (evaluateSource (lookup dict) source) (\_ -> Nothing);
        return v;
    } where
    {
        lookup :: (?context::String) => (String -> Maybe Value) -> String -> StateT (String -> Maybe Value) m Value;
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
                        Nothing -> failC "library not found";
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

    evaluateSource :: (Applicative m,MonadFix m,?context::String) => (String -> m Value) -> String -> m Value;
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
    
    readText :: forall m. (Monad m,?context::String) => String -> m (ArgoExpression Value);
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
        readIntercalate :: ReadP () -> ReadP a -> ReadP [a];
        readIntercalate int ra = (do
        {
            first <- ra;
            rest <- manyMax (do
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
            _ <- manyMax (satisfy (\c -> not (isLineBreak c)));
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

        readWSAndString :: String -> ReadP ();
        readWSAndString s = do
        {
            readWS;
            _ <- string s;
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
        goodChar '!' = False;
        goodChar c = not (isSpace c);
        
        firstChar :: Char -> Bool;
        firstChar = isAlpha;
        
        readIdentifierChar :: ReadP Char;
        readIdentifierChar = readEscapedChar <++ (satisfy goodChar);

        readQuotedString :: ReadP String;
        readQuotedString = do
        {
            readWSAndChar '"';
            s <- manyMax readQuotedChar;
            _ <- char '"';
            return s;
        };

        readIdentifier :: ReadP String;
        readIdentifier = do
        {
            readWS;
            first <- satisfy firstChar;
            rest <- manyMax readIdentifierChar;
            return (first:rest);
        };

        readUnderscored :: ReadP (Value -> Bool);
        readUnderscored = do
        {
            readWSAndChar '_';
            typename <- manyMax readIdentifierChar;
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
            readPNumber;
        };

        readArrayPattern :: ReadP (ArgoPatternExpression Value);
        readArrayPattern = do
        {
            readWSAndChar '[';
            pats <- readIntercalate (readWSAndChar ',') readPattern;
            _ <- optionalMax (readWSAndChar ',');
            mextra <- optionalMax (do
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

        readConstExpression :: ReadP Value;
        readConstExpression = do
        {
            exp <- readExpression;
            (Identity v) <- monoEvaluateExpression (\_ -> mzero) exp;
            return v;
        };

        readFunctionPattern :: ReadP (ArgoPatternExpression (Value -> Value));
        readFunctionPattern = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readPatternField;
            _ <- optionalMax (readWSAndChar ',');
            readWSAndChar '}';
            return ( (matchAll fields));
        } where
        {
            readPatternField :: ReadP (ArgoPatternExpression (Value -> Value));
            readPatternField = do
            {
                marg <- optionalMax (do
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

        readSinglePattern :: ReadP (ArgoPatternExpression Value);
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

        readPattern :: ReadP (ArgoPatternExpression Value);
        readPattern = do
        {
            pat1 <- readSinglePattern;
            patr <- manyMax (do
            {
                readWSAndChar '@';
                readSinglePattern;
            });
            return (matchAll (pat1:patr));
        };

        argoBind :: ArgoPatternExpression Value -> ArgoExpression r -> ArgoExpression (Value -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind (monoWitMap SymbolReference pat) exp));

        readFunction :: ReadP (ArgoExpression (Value -> Value));
        readFunction = do
        {
            readWSAndChar '{';
            fields <- readIntercalate (readWSAndChar ',') readField;
            _ <- optionalMax (readWSAndChar ',');
            readWSAndChar '}';
            return (assembleFunction fields);
        } where
        {
            readField :: ReadP (ArgoPatternExpression Value,ArgoExpression Value);
            readField = do
            {
                mpat <- optionalMax (do
                {
                    pat <- readPattern;
                    readWSAndChar ':';
                    return pat;
                });
                result <- readExpression;
                return (fromMaybe (patternMatch (isValue ())) mpat,result);
            };

            assembleFunction :: [(ArgoPatternExpression Value,ArgoExpression Value)] -> ArgoExpression (Value -> Value);
            assembleFunction [] = pure (\_ -> toValue ());
            assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
            {
                Just r -> r;
                Nothing -> vv v;
            }) (argoBind pat exp) (assembleFunction ps);
        };
        
        readArray :: ReadP (ArgoExpression [Value]);
        readArray = do
        {
            readWSAndChar '[';
            exps <- readIntercalate (readWSAndChar ',') readExpression;
            _ <- optionalMax (readWSAndChar ',');
            mextra <- optionalMax (do
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
        
        readActionExpression :: ReadP (ArgoExpression (IO Value));
        readActionExpression = do
        {
            exp <- readExpressionNoLet;
            return (fmap fromValue exp);
        };

        argoStrictBind :: ArgoPatternExpression Value -> ArgoExpression r -> ArgoExpression (Value -> r);
        argoStrictBind patExpr valExpr = fmap (\vmr v -> fromMaybe (errorC "unmatched binding") (vmr v)) (argoBind patExpr valExpr);
       
        readActionContents :: ReadP (ArgoExpression (IO Value));
        readActionContents = do
        {
            exp <- readActionExpression;
            readWSAndChar ',';
            rest <- readActionContents;
            return (liftA2 (>>) exp rest);
        } <++ do
        {
            patExpr <- readPattern;
            readWSAndChar '=';
            bindExpr <- readExpression;
            readWSAndChar ',';
            valExpr <- readActionContents;
            return ((argoStrictBind patExpr valExpr) <*> bindExpr);
        } <++ do
        {
            patExpr <- readPattern;
            readWSAndString "=!";
            bindExpr <- readActionExpression;
            readWSAndChar ',';
            valExpr <- readActionContents;
            return (liftA2 (>>=) bindExpr (argoStrictBind patExpr valExpr));
        } <++ do
        {
            expr <- readActionExpression;
            _ <- optionalMax (readWSAndChar ',');
            return expr;
        };
        
        readTerm :: ReadP (ArgoExpression Value);
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
            readWSAndString "![";
            action <- readActionContents;
            readWSAndChar ']';
            return (fmap toValue action);
        } <++ do
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
        
        readExpressionNoLet :: ReadP (ArgoExpression Value);
        readExpressionNoLet = do
        {
            (f:args) <- some readTerm;
            return (applyArgs f args);
        } where
        {
            applyArgs expf [] = expf;
            applyArgs expf (expa:args) = applyArgs (liftA2 applyValue expf expa) args;
        };
        
        readExpression :: ReadP (ArgoExpression Value);
        readExpression = do
        {
            patExpr <- readPattern;
            readWSAndChar '=';
            bindExpr <- readExpression;
            readWSAndChar ',';
            valExpr <- readExpression;
            return ((argoStrictBind patExpr valExpr) <*> bindExpr);
        } <++ readExpressionNoLet;
        
        readExpressionToEnd :: ReadP (ArgoExpression Value);
        readExpressionToEnd = do
        {
            exp <- readExpression;
            readWS;
            eof;
            return exp;
        };
    };
}
