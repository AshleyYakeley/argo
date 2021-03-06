{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS -Wno-redundant-constraints #-}
#endif
module Data.Argo.Read where
{
    import Import hiding (many,(<|>),optional);
    import qualified Control.Monad.Trans.State;
    import Text.Parsec.String;
    import Text.Parsec;
    import Language.Expression;
    import Data.Argo.Number;
    import Data.Argo.Object;
    import Data.Argo.Value;
    import Data.Argo.Read.Lexical;
    import Data.Argo.Read.RegularExpression;

    data Reference = ThisReference | LibReference String | SymbolReference String deriving (Eq);

    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
        show ThisReference = "$this";
    };

    type ArgoExpression = MonoValueExpression Reference Value Identity;
    type ArgoPatternExpression q = MonoPatternExpression String Value Maybe q ();

    -- The recursive library lookup magic happens here.
    evaluateWithLibs :: forall m. (Applicative m,MonadFix m,?context::String) => (String -> m (Maybe (Either String Value))) -> String -> m Value;
    evaluateWithLibs libReader source = mdo
    {
        (v,dict) <- runStateT (evaluateSource (lookup dict) source) (\_ -> Nothing);
        return v;
    } where
    {
        lookup :: (String -> Maybe Value) -> String -> StateT (String -> Maybe Value) m Value;
        lookup dict libname = let {?context = libname} in do
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

    data ReadType t where
    {
        ExpressionReadType :: ReadType (ArgoExpression Value);

    };

    readText :: forall m. (Monad m,?context::String) => String -> m (ArgoExpression Value);
    readText = readThing ExpressionReadType;

    readThing :: forall t m. (Monad m,?context::String) => ReadType t -> String -> m t;
    readThing rtype input = let
    {
        parseResult = parse (readToEnd (readTypeThing rtype)) ?context input;
        result = case parseResult of
        {
            Right a -> return a;
            Left err -> fail (show err);
        };
    } in result where
    {
        readTypeThing :: ReadType t -> Parser t;
        readTypeThing ExpressionReadType = readExpression;

        readStringAndWS :: String -> Parser ();
        readStringAndWS s = do
        {
            _ <- string s;
            readWS;
            return ();
        };

        readIdentifierChar :: Parser Char;
        readIdentifierChar = readEscapedChar <|> (satisfy goodChar);

        readIdentifier :: Parser String;
        readIdentifier = do
        {
            first <- readEscapedChar <|> (satisfy isAlpha);
            rest <- many readIdentifierChar;
            readWS;
            return (first:rest);
        };

        readFieldLabel :: Parser String;
        readFieldLabel = readQuotedString <|> readIdentifier;

        readUnderscored :: Parser (Value -> Bool);
        readUnderscored = do
        {
            readCharAndWS '_';
            typename <- many readIdentifierChar;
            return (case typename of
            {
                "" -> \_ -> True;
                _ -> \v -> typename == (valueTypeName v);
            });
        };

        readNumber :: Parser Rational;
        readNumber = do
        {
            r <- readPNumber;
            readWS;
            return r;
        };

        readArrayPattern :: Parser (ArgoPatternExpression Value);
        readArrayPattern = do
        {
            readCharAndWS '[';
            pats <- readCommaSeparated readPattern;
            mextra <- optionMaybe (do
            {
                readCharAndWS ';';
                readPattern;
            });
            readCharAndWS ']';
            return (subPattern fromValueMaybe (listPat mextra pats));
        } where
        {
            maybeSplit (a:b) = Just (a,b);
            maybeSplit [] = Nothing;

            listPat mextra (pat1:patr) = subPattern maybeSplit (patternMatchPair pat1 (listPat mextra patr));
            listPat Nothing [] = patternMatch null;
            listPat (Just extra) [] = subPattern (Just . toValue) extra;
        };

        readConstExpression :: Parser Value;
        readConstExpression = do
        {
            exp <- readExpression;
            (Identity v) <- monoEvaluateExpression (\s -> parserFail ("expression not constant, open on " ++ show s)) exp;
            return v;
        };

        readObjectPatternInside :: Parser (ArgoPatternExpression (Object Value));
        readObjectPatternInside = do
        {
            fields <- readCommaSeparated readObjectField;
            return (matchAll fields);
        } where
        {
            readObjectField :: Parser (ArgoPatternExpression (Object Value));
            readObjectField = do
            {
                fieldLabel <- option "" (try (do
                {
                    fieldLabel <- readFieldLabel;
                    readCharAndWS ':';
                    return fieldLabel;
                }));
                pat <- readPattern;
                return (subPattern (\obj -> objectLookup obj fieldLabel) pat);
            };
        };

        readFunctionPatternInside :: Parser (ArgoPatternExpression (Value -> Value));
        readFunctionPatternInside = do
        {
            readCharAndWS '|';
            fields <- readCommaSeparated readPatternField;
            readCharAndWS '|';
            return (matchAll fields);
        } where
        {
            readPatternField :: Parser (ArgoPatternExpression (Value -> Value));
            readPatternField = do
            {
                -- arg <- readExpression;
                arg <- readConstExpression;
                readCharAndWS ':';
                pat <- readPattern;
                return (subPattern (\f -> Just (f arg)) pat);
            };
        };

        readBracedPattern :: Parser (ArgoPatternExpression Value);
        readBracedPattern = do
        {
            readCharAndWS '{';
            patexpr <-
                fmap (subPattern fromValueMaybe) readFunctionPatternInside <|>
                fmap (subPattern fromValueMaybe) readObjectPatternInside;
            readCharAndWS '}';
            return patexpr;
        };

        readSinglePattern :: Parser (ArgoPatternExpression Value);
        readSinglePattern = do
        {
            s <- readQuotedString;
            return (patternMatch (isValue s));
        } <|> do
        {
            n <- readNumber;
            return (patternMatch (isValue n));
        } <|> do
        {
            i <- readIdentifier;
            return (case i of
            {
                "null" -> patternMatch (isValue ());
                "true" -> patternMatch (isValue True);
                "false" -> patternMatch (isValue False);
                _ -> monoPatternSymbol i;
            });
        } <|> do
        {
            match <- readUnderscored;
            return (patternMatch match);
        } <|>
        readArrayPattern <|>
        readBracedPattern <|> do
        {
            regexp <- readRegularExpression;
            return (subPattern fromValueMaybe (regexSingle regexp));
        };

        readPattern :: Parser (ArgoPatternExpression Value);
        readPattern = do
        {
            pat1 <- readSinglePattern;
            patr <- many (do
            {
                readCharAndWS '@';
                readSinglePattern;
            });
            return (matchAll (pat1:patr));
        };

        argoBind :: ArgoPatternExpression a -> ArgoExpression r -> ArgoExpression (a -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind (monoWitMap SymbolReference pat) exp));

        argoStrictBind :: ArgoPatternExpression a -> ArgoExpression r -> ArgoExpression (a -> r);
        argoStrictBind patExpr valExpr = fmap (\vmr v -> fromMaybe (errorC "unmatched binding") (vmr v)) (argoBind patExpr valExpr);

        -- let pat = bind in val
        -- ==> (\pat -> val) bind
        patternBind :: ArgoPatternExpression a -> ArgoExpression a -> ArgoExpression r -> ArgoExpression r;
        patternBind patExpr bindExpr valExpr = (argoStrictBind patExpr valExpr) <*> bindExpr;

        -- letrec pat = bind in val
        -- ==> let pat = fix (\pat -> bind) in val
        patternRecBind :: (ArgoPatternExpression a,ArgoExpression a) -> ArgoExpression r -> ArgoExpression r;
        patternRecBind (patExpr,bindExpr) valExpr = patternBind patExpr (fmap fix (argoStrictBind patExpr bindExpr)) valExpr;

        combineBinds :: (forall b. (ArgoPatternExpression b,ArgoExpression b) -> r) -> [(ArgoPatternExpression a,ArgoExpression a)] -> r;
        combineBinds f [] = f (pure (),pure ());
        combineBinds f ((pat,bind):binds) = combineBinds (\(pat1,bind1) -> f (patternMatchPair pat pat1,liftA2 (,) bind bind1)) binds;

        recursiveBind :: [(ArgoPatternExpression a,ArgoExpression a)] -> ArgoExpression r -> ArgoExpression r;
        recursiveBind [] = id;  -- optimisation for common case
        recursiveBind binds = combineBinds patternRecBind binds;

        readObjectInside :: Parser (ArgoExpression (Object Value));
        readObjectInside = do
        {
            fields <- readCommaSeparated readField;
             return (fmap MkObject (traverse (\(s,exp) -> fmap (\v -> (s,v)) exp) fields));
        } where
        {
            readField :: Parser (String,ArgoExpression Value);
            readField = do
            {
                fieldLabel <- option "" (try (do
                {
                    fieldLabel <- readFieldLabel;
                    readCharAndWS ':';
                    return fieldLabel;
                }));
                fieldValueExpr <- readExpression;
                return (fieldLabel,fieldValueExpr);
            };
        };

        readFunctionInside :: Parser (ArgoExpression (Value -> Value));
        readFunctionInside = do
        {
            readCharAndWS '|';
            fields <- readCommaSeparated readField;
            readCharAndWS '|';
            return (assembleFunction fields);
        } where
        {
            readField :: Parser (ArgoPatternExpression Value,ArgoExpression Value);
            readField = do
            {
                pat <- readPattern;
                readCharAndWS ':';
                result <- readExpression;
                return (pat,result);
            };

            assembleFunction :: [(ArgoPatternExpression Value,ArgoExpression Value)] -> ArgoExpression (Value -> Value);
            assembleFunction [] = pure (\_ -> errorC "unmatched function application");
            assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
            {
                Just r -> r;
                Nothing -> vv v;
            }) (argoBind pat exp) (assembleFunction ps);
        };

        readBraced :: Parser (ArgoExpression Value);
        readBraced = do
        {
            readCharAndWS '{';
            expr <- (fmap (fmap toValue) readFunctionInside) <|> (fmap (fmap toValue) readObjectInside);
            readCharAndWS '}';
            return expr;
        };

        readArray :: Parser (ArgoExpression [Value]);
        readArray = do
        {
            readCharAndWS '[';
            exps <- readCommaSeparated readExpression;
            mextra <- optionMaybe (do
            {
                readCharAndWS ';';
                readExpression;
            });
            readCharAndWS ']';
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

        readActionExpression :: Parser (ArgoExpression (IO Value));
        readActionExpression = do
        {
            exp <- readExpressionNoLet;
            return (fmap fromValue exp);
        };

        readLet :: Parser (ArgoPatternExpression Value,ArgoExpression Value);
        readLet = do
        {
            (patExpr,argExprs) <- try (do
            {
                patExpr <- readPattern;
                argExprs <- many readPattern;
                readCharAndWS '=';
                return (patExpr,argExprs);
            });
            bodyExpr <- readExpression;
            return (patExpr,abstractExprs argExprs bodyExpr);
        } where
        {
            abstractExprs [] bodyExpr = bodyExpr;
            abstractExprs (arg:args) bodyExpr = abstractExpr arg (abstractExprs args bodyExpr);

            abstractExpr argExpr bodyExpr = fmap strictFunction (argoBind argExpr bodyExpr);

            strictFunction :: (Value -> Maybe Value) -> Value;
            strictFunction f = toValue (\v -> case f v of
            {
                Just r -> r;
                Nothing -> errorC "no match";
            });
        };

        readBinderComma :: Parser [(ArgoPatternExpression Value,ArgoExpression Value)];
        readBinderComma = do
        {
            readCharAndWS '@';
            val <- readConstExpression;
            readCharAndWS ',';
            case val of
            {
                ObjectValue (MkObject binds) -> return (fmap (\(n,v) -> (monoPatternSymbol n,pure v)) binds);
                _ -> mzero;
            };
        } <|> do
        {
            bind <- readLet;
            readCharAndWS ',';
            return [bind];
        };

        readActionRest :: Parser (ArgoExpression (IO Value));
        readActionRest = do
        {
            patExpr <- try (do
            {
                patExpr <- readPattern;
                readStringAndWS "=!";
                return patExpr;
            });
            bindExpr <- readActionExpression;
            readCharAndWS ',';
            valExpr <- readActionRest;
            return (liftA2 (>>=) bindExpr (argoStrictBind patExpr valExpr));
        } <|> do
        {
            binds <- readBinderComma;
            rest <- readActionRest;
            return (recursiveBind binds rest);
        } <|> do
        {
            expr <- readActionExpression;
            do
            {
                readCharAndWS ',';
                do
                {
                    readCharAndWS ']';
                    return expr;
                } <|> do
                {
                    rest <- readActionRest;
                    return (liftA2 (>>) expr rest);
                };
            } <|> do
            {
                readCharAndWS ']';
                return expr;
            }
        };

        readAction :: Parser (ArgoExpression (IO Value));
        readAction = do
        {
            readStringAndWS "![";
            readActionRest;
        };

        readDollarReference :: Parser Reference;
        readDollarReference = do
        {
            readCharAndWS '$';
            name <- readFieldLabel;
            case name of
            {
                "this" -> return ThisReference;
                _ -> return (LibReference name);
            };
        };

        readTermNoPostfix :: Parser (ArgoExpression Value);
        readTermNoPostfix = do
        {
            s <- readQuotedString;
            return (pure (toValue s));
        } <|> do
        {
            n <- readNumber;
            return (pure (toValue n));
        } <|> do
        {
            i <- readIdentifier;
            return (case i of
            {
                "null" -> pure (toValue ());
                "true" -> pure (toValue True);
                "false" -> pure (toValue False);
                _ -> monoValueSymbol (SymbolReference i);
            });
        } <|>
        fmap (fmap toValue) readArray <|> do
        {
            action <- readAction;
            return (fmap toValue action);
        } <|> do
        {
            readCharAndWS '(';
            exp <- readExpression;
            readCharAndWS ')';
            return exp;
        } <|>
        readBraced <|>
        fmap monoValueSymbol readDollarReference;

        postfixLookup :: String -> Value -> Value;
        postfixLookup fieldLabel val = case objectLookup (fromValue val) fieldLabel of
        {
            Just r -> r;
            Nothing -> toValue ();
        };

        readPostfix :: Parser (ArgoExpression (Value -> Value));
        readPostfix = do
        {
            readCharAndWS '.';
            fieldLabel <- readFieldLabel;
            return (pure (postfixLookup fieldLabel));
        };

        readTerm :: Parser (ArgoExpression Value);
        readTerm = do
        {
            term <- readTermNoPostfix;
            postfixes <- many readPostfix;
            return (foldl (\t f -> f <*> t) term postfixes);
        };

        readExpressionNoLet :: Parser (ArgoExpression Value);
        readExpressionNoLet = do
        {
            (f:args) <- some readTerm;
            return (applyArgs f args);
        } where
        {
            applyArgs expf [] = expf;
            applyArgs expf (expa:args) = applyArgs (liftA2 applyValue expf expa) args;
        };

        readExpression :: Parser (ArgoExpression Value);
        readExpression = do
        {
            bindingss <- many readBinderComma;
            expr <- readExpressionNoLet;
            return (recursiveBind (concat bindingss) expr);
        };

        readToEnd :: Parser t -> Parser t;
        readToEnd r = do
        {
            readWS;
            t <- r;
            optional (readCharAndWS ',');
            eof;
            return t;
        };
    };
}
