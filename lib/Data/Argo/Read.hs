module Data.Argo.Read where
{
    import Import hiding (many,(<|>),optional);
    import qualified Control.Monad.Trans.State;
    import Text.Parsec.String;
    import Text.Parsec;
    import Language.Expression.Expression;
    import Language.Expression.Mono;
    import Language.Expression.Regular;
    import Data.Argo.Number;
    import Data.Argo.Value;

    data Reference = ThisReference | LibReference String | SymbolReference String deriving (Eq);

    instance Show Reference where
    {
        show (SymbolReference s) = s;
        show (LibReference s) = '$':(show s);
        show ThisReference = "$this";
    };

    type ArgoExpression = MonoValueExpression Reference Value Identity;
    type ArgoPatternExpression q = MonoPatternExpression String Value Maybe q ();
    type ArgoRegularExpression = MonoRegularExpression String Value String;

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

    readText :: forall m. (Monad m,?context::String) => String -> m (ArgoExpression Value);
    readText input = let
    {
        parseResult = parse readExpressionToEnd ?context input;
        result = case parseResult of
        {
            Right a -> return a;
            Left err -> fail (show err);
        };
    } in result where
    {
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

        readWS :: Parser ();
        readWS = do
        {
            spaces;
            optional (do
            {
                readComment;
                readWS;
            });
        };

        readCharAndWS :: Char -> Parser ();
        readCharAndWS c = do
        {
            _ <- char c;
            readWS;
        };

        readStringAndWS :: String -> Parser ();
        readStringAndWS s = do
        {
            _ <- string s;
            readWS;
            return ();
        };

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

        readQuotedChar :: Parser Char;
        readQuotedChar = readEscapedChar <|> (satisfy ('"' /=));

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
        goodChar '$' = False;
        goodChar c = not (isSpace c);

        goodREChar :: Char -> Bool;
        goodREChar '/' = False;
        goodREChar '?' = False;
        goodREChar '*' = False;
        goodREChar '+' = False;
        goodREChar c = goodChar c;

        readQuotedString :: Parser String;
        readQuotedString = do
        {
            _ <- char '"';
            s <- many readQuotedChar;
            readCharAndWS '"';
            return s;
        };

        readIdentifierChar :: Parser Char;
        readIdentifierChar = readEscapedChar <|> (satisfy goodChar);

        readIdentifier :: Parser String;
        readIdentifier = do
        {
            first <- satisfy isAlpha;
            rest <- many readIdentifierChar;
            readWS;
            return (first:rest);
        };

        readREIdentifier :: Parser String;
        readREIdentifier = do
        {
            first <- satisfy isAlpha;
            rest <- many (readEscapedChar <|> (satisfy goodREChar));
            readWS;
            return (first:rest);
        };

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
            (Identity v) <- monoEvaluateExpression (\_ -> mzero) exp;
            return v;
        };

        readFunctionPattern :: Parser (ArgoPatternExpression (Value -> Value));
        readFunctionPattern = do
        {
            readCharAndWS '{';
            fields <- readCommaSeparated readPatternField;
            readCharAndWS '}';
            return ( (matchAll fields));
        } where
        {
            readPatternField :: Parser (ArgoPatternExpression (Value -> Value));
            readPatternField = do
            {
                arg <- option (toValue ()) (try (do
                {
                    -- arg <- readExpression;
                    arg <- readConstExpression;
                    readCharAndWS ':';
                    return arg;
                }));
                pat <- readPattern;
                return (subPattern (\f -> Just (f arg)) pat);
            };
        };

        readRE :: Parser ArgoRegularExpression;
        readRE = do
        {
            terms <- readCommaSeparated readRETerm;
            return (foldTerms terms);
        } where
        {
            foldTerms [] = regexEmpty;
            foldTerms [r] = r;
            foldTerms (r:rs) = regexConcat (\_ _ v -> v) r (foldTerms rs);
        };

        reNull :: MonoSymbol a Value val -> val;
        reNull (MkMonoSymbol _) = toValue ();

        reArrayEmpty :: MonoSymbol a Value val -> val;
        reArrayEmpty (MkMonoSymbol _) = toValue ([] :: [Value]);

        reArrayCons :: MonoSymbol a Value val -> val -> val -> val;
        reArrayCons (MkMonoSymbol _) first rest = toValue (first:fromValue rest);

        readRETerm :: Parser ArgoRegularExpression;
        readRETerm = do
        {
            first <- readREFactor;
            mrest <- optionMaybe (do
            {
                readCharAndWS '|';
                readRETerm;
            });
            return (case mrest of
            {
                Just rest -> regexAlternate reNull first rest;
                Nothing -> first;
            });
        };

        readREFactor :: Parser ArgoRegularExpression;
        readREFactor = do
        {
            first <- readREElement;
            mrest <- optionMaybe (do
            {
                readCharAndWS '@';
                readREFactor;
            });
            return (case mrest of
            {
                Just rest -> regexParallel first rest;
                Nothing -> first;
            });
        };

        readREElement :: Parser ArgoRegularExpression;
        readREElement = do
        {
            item <- readREItem;
            mmod <- optionMaybe (do
            {
                readCharAndWS '?';
                return Nothing;
            } <|> do
            {
                readCharAndWS '*';
                return (Just (0,Nothing));
            } <|> do
            {
                readCharAndWS '+';
                return (Just (1,Nothing));
            } <|> do
            {
                readCharAndWS '{';
                a <- readDigits;
                readWS;
                mmb <- optionMaybe (do
                {
                    readCharAndWS ',';
                    optionMaybe (do
                    {
                        mb <- readDigits;
                        readWS;
                        return mb;
                    });
                });
                readCharAndWS '}';
                return (Just (a,case mmb of
                {
                    Nothing -> Just a;
                    Just mb -> mb;
                }));
            });
            return (case mmod of
            {
                Just Nothing -> regexAlternate reNull item regexEmpty;
                Just (Just (a,mb)) -> regexRepeat (fromInteger a) (fmap fromInteger mb) reArrayEmpty reArrayCons item;
                Nothing -> item;
            });
        };

        isGeneralCategory :: GeneralCategory -> Char -> Bool;
        isGeneralCategory gc c = generalCategory c == gc;

        readCharClass :: Parser (Char -> Bool);
        readCharClass = do
        {
            s <- readQuotedString;
            return (\c -> elem c s);
        } <|> do
        {
            name <- readREIdentifier;
            case name of
            {
                "Lu" -> return (isGeneralCategory UppercaseLetter);
                "Ll" -> return (isGeneralCategory LowercaseLetter);
                "Lt" -> return (isGeneralCategory TitlecaseLetter);
                "Lm" -> return (isGeneralCategory ModifierLetter);
                "Lo" -> return (isGeneralCategory OtherLetter);
                "Mn" -> return (isGeneralCategory NonSpacingMark);
                "Mc" -> return (isGeneralCategory SpacingCombiningMark);
                "Me" -> return (isGeneralCategory EnclosingMark);
                "Nd" -> return (isGeneralCategory DecimalNumber);
                "Nl" -> return (isGeneralCategory LetterNumber);
                "No" -> return (isGeneralCategory OtherNumber);
                "Pc" -> return (isGeneralCategory ConnectorPunctuation);
                "Pd" -> return (isGeneralCategory DashPunctuation);
                "Ps" -> return (isGeneralCategory OpenPunctuation);
                "Pe" -> return (isGeneralCategory ClosePunctuation);
                "Pi" -> return (isGeneralCategory InitialQuote);
                "Pf" -> return (isGeneralCategory FinalQuote);
                "Po" -> return (isGeneralCategory OtherPunctuation);
                "Sm" -> return (isGeneralCategory MathSymbol);
                "Sc" -> return (isGeneralCategory CurrencySymbol);
                "Sk" -> return (isGeneralCategory ModifierSymbol);
                "So" -> return (isGeneralCategory OtherSymbol);
                "Zs" -> return (isGeneralCategory Space);
                "Zl" -> return (isGeneralCategory LineSeparator);
                "Zp" -> return (isGeneralCategory ParagraphSeparator);
                "Cc" -> return (isGeneralCategory Control);
                "Cf" -> return (isGeneralCategory Format);
                "Cs" -> return (isGeneralCategory Surrogate);
                "Co" -> return (isGeneralCategory PrivateUse);
                "Cn" -> return (isGeneralCategory NotAssigned);

                "L" -> return (\c -> case generalCategory c of
                {
                    UppercaseLetter -> True;
                    LowercaseLetter -> True;
                    TitlecaseLetter -> True;
                    ModifierLetter -> True;
                    OtherLetter -> True;
                    _ -> False;
                });
                "M" -> return (\c -> case generalCategory c of
                {
                    NonSpacingMark -> True;
                    SpacingCombiningMark -> True;
                    EnclosingMark -> True;
                    _ -> False;
                });
                "N" -> return (\c -> case generalCategory c of
                {
                    DecimalNumber -> True;
                    LetterNumber -> True;
                    OtherNumber -> True;
                    _ -> False;
                });
                "P" -> return (\c -> case generalCategory c of
                {
                    ConnectorPunctuation -> True;
                    DashPunctuation -> True;
                    OpenPunctuation -> True;
                    ClosePunctuation -> True;
                    InitialQuote -> True;
                    FinalQuote -> True;
                    OtherPunctuation -> True;
                    _ -> False;
                });
                "S" -> return (\c -> case generalCategory c of
                {
                    MathSymbol -> True;
                    CurrencySymbol -> True;
                    ModifierSymbol -> True;
                    OtherSymbol -> True;
                    _ -> False;
                });
                "Z" -> return (\c -> case generalCategory c of
                {
                    Space -> True;
                    LineSeparator -> True;
                    ParagraphSeparator -> True;
                    _ -> False;
                });
                "C" -> return (\c -> case generalCategory c of
                {
                    Control -> True;
                    Format -> True;
                    Surrogate -> True;
                    PrivateUse -> True;
                    NotAssigned -> True;
                    _ -> False;
                });

                -- http://perldoc.perl.org/perlrecharclass.html#POSIX-Character-Classes
                "cntrl" -> return isControl;
                "space" -> return isSpace;
                "lower" -> return isLower;
                "upper" -> return isUpper;
                "alpha" -> return isAlpha;
                "alnum" -> return isAlphaNum;
                "print" -> return isPrint;
                "digit" -> return isDigit;
                "odigit" -> return isOctDigit;
                "xdigit" -> return isHexDigit;
                "ascii" -> return isAscii;
                "ascii-lower" -> return isAsciiLower;
                "ascii-upper" -> return isAsciiUpper;
                "latin1" -> return isLatin1;
                "blank" -> return isSeparator;
                "graph" -> return (\c -> (isPrint c) && not (isSpace c));
                "punct" -> return isPunctuation;

                _ -> fail ("character class " ++ (show name) ++" not recognised");
            };
        };

        anyf :: [a -> Bool] -> a -> Bool;
        anyf [] _ = False;
        anyf (f:_) a | f a = True;
        anyf (_:ff) a = anyf ff a;

        readREItem :: Parser ArgoRegularExpression;
        readREItem = do
        {
            readCharAndWS '.';
            return regexAnyChar;
        } <|> do
        {
            name <- readREIdentifier;
            return (regexSymbol toValue (MkMonoSymbol name) regexAnything);
        } <|> do
        {
            s <- readQuotedString;
            return (regexText s);
        } <|> do
        {
            readCharAndWS '[';
            mneg <- optionMaybe (readCharAndWS '^');
            mm <- many readCharClass;
            readCharAndWS ']';
            return (regexSet (case mneg of
            {
                Just () -> \c -> not (anyf mm c);
                Nothing -> anyf mm;
            }));
        } <|> do
        {
            readCharAndWS '(';
            regex <- readRE;
            readCharAndWS ')';
            return regex;
        };

        readRegularExpression :: Parser ArgoRegularExpression;
        readRegularExpression = do
        {
            readCharAndWS '/';
            regex <- readRE;
            readCharAndWS '/';
            return regex;
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
        fmap (subPattern fromValueMaybe) readFunctionPattern <|> do
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

        argoBind :: ArgoPatternExpression Value -> ArgoExpression r -> ArgoExpression (Value -> Maybe r);
        argoBind pat exp = fmap (\(Compose (Compose vmir)) v -> fmap runIdentity (vmir v))
         (toSimpleValueExpression (monoPatternBind (monoWitMap SymbolReference pat) exp));

        readFunction :: Parser (ArgoExpression (Value -> Value));
        readFunction = do
        {
            readCharAndWS '{';
            fields <- readCommaSeparated readField;
            readCharAndWS '}';
            return (assembleFunction fields);
        } where
        {
            readField :: Parser (ArgoPatternExpression Value,ArgoExpression Value);
            readField = do
            {
                pat <- option (patternMatch (isValue ())) (try (do
                {
                    pat <- readPattern;
                    readCharAndWS ':';
                    return pat;
                }));
                result <- readExpression;
                return (pat,result);
            };

            assembleFunction :: [(ArgoPatternExpression Value,ArgoExpression Value)] -> ArgoExpression (Value -> Value);
            assembleFunction [] = pure (\_ -> toValue ());
            assembleFunction ((pat,exp):ps) = liftA2 (\vmv vv v -> case vmv v of
            {
                Just r -> r;
                Nothing -> vv v;
            }) (argoBind pat exp) (assembleFunction ps);
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

        argoStrictBind :: ArgoPatternExpression Value -> ArgoExpression r -> ArgoExpression (Value -> r);
        argoStrictBind patExpr valExpr = fmap (\vmr v -> fromMaybe (errorC "unmatched binding") (vmr v)) (argoBind patExpr valExpr);

        readBinding :: Parser (ArgoExpression a) -> Parser (ArgoExpression a);
        readBinding readValExpr = do
        {
            (patExpr,argExprs) <- try (do
            {
                patExpr <- readPattern;
                argExprs <- many readPattern;
                readCharAndWS '=';
                return (patExpr,argExprs);
            });
            bodyExpr <- readExpression;
            readCharAndWS ',';
            valExpr <- readValExpr;
            return ((argoStrictBind patExpr valExpr) <*> (abstractExprs argExprs bodyExpr));
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
        } <|>
        readBinding readActionRest <|> do
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
            do
            {
                name <- readIdentifier;
                case name of
                {
                    "this" -> return ThisReference;
                    _ -> mzero;
                };
            } <|> do
            {
                libname <- readQuotedString;
                return (LibReference libname);
            }
        };

        readTerm :: Parser (ArgoExpression Value);
        readTerm = do
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
        fmap (fmap toValue) readFunction <|>
        fmap monoValueSymbol readDollarReference <|>
        fmap (fmap toValue) readFunction;

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
        readExpression = readBinding readExpression <|> readExpressionNoLet;

        readExpressionToEnd :: Parser (ArgoExpression Value);
        readExpressionToEnd = do
        {
            readWS;
            exp <- readExpression;
            optional (readCharAndWS ',');
            eof;
            return exp;
        };
    };
}
