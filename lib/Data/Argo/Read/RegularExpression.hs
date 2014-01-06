module Data.Argo.Read.RegularExpression where
{
    import Import hiding (many,(<|>),optional);
    import Text.Parsec.String;
    import Text.Parsec;
    import Language.Expression.Mono;
    import Language.Expression.Regular;
    import Data.Argo.Number;
    import Data.Argo.Value;
    import Data.Argo.Read.Lexical;


    type ArgoRegularExpression = MonoRegularExpression String Value String;

    readRegularExpression :: (?context::String) => Parser ArgoRegularExpression;
    readRegularExpression = do
    {
        readCharAndWS '/';
        regex <- readRE;
        readCharAndWS '/';
        return regex;
    } where
    {
        readREIdentifier :: Parser String;
        readREIdentifier = do
        {
            first <- satisfy isAlpha;
            rest <- many (readEscapedChar <|> (satisfy goodREChar));
            readWS;
            return (first:rest);
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
    };
}
