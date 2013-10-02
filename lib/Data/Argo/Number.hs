module Data.Argo.Number where
{
    import Import hiding (many,(<|>));
    import Text.Parsec;
    import Text.Parsec.String;
    import Text.Parsec.Pos(updatePosChar);

    satisfyMaybe :: (Stream s m Char) => (Char -> Maybe a) -> ParsecT s u m a;
    satisfyMaybe = tokenPrim (\c -> show [c]) (\pos c _cs -> updatePosChar pos c);

    decimalDigit :: Char -> Maybe Int;
    decimalDigit c | (c >= '0') && (c <= '9') = Just ((fromEnum c) - (fromEnum '0'));
    decimalDigit _ = Nothing;

    hexDigit :: Char -> Maybe Int;
    hexDigit c | (c >= '0') && (c <= '9') = Just ((fromEnum c) - (fromEnum '0'));
    hexDigit c | (c >= 'A') && (c <= 'F') = Just (((fromEnum c) - (fromEnum 'A')) + 10);
    hexDigit c | (c >= 'a') && (c <= 'f') = Just (((fromEnum c) - (fromEnum 'a')) + 10);
    hexDigit _ = Nothing;

    type Number = Rational;

    showNumber :: Number -> String;
    showNumber r = let
    {
        n = numerator r;
        d = denominator r;
    } in case d of
    {
        1 -> show n;
        _ -> (show n) ++ "/" ++ (show d);
    };

    assembleDigits :: [Integer] -> Integer;
    assembleDigits = assembleDigits' 0 where
    {
        assembleDigits' :: Integer -> [Integer] -> Integer;
        assembleDigits' r [] = r;
        assembleDigits' r (i:ii) = assembleDigits' (r * 10 + i) ii;
    };

    readDigits :: Parser Integer;
    readDigits = do
    {
        digits <- many1 readDigit;
        return (assembleDigits digits);
    };

    readDigit :: Parser Integer;
    readDigit = satisfyMaybe ((fmap toInteger) . decimalDigit);

    readPNumber :: Parser Number;
    readPNumber = readSigned readUnsignedNumber where
    {
        readUnsignedNumber :: Parser Number;
        readUnsignedNumber = do
        {
            n <- readDigits;
            (readFractionRest n) <|> (readDecimalRest n);
        };

        readFractionRest :: Integer -> Parser Rational;
        readFractionRest n = do
        {
            _ <- char '/';
            d <- readDigits;
            return (n % d);
        };

        readSigned :: (Num a) => Parser a -> Parser a;
        readSigned reader = do
        {
            _ <- char '-';
            n <- reader;
            return (negate n);
        } <|> reader;

        readInteger :: Parser Integer;
        readInteger = readSigned readDigits;

        readExponent :: Parser Rational;
        readExponent = do
        {
            _ <- satisfy (\c -> c == 'E' || c == 'e');
            i <- readInteger;
            return (10 ^^ i);
        };

        basis :: [Integer] -> Integer;
        basis [] = 1;
        basis (_:aa) = 10 * (basis aa);

        repeatingDigits :: [Integer] -> Rational;
        repeatingDigits [] = 0;
        repeatingDigits dd = (assembleDigits dd) % (basis dd - 1);

        readDecimalRest :: Integer -> Parser Rational;
        readDecimalRest whole = do
        {
            mdecimal <- optionMaybe (do
            {
                _ <- char '.';
                fdigits <- many readDigit;
                mrepeating <- optionMaybe (do
                {
                    _ <- char '_';
                    rdigits <- many readDigit;
                    return (repeatingDigits rdigits);
                });
                let
                {
                    fixedpartShifted = fromInteger (assembleDigits fdigits);
                    decimalShifted = case mrepeating of
                    {
                        Just repeating -> fixedpartShifted + repeating;
                        Nothing -> fixedpartShifted;
                    };
                    decimal = decimalShifted / (fromInteger (basis fdigits));
                };
                return decimal;
            });
            mexp <- optionMaybe readExponent;
            let
            {
                rwhole = fromInteger whole;
                mantissa = case mdecimal of
                {
                    Just decimal -> rwhole + decimal;
                    Nothing -> rwhole;
                };
                result = case mexp of
                {
                    Just exp -> mantissa * exp;
                    Nothing -> mantissa;
                };
            };
            return result;
        };

    };
}
