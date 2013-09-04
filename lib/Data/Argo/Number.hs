module Data.Argo.Number where
{
    import Import;
    
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
    
    readPNumber :: ReadP Number;
    readPNumber = readSigned readUnsignedNumber where
    {
        readUnsignedNumber :: ReadP Number;
        readUnsignedNumber = readFraction <++ readDecimal;
    
        readFraction :: ReadP Rational;
        readFraction = do
        {
            n <- readDigits;
            _ <- char '/';
            d <- readDigits;
            return (n % d);
        };
        
        readSigned :: (Num a) => ReadP a -> ReadP a;
        readSigned reader = do
        {
            _ <- char '-';
            n <- reader;
            return (negate n);
        } <++ reader;
        
        readInteger :: ReadP Integer;
        readInteger = readSigned readDigits;
        
        readExponent :: ReadP Rational;
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
        
        readDecimal :: ReadP Rational;
        readDecimal = do
        {
            whole <- readDigits;
            mdecimal <- optionalMax (do
            {
                _ <- char '.';
                fdigits <- manyMax readDigit;
                mrepeating <- optionalMax (do
                {
                    _ <- char '_';
                    rdigits <- manyMax readDigit;
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
            mexp <- optionalMax readExponent;
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
    
    
        assembleDigits = assembleDigits' 0 where
        {
            assembleDigits' :: Integer -> [Integer] -> Integer;
            assembleDigits' r [] = r;
            assembleDigits' r (i:ii) = assembleDigits' (r * 10 + i) ii;
        };
    
        readDigits :: ReadP Integer;
        readDigits = do
        {
            digits <- many1Max readDigit;
            return (assembleDigits digits);
        };
        
        readDigit :: ReadP Integer;
        readDigit = do
        {
            c <- get;
            case decimalDigit c of
            {
                Just i -> return (toInteger i);
                Nothing -> mzero;
            };
        };
    };
}
