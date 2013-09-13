module Data.Argo.StdLib(stdLib,stdLibValue) where
{
    import Import;
    import qualified Data.ByteString as B;
    import Data.Argo.Number;
    import Data.Argo.Value;
    import Data.Argo.StdLib.File;
    import Data.Argo.StdLib.Process;
    import Codec.Binary.UTF8.String;
    import System.IO.UTF8;
    import System.IO hiding (hPutStr,utf8);

    fixV :: (Value -> Value) -> Value;
    fixV f = f (fixV f);

    defaultV :: Value -> Value -> Value;
    defaultV NullValue v = v;
    defaultV v _ = v;

    defaultFunction :: (Value -> Value) -> (Value -> Value) -> (Value -> Value);
    defaultFunction f1 f2 x = defaultV (f1 x) (f2 x);

    takeV :: Int -> Either [Value] String -> Either [Value] String;
    takeV i (Left arr) = Left (take i arr);
    takeV i (Right s) = Right (take i s);

    dropV :: Int -> Either [Value] String -> Either [Value] String;
    dropV i (Left arr) = Left (drop i arr);
    dropV i (Right s) = Right (drop i s);

    bytes :: String -> [Word8];
    bytes = digitStreamToBytes . toDigitStream where
    {
        toDigitStream :: String -> [Word8];
        toDigitStream [] = [];
        toDigitStream (c:cc) | Just i <- hexDigit c = (toEnum i):(toDigitStream cc);
        toDigitStream (c:cc) | isSpace c = toDigitStream cc;
        toDigitStream (c:_) = error ("unrecognised char in bytes: " ++ [c]);
        
        digitStreamToBytes :: [Word8] -> [Word8];
        digitStreamToBytes [] = [];
        digitStreamToBytes [_] = error "extra hex digit";
        digitStreamToBytes (a:b:rest) = ((a * 16) + b):(digitStreamToBytes rest);
    };
    
    utf8 :: Either String [Word8] -> Either [Word8] String;
    utf8 (Left s) = Left (encode s);
    utf8 (Right b) = Right (decode b);
    
    subst :: String -> String -> String -> (String -> String) -> String;
    subst pre post template dict = case searchList pre template of
    {
        Just (a,aa) -> case searchList post aa of
        {
            Just (b,c) -> a ++ (dict b) ++ (subst pre post c dict);
            Nothing -> template;
        };
        Nothing -> template;
    }
    where
    {
        -- searchList s (a ++ s ++ b) = Just (a,b) where s is first occurrence
        searchList :: (Eq a) => [a] -> [a] -> Maybe ([a],[a]);
        searchList [] text = Just ([],text);
        searchList _ [] = Nothing;
        searchList (s:ss) (t:tt) | s == t = searchList ss tt;
        searchList sc (t:tt) = do
        {
            (a,b) <- searchList sc tt;
            return (t:a,b);
        };
    };

    eq :: Value -> Value -> Bool;
    eq NullValue NullValue = True;
    eq (BoolValue a) (BoolValue b) = a == b;
    eq (NumberValue a) (NumberValue b) = a == b;
    eq (StringValue a) (StringValue b) = a == b;
    eq (ByteArrayValue a) (ByteArrayValue b) = a == b;
    eq (ArrayValue a) (ArrayValue b) = eqArray a b;
    eq _ _ = False;
    
    eqArray :: [Value] -> [Value] -> Bool;
    eqArray [] [] = True;
    eqArray (a:aa) (b:bb) | eq a b = eqArray aa bb;
    eqArray _ _ = False;

    mapV :: (Value -> Value) -> Either (Value -> Value) (Either (IO Value) [Value]) -> Either (Value -> Value) (Either (IO Value) [Value]);
    mapV f (Left x) = Left (fmap f x);
    mapV f (Right (Left x)) = Right (Left (fmap f x));
    mapV f (Right (Right x)) = Right (Right (fmap f x));

    forV :: [Value] -> (Value -> IO Value) -> IO [Value];
    forV = for;

    stdLib :: (?context :: String) => String -> Value;
    stdLib "error" = toValue (errorC :: String -> Value);
    
    stdLib "default" = toValue defaultV;
    stdLib "function-default" = toValue defaultFunction;
    stdLib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    stdLib "-" = toValue ((-) :: Rational -> Rational -> Rational);
    stdLib "=" = toValue eq;

    stdLib "show" = toValue (show :: Value -> String);

    stdLib "take" = toValue takeV;
    stdLib "drop" = toValue dropV;
    stdLib "string-subst" = toValue subst;
    stdLib "string-concat" = toValue (concat :: [String] -> String);
    stdLib "bytes-concat" = toValue B.concat;
    stdLib "array-concat" = toValue (concat :: [[Value]] -> [Value]);

    stdLib "subst" = toValue subst;

    stdLib "bytes" = toValue bytes;
    stdLib "utf-8" = toValue utf8;

    stdLib "stdout" = toValue (hPutStr stdout);
    stdLib "stderr" = toValue (hPutStr stderr);

    stdLib "map" = toValue mapV;
    stdLib "for" = toValue forV;
    stdLib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    stdLib "return" = toValue (return :: Value -> IO Value);
    stdLib "fail" = toValue (fail :: String -> IO Value);
    stdLib "fix" = toValue fixV;
    stdLib "action-fix" = toValue (fixIO :: (Value -> IO Value) -> IO Value);
    stdLib s | Just r <- fileFunctions s = r;
    stdLib s | Just r <- processFunctions s = r;
    stdLib s = errorC ("$\"std\" " ++ (show s) ++ ": not found");

    stdLibValue :: (?context :: String) => Value;
    stdLibValue = toValue stdLib;
}
