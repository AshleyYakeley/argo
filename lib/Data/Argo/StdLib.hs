module Data.Argo.StdLib(stdLib,stdLibValue) where
{
    import Codec.Binary.UTF8.String;
    import System.IO hiding (hPutStr,utf8);
    import qualified Data.ByteString as B;

    import Import;
    import Data.Argo.Number;
    import Data.Argo.Object;
    import Data.Argo.Value;
    import Data.Argo.Read;
    import Data.Argo.StdLib.File;
    import Data.Argo.StdLib.Process;
    import Data.Argo.TextFile;

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
        searchList s t | Just text <- matchStart s t = Just ([],text);
        searchList _ [] = Nothing;
        searchList sc (t:tt) = do
        {
            (a,b) <- searchList sc tt;
            return (t:a,b);
        };

        matchStart :: (Eq a) => [a] -> [a] -> Maybe [a];
        matchStart [] t = Just t;
        matchStart (s:ss) (t:tt) | s == t = matchStart ss tt;
        matchStart _ _ = Nothing;
    };

    argoRead :: String -> (String -> Maybe Value) -> String -> Maybe Value;
    argoRead context libs text = let {?context = context} in
        evaluateWithLibs (\s -> return (fmap Right (libs s))) text;

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

    stdLib :: (?context :: String) => Object Value;
    stdLib = MkObject
    ([
        ("error",toValue (errorC :: String -> Value)),

        ("default",toValue defaultV),
        ("function-default",toValue defaultFunction),
        ("+",toValue ((+) :: Rational -> Rational -> Rational)),
        ("-",toValue ((-) :: Rational -> Rational -> Rational)),
        ("=",toValue eq),

        ("show",toValue (show :: Value -> String)),

        ("take",toValue takeV),
        ("drop",toValue dropV),
        ("string-subst",toValue subst),
        ("string-concat",toValue (concat :: [String] -> String)),
        ("bytes-concat",toValue B.concat),
        ("array-concat",toValue (concat :: [[Value]] -> [Value])),

        ("subst",toValue subst),

        ("bytes",toValue bytes),
        ("utf-8",toValue utf8),

        ("stdout",toValue (hPutStr stdout)),
        ("stderr",toValue (hPutStr stderr)),

        ("map",toValue mapV),
        ("for",toValue forV),
        (">>=",toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value)),
        ("return",toValue (return :: Value -> IO Value)),
        ("fail",toValue (fail :: String -> IO Value)),
        ("fix",toValue fixV),
        ("action-fix",toValue (fixIO :: (Value -> IO Value) -> IO Value)),

        ("argo-read",toValue argoRead)
    ] ++
    fileFunctions ++
    processFunctions);

    stdLibValue :: (?context :: String) => Value;
    stdLibValue = toValue stdLib;
}
