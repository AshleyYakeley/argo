module Data.Argo.StdLib(stdlib) where
{
    import Import;
    import Data.Argo.Read;
    import Data.Argo.Value;

    fixV :: (Value -> Value) -> Value;
    fixV f = f (fixV f);

    lib :: String -> Value;
    lib "+" = toValue ((+) :: Rational -> Rational -> Rational);
    lib "-" = toValue ((-) :: Rational -> Rational -> Rational);
    lib ">>=" = toValue ((>>=) :: IO Value -> (Value -> IO Value) -> IO Value);
    lib "return" = toValue (return :: Value -> IO Value);
    lib "fix" = toValue fixV;
    lib _ = toValue ();

    stdlib :: Value;
    stdlib = toValue lib;
}
