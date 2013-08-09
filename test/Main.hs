module Main where
{
    import Data.Functor.Identity;
    import Test.Framework;
    import TestUtil;
    import Data.Argo;

    parse :: String -> Maybe Value;
    parse s = do
    {
        expr <- readText s;
        case evalExpression expr of
        {
            Left _syms -> Nothing;
            Right (Identity (r :: Value)) -> return r;
        };
    };

    showTest :: Value -> String -> Test;
    showTest v s = pureTest s (diff s (show v));

    argoTest :: String -> Maybe Value -> Test;
    argoTest s mv = pureTest s (diff (show mv) (show (parse s)));

    tests :: [Test];
    tests = 
    [
        showTest NullValue "null",
        showTest (BoolValue False) "false",
        showTest (BoolValue True) "true",
        showTest (NumberValue 3) "3 % 1",
        showTest (StringValue "") "\"\"",
        showTest (StringValue "hello") "\"hello\"",
        showTest (ArrayValue []) "[]",
        showTest (ArrayValue [NullValue]) "[null]",
        showTest (ArrayValue [StringValue "ab",StringValue "cd"]) "[\"ab\",\"cd\"]",
        showTest (FunctionValue id) "{...}",

        -- basic types
        argoTest "null" (Just NullValue),
        argoTest "false" (Just (BoolValue False)),
        argoTest "true" (Just (BoolValue True)),
        argoTest "3" (Just (NumberValue 3)),
        argoTest "{a:a}" (Just (FunctionValue id)),
        argoTest "\"\"" (Just (StringValue "")),
        argoTest "\"hx\"" (Just (StringValue "hx")),

        -- function application
        argoTest "{a:a} 45" (Just (NumberValue 45)),
        argoTest "{a:12} 45" (Just (NumberValue 12))
    ];

    main :: IO ();
    main = defaultMain tests;
}
