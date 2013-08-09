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

    evalTest :: String -> Maybe Value -> Test;
    evalTest s mv = pureTest s (diff (show mv) (show (parse s)));

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

        -- null type
        evalTest "null" (Just NullValue),
        
        -- boolean type
        evalTest "false" (Just (BoolValue False)),
        evalTest "true" (Just (BoolValue True)),
        
        -- number type
        evalTest "3" (Just (NumberValue 3)),
        evalTest "-4" (Just (NumberValue (-4))),
        
        --string type
        evalTest "\"\"" (Just (StringValue "")),
        evalTest "\"hx\"" (Just (StringValue "hx")),
        
        -- array type
        evalTest "[]" (Just (ArrayValue [])),
        evalTest "[37]" (Just (ArrayValue [NumberValue 37])),
        evalTest "[null,true,47]" (Just (ArrayValue [NullValue,BoolValue True,NumberValue 37])),
        evalTest "[;[]]" (Just (ArrayValue [])),
        evalTest "[;[\"abGc\"]]" (Just (ArrayValue [StringValue "abGc"])),
        evalTest "[51;[]]" (Just (ArrayValue [NumberValue 51])),
        
        -- function type
        evalTest "{a:a}" (Just (FunctionValue id)),
        evalTest "{ab:ab}" (Just (FunctionValue id)),
        evalTest "{ab:a}" Nothing,

        -- function application
        evalTest "{a:a} 45" (Just (NumberValue 45)),
        evalTest "{a:12} 45" (Just (NumberValue 12)),
        evalTest "{a:{b:a}} 27 31" (Just (NumberValue 27)),
        evalTest "{a:{b:b}} 27 31" (Just (NumberValue 31)),

        -- pattern matching
        evalTest "{_:true} 37" (Just (BoolValue True)),
        evalTest "{null:true,_:false} null" (Just (BoolValue True)),
        evalTest "{null:true,_:false} \"h\"" (Just (BoolValue False)),
        evalTest "{{1:2}:true,_:false} {1:2}" (Just (BoolValue True)),
        evalTest "{{1:2}:true,_:false} {1:3}" (Just (BoolValue False)),
        evalTest "{{1:2}:true,_:false} {1:null}" (Just (BoolValue False))
    ];

    main :: IO ();
    main = defaultMain tests;
}
