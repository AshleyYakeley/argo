module Main where
{
    import Data.Functor.Identity;
    import Test.Framework;
    import TestUtil;
    import Data.Argo;

    parse :: String -> FailM Value;
    parse s = do
    {
        expr <- readText s;
        case evalExpression expr of
        {
            Left _syms -> fail "not closed";
            Right (Identity (r :: Value)) -> return r;
        };
    };

    showTest :: Value -> String -> Test;
    showTest v s = pureTest s (diff s (show v));

    evalTest :: String -> FailM Value -> Test;
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
        evalTest "null" (return NullValue),
        
        -- boolean type
        evalTest "false" (return (BoolValue False)),
        evalTest "true" (return (BoolValue True)),
        
        -- number type
        evalTest "0" (return (NumberValue 0)),
        evalTest "3" (return (NumberValue 3)),
        evalTest "-4" (return (NumberValue (-4))),
        
        --string type
        evalTest "\"\"" (return (StringValue "")),
        evalTest "\"hx\"" (return (StringValue "hx")),
        
        -- array type
        evalTest "[]" (return (ArrayValue [])),
        evalTest "[37]" (return (ArrayValue [NumberValue 37])),
        evalTest "[null,true,47]" (return (ArrayValue [NullValue,BoolValue True,NumberValue 47])),
        evalTest "[;[]]" (return (ArrayValue [])),
        evalTest "[;[\"abGc\"]]" (return (ArrayValue [StringValue "abGc"])),
        evalTest "[51;[]]" (return (ArrayValue [NumberValue 51])),
        evalTest "[false,51;[null,35]]" (return (ArrayValue [BoolValue False,NumberValue 51,NullValue,NumberValue 35])),
        
        -- function type
        evalTest "{}" (return (FunctionValue id)),
        evalTest "{_:null}" (return (FunctionValue id)),
        evalTest "{_:true}" (return (FunctionValue id)),
        evalTest "{_:false}" (return (FunctionValue id)),
        evalTest "{_:false,0:0}" (return (FunctionValue id)),
        evalTest "{a:a}" (return (FunctionValue id)),
        evalTest "{a:a,0:0}" (return (FunctionValue id)),
        evalTest "{_:0,0:0}" (return (FunctionValue id)),
        evalTest "{0:0,_:false}" (return (FunctionValue id)),
        evalTest "{_:true,_:false}" (return (FunctionValue id)),
        evalTest "{a:a}" (return (FunctionValue id)),
        evalTest "{ab:ab}" (return (FunctionValue id)),
        evalTest "{ab:a}" (fail "not closed"),
        evalTest "{1:1,2:2}" (return (FunctionValue id)),

        -- function application
--        evalTest "null 45" (fail "non-function application"),
--        evalTest "null null" (fail "non-function application"),
--        evalTest "\"a\" 25" (fail "non-function application"),
        evalTest "{a:a} 45" (return (NumberValue 45)),
        evalTest "{a:12} 45" (return (NumberValue 12)),
        evalTest "{a:{b:a}} 27 31" (return (NumberValue 27)),
        evalTest "{a:{b:b}} 27 31" (return (NumberValue 31)),
        
        -- comments
        evalTest "#\n34" (return (NumberValue 34)),
        evalTest "#\n{#\na#\n:#\na#\n}#\n89#\n" (return (NumberValue 89)),
        evalTest " #\n { #\n a #\n : #\n a #\n } #\n 89 #\n" (return (NumberValue 89))       
    ] ++ concat
        -- pattern matching wildcard & type
    [
        patternTests "_" ["null","false","0","\"\"","[]","{}"] [],
        patternTests "_null" ["null"] ["false","0","\"\"","[]","{}"],
        patternTests "null" ["null"] ["false","0","\"\"","[]","{}"],
        patternTests "_boolean" ["true","false"] ["null","0","\"\"","[]","{}"],
        patternTests "true" ["true"] ["null","false","0","\"\"","[]","{}"],
        patternTests "false" ["false"] ["null","true","0","\"\"","[]","{}"],
        patternTests "_number" ["0"] ["null","false","\"\"","[]","{}"],
        patternTests "_string" ["\"\""] ["null","false","0","[]","{}"],
        patternTests "_array" ["[]"] ["null","false","0","\"\"","{}"],
        patternTests "[]" ["[]"] ["null","false","0","\"\"","{}","[null]","[[]]"],
        patternTests "[;_]" ["[]","[null]","[[]]"] ["null","false","0","\"\"","{}"],
        patternTests "_function" ["{}"] ["null","false","0","\"\"","[]"],
        patternTests "{}" ["{}","{1:2}"] ["null","false","0","\"\"","[]"],
        patternTests "{1:2}" ["{1:2}"] ["null","false","0","\"\"","[]","{}","{1:3}","{1:null}","{2:2}"],
        patternTests "{1:2,1:4}" ["{1:2}"] ["null","false","0","\"\"","[]","{}","{1:3}","{1:4}","{1:null}","{2:2}"],
        patternTests "{1:_number}" ["{1:2}","{1:3}","{1:4}"] ["null","false","0","\"\"","[]","{}","{1:null}","{2:2}"],
        patternTests "_@_" ["null","false","0","\"\"","[]","{}"] [],
        patternTests "a@_number" ["0"] ["null","false","\"\"","[]","{}"]
    ] ++
    [
        evalTest "{a@b:[a,b]} 2" (return (ArrayValue [NumberValue 2,NumberValue 2])),
        evalTest "{a@[b]:[a,b]} [1]" (return (ArrayValue [ArrayValue [NumberValue 2],NumberValue 2]))
    ] where
    {
        patternTest :: String -> String -> Bool -> Test;
        patternTest pat val result = evalTest ("{" ++ pat ++ ":true,_:false} " ++ val) (return (BoolValue result));
        
        patternTests :: String -> [String] -> [String] -> [Test];
        patternTests pat goods bads = (fmap (\val -> patternTest pat val True) goods) ++ (fmap (\val -> patternTest pat val False) bads);
    };

    main :: IO ();
    main = defaultMain tests;
}
