module Main where
{
    import Data.Ratio;
    import qualified Data.ByteString as B;
    import Test.Framework;
    import TestUtil;
    import Data.Argo;

    showTest :: Value -> String -> Test;
    showTest v s = pureTest s (diff s (show v));

    libFinder :: [(String,String)] -> String -> FailM (Maybe String);
    libFinder ((name,text):_) name' | name == name' = return (Just text);
    libFinder (_:rest) name' = libFinder rest name';
    libFinder [] _ = return Nothing;

    evalTestWithLibs :: [(String,String)] -> String -> FailM Value -> Test;
    evalTestWithLibs libs s mv = let {?context = "test"} in
     pureTest s (diff (show mv) (show (evaluateWithStdLib (libFinder libs) s :: FailM Value)));

    evalTest :: String -> FailM Value -> Test;
    evalTest = evalTestWithLibs [];

    tests :: [Test];
    tests =
    [
        showTest NullValue "null",
        showTest (BoolValue False) "false",
        showTest (BoolValue True) "true",
        showTest (NumberValue 3) "3",
        showTest (NumberValue 3.7) "37/10",
        showTest (NumberValue (-3.7)) "-37/10",
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
        evalTest "2/3" (return (NumberValue (2%3))),
        evalTest "-18/6" (return (NumberValue (-3))),
        evalTest "-21/5" (return (NumberValue (-4.2))),
        evalTest "19.1" (return (NumberValue (19.1))),
        evalTest "-1.3" (return (NumberValue (-1.3))),
        evalTest "25" (return (NumberValue (25))),
        evalTest "25." (return (NumberValue (25))),
        evalTest "25.0" (return (NumberValue (25))),
        evalTest "25.00" (return (NumberValue (25))),
        evalTest "25.0_" (return (NumberValue (25))),
        evalTest "25.0_0" (return (NumberValue (25))),
        evalTest "25._0" (return (NumberValue (25))),
        evalTest "25._" (return (NumberValue (25))),
        evalTest "-5.1_6" (return (NumberValue (-31%6))),
        evalTest "3e6" (return (NumberValue (3000000))),
        evalTest "-1.25_142857E-4" (return (NumberValue (-876/7000000))),

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
        evalTest "{ab:a}" (fail "test: undefined: a"),
        evalTest "{1:1,2:2}" (return (FunctionValue id)),
        evalTest "{1}" (return (FunctionValue id)),
        evalTest "{1,}" (return (FunctionValue id)),
        evalTest "{null,}" (return (FunctionValue id)),
        evalTest "{null:null,}" (return (FunctionValue id)),
        evalTest "{{_}: null}" (return (FunctionValue id)),
        evalTest "{{a}: null}" (return (FunctionValue id)),
        evalTest "{{{\"a\":a}}: null}" (return (FunctionValue id)),
        evalTest "{{{\"a\":a,\"b\":b}}: null}" (return (FunctionValue id)),

        -- function application
--        evalTest "null 45" (fail "non-function application"),
--        evalTest "null null" (fail "non-function application"),
--        evalTest "\"a\" 25" (fail "non-function application"),
        evalTest "{a:a} 45" (return (NumberValue 45)),
        evalTest "{a:12} 45" (return (NumberValue 12)),
        evalTest "{a:{b:a}} 27 31" (return (NumberValue 27)),
        evalTest "{a:{b:b}} 27 31" (return (NumberValue 31)),
        evalTest "{37} 45" (return NullValue),
        evalTest "{37} null" (return (NumberValue 37)),
        evalTest "{37,true:54} null" (return (NumberValue 37)),
        evalTest "{37,true:54} true" (return (NumberValue 54)),
        evalTest "{37,true:54} false" (return NullValue),
        evalTest "{37,true:54,} null" (return (NumberValue 37)),
        evalTest "{37,true:54,} true" (return (NumberValue 54)),
        evalTest "{37,true:54,} false" (return NullValue),

        -- comments
        evalTest "#\n34" (return (NumberValue 34)),
        evalTest "#\n{#\na#\n:#\na#\n}#\n89#\n" (return (NumberValue 89)),
        evalTest " #\n { #\n a #\n : #\n a #\n } #\n 89 #\n" (return (NumberValue 89)),

        -- parentheses
        evalTest "(1)" (return (NumberValue 1)),
        evalTest "({a:a} 1)" (return (NumberValue 1)),
        evalTest "{a:a} (1)" (return (NumberValue 1)),
        evalTest "{a:(a)} 1" (return (NumberValue 1)),

        -- let-binding
        evalTest "a=1,a" (return (NumberValue 1)),
        evalTest "(a=1,a)" (return (NumberValue 1)),
        evalTest "a=1,(a)" (return (NumberValue 1)),
        evalTest "a=(1),a" (return (NumberValue 1)),
        evalTest "[a=1,a]" (return (ArrayValue [NumberValue 1])),
        evalTest "b=(a=1,a),b" (return (NumberValue 1)),
        evalTest "b=a=1,a,b" (return (NumberValue 1)),
        evalTest "a=1,b=2,[b,a]" (return (ArrayValue [NumberValue 2,NumberValue 1]))
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
        patternTests "{1:2}" ["{1:2}","{1:2,1:4}"] ["null","false","0","\"\"","[]","{}","{1:3}","{1:null}","{2:2}"],
        patternTests "{1:2,3:4}" ["{1:2,3:4}"] ["null","false","0","\"\"","[]","{}","{1:3}","{1:4}","{1:null}","{2:2}"],
        patternTests "{1:_number}" ["{1:2}","{1:3}","{1:4}"] ["null","false","0","\"\"","[]","{}","{1:null}","{2:2}"],
        patternTests "{_number}" ["{null:2}","{null:3}","{null:4}","{2}","{3}","{4}"] ["{1:2}","{1:3}","{1:4}","null","false","0","\"\"","[]","{}","{1:null}","{2:2}"],
        patternTests "{1:_number,}" ["{1:2}","{1:3}","{1:4}"] ["null","false","0","\"\"","[]","{}","{1:null}","{2:2}"],
        patternTests "{_number,}" ["{null:2}","{null:3}","{null:4}","{2}","{3}","{4}"] ["{1:2}","{1:3}","{1:4}","null","false","0","\"\"","[]","{}","{1:null}","{2:2}"],
        patternTests "_@_" ["null","false","0","\"\"","[]","{}"] [],
        patternTests "a@_number" ["0"] ["null","false","\"\"","[]","{}"]
    ] ++ concat
    [
        regexpTests "" [""] ["a"," ","abc"],
        regexpTests "\"abc\"" ["abc"] ["a"," ","abcd","ab"],
        regexpTests "\"abc\",\"def\"" ["abcdef"] ["a"," ","abcd","ab","abc","def","d","abcdeff"]
    ] ++
    [
        evalTest "{a@b:[a,b]} 2" (return (ArrayValue [NumberValue 2,NumberValue 2])),
        evalTest "{a@[b]:[a,b]} [1]" (return (ArrayValue [ArrayValue [NumberValue 1],NumberValue 1]))
    ] ++
    [
        evalTest "$\"std\" \"+\" 3 4" (return (NumberValue 7)),
        evalTest "$\"std\" \"fix\"" (return (FunctionValue id)),
        evalTest "$\"std\" \"fix\" {fib:{0:0,1:1,n:$\"std\" \"+\" (fib ($\"std\" \"-\" n 1)) (fib ($\"std\" \"-\" n 2))}} 11" (return (NumberValue 89)),

        evalTest "$\"std\" \"take\" 4 \"abcdefg\"" (return (StringValue "abcd")),
        evalTest "$\"std\" \"drop\" 2 \"abcdefg\"" (return (StringValue "cdefg")),
        evalTest "$\"std\" \"take\" 3 [1,2,3,4,5,6,7]" (return (ArrayValue [NumberValue 1,NumberValue 2,NumberValue 3])),
        evalTest "$\"std\" \"drop\" 3 [1,2,3,4,5,6,7]" (return (ArrayValue [NumberValue 4,NumberValue 5,NumberValue 6,NumberValue 7])),

        evalTest "$\"std\" \"bytes\" \"AF 30 2b05\"" (return (ByteArrayValue (B.pack [0xAF,0x30,0x2B,0x05]))),

        evalTest "$\"std\" \"=\" null null" (return (BoolValue True)),
        evalTest "$\"std\" \"=\" 1 1" (return (BoolValue True)),
        evalTest "$\"std\" \"=\" 1 2" (return (BoolValue False)),
        evalTest "$\"std\" \"=\" {} {}" (return (BoolValue False)),
        evalTest "$\"std\" \"=\" [] []" (return (BoolValue True)),
        evalTest "$\"std\" \"=\" [] {}" (return (BoolValue False)),
        evalTest "$\"std\" \"=\" 2 {}" (return (BoolValue False)),
        evalTest "$\"std\" \"=\" [{}] [{}]" (return (BoolValue False)),
        evalTest "$\"std\" \"=\" [[]] [[]]" (return (BoolValue True)),
        evalTest "$\"std\" \"=\" [4] [4]" (return (BoolValue True)),

        -- std functions
        evalTest "$\"std\" \"subst\" \"[[\" \"]]\" \"[[a]]\" {\"a\":\"b\"}" (return (StringValue "b")),
        evalTest "$\"std\" \"subst\" \"[[\" \"]]\" \"[ [a]]\" {\"a\":\"b\"}" (return (StringValue "[ [a]]")),

        -- lib loading
        evalTestWithLibs [("a","[2]")] "[1;$\"a\"]" (return (ArrayValue [NumberValue 1,NumberValue 2])),
        evalTestWithLibs [("a","[2;$\"b\"]"),("b","[3]")] "[1;$\"a\"]" (return (ArrayValue [NumberValue 1,NumberValue 2,NumberValue 3])),
        evalTestWithLibs [("b","[3]"),("a","[2;$\"b\"]")] "[1;$\"a\"]" (return (ArrayValue [NumberValue 1,NumberValue 2,NumberValue 3])),
--        evalTestWithLibs [("a","null null")] "[1;$\"a\"]" (fail "test: $\"a\": null is not of type function"),
        evalTestWithLibs [("z","$\"std\" \"+\" 3 4")] "$\"z\"" (return (NumberValue 7)),
--        evalTestWithLibs [("z","$\"std\" \"+\" 3 null")] "$\"z\"" (fail "test: $\"z\": null is not of type number"),
--        evalTestWithLibs [("z","$\"std\" \"+\" null 3")] "$\"z\"" (fail "test: $\"z\": null is not of type number"),
        evalTestWithLibs [("a","$\"std\" \"+\" 3 4"),("b","$\"a\""),("c","$\"b\""),("d","$\"c\"")] "$\"d\"" (return (NumberValue 7)),

        -- recursive library reference
        evalTestWithLibs [("a","[4;$this]")] "$\"std\" \"take\" 2 $\"a\"" (return (ArrayValue [NumberValue 4,NumberValue 4])),
        evalTestWithLibs [("a","[4;$\"a\"]")] "$\"std\" \"take\" 2 $\"a\"" (return (ArrayValue [NumberValue 4,NumberValue 4])),
        evalTestWithLibs [("a","[5;$\"b\"]"),("b","[7;$\"a\"]")] "$\"std\" \"take\" 4 $\"a\""
            (return (ArrayValue [NumberValue 5,NumberValue 7,NumberValue 5,NumberValue 7]))
    ] where
    {
        patternTest :: String -> String -> Bool -> Test;
        patternTest pat val result = evalTest ("{" ++ pat ++ ":true,_:false} " ++ val) (return (BoolValue result));

        patternTests :: String -> [String] -> [String] -> [Test];
        patternTests pat goods bads = (fmap (\val -> patternTest pat val True) goods) ++ (fmap (\val -> patternTest pat val False) bads);

        regexpTests  :: String -> [String] -> [String] -> [Test];
        regexpTests pat goods bads = patternTests ("/" ++ pat ++"/") (fmap show goods) (["null","false","0","[]","{}"] ++ (fmap show bads));
    };

    main :: IO ();
    main = defaultMain tests;
}
