{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module TestUtil
    (
    module TestUtil,
    module Test.Framework,
    ) where

{
    import Test.Framework;
    import Test.Framework.Providers.API;
    import Control.Applicative;
    import Control.Exception;
    import Data.Typeable;

    data Result = Pass | Fail String deriving Typeable;

    instance Show Result where
    {
        show Pass = "passed";
        show (Fail s) = "failed: " ++ s;
    };

    instance TestResultlike () Result where
    {
        testSucceeded Pass = True;
        testSucceeded (Fail _) = False;
    };

    instance Testlike () Result (IO Result) where
    {
        testTypeName _ = "Cases";
        runTest _ ior = do
        {
            r <- ior;
            return (Finished r,return ());
        };
    };

    ioTest :: String -> IO Result -> Test;
    ioTest = Test;

    pureTest :: String -> Result -> Test;
    pureTest name result = ioTest name (catch (evaluate result) (\(ex :: SomeException) -> return (Fail (show ex))));

    diff :: (Show a,Eq a) => a -> a -> Result;
    diff expected found | expected == found = Pass;
    diff expected found = Fail ("expected " ++ (show expected) ++ " but found " ++ (show found));

    data FailM a = Success a | Error String;
    
    instance Functor FailM where
    {
        fmap ab (Success a) = Success (ab a);
        fmap _ (Error s) = Error s;
    };
    
    instance Applicative FailM where
    {
        pure = Success;
        (Success ab) <*> (Success a) = Success (ab a);
        (Error err) <*> _ = Error err;
        (Success _) <*> (Error err) = Error err;
    };
    
    instance Monad FailM where
    {
        return = Success;
        (Success a) >>= f = f a;
        (Error err) >>= _ = Error err;
        fail = Error;
    };
    
    instance (Show a) => Show (FailM a) where
    {
        show (Success a) = "success: " ++ (show a);
        show (Error err) = "failure: " ++ err;
    };
}
