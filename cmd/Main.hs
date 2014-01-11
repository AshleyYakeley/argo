module Main where
{
    import Prelude hiding (getContents,readFile,interact,putStr,putStrLn,getLine);
    import Data.Argo;
    import Control.Exception;
    import System.Environment;
    import System.IO.UTF8 hiding (interact);
    import System.IO (hSetBuffering,BufferMode(..),stdout);
    import System.IO.Error;
    import System.Posix.Terminal;
    import System.Posix.IO;

    data Mode = FileMode FilePath | ExpressionMode String | InteractiveMode | StdInMode | InteractiveOrStdInMode;

    interpretArgs :: [String] -> ([FilePath],Mode,[String]);
    interpretArgs ("-I":dir:rest) = case interpretArgs rest of
    {
        (dirs,mode,appArgs) -> (dir:dirs,mode,appArgs);
    };
    interpretArgs ("-e":expr:rest) = ([],ExpressionMode expr,rest);
    interpretArgs ("-i":rest) = ([],InteractiveMode,rest);
    interpretArgs ("-":rest) = ([],StdInMode,rest);
    interpretArgs (s:rest) = ([],FileMode s,rest);
    interpretArgs [] = ([],InteractiveOrStdInMode,[]);

    runExprNoArgs :: Value -> IO ();
    runExprNoArgs (ActionValue action) = do
    {
        _result <- action;
        return ();
    };
    runExprNoArgs val = putStrLn (show val);

    interact :: [FilePath] -> IO ();
    interact dirs = do
    {
        hSetBuffering stdout NoBuffering;
        interactLoop;
    } where
    {
        interactLoop = do
        {
            putStr "argo> ";
            s <- getLine;
            catch (do
            {
                r <- let {?context = "input"} in evaluateWithDirs dirs s;
                runExprNoArgs r;
            }) (\(se :: SomeException) -> case fromException se of
            {
                Just e -> putStrLn (ioeGetErrorString e);
                Nothing -> putStrLn (show se);
            });
            interactLoop;
        };
    };

    main :: IO ();
    main = do
    {
        args <- getArgs;
        let
        {
            (dirs,mode,appArgs) = interpretArgs args;
            runIt context getter = let {?context = context} in do
            {
                s <- getter;
                r <- evaluateWithDirs dirs s;
                runExprNoArgs (case r of
                {
                    FunctionValue fn -> fn (toValue appArgs);
                    val -> val;
                });
            };
        };
        case mode of
        {
            InteractiveOrStdInMode -> do
            {
                istty <- queryTerminal stdInput;
                if istty
                 then interact dirs
                 else runIt "stdin" getContents;
            };
            StdInMode -> runIt "stdin" getContents;
            InteractiveMode -> interact dirs;
            FileMode filePath -> runIt filePath (readFile filePath);
            ExpressionMode expr -> runIt "expression" (return expr);
        };
    };
}
