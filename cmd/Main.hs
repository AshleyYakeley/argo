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

    interpretArgs :: [String] -> ([FilePath],Maybe FilePath,[String]);
    interpretArgs ("-I":dir:rest) = case interpretArgs rest of
    {
        (dirs,mFilePath,appArgs) -> (dir:dirs,mFilePath,appArgs);
    };
    interpretArgs ("-":rest) = ([],Nothing,rest);
    interpretArgs (s:rest) = ([],Just s,rest);
    interpretArgs [] = ([],Nothing,[]);

    run :: String -> [FilePath] -> [String] -> IO String -> IO ();
    run filename dirs appArgs getter = do
    {
        s <- getter;
        r <- let {?context = filename} in evaluateWithDirs dirs s;
        _ :: Value <- r appArgs;
        return ();
    };

    interact :: [FilePath] -> IO ();
    interact dirs = do
    {
        putStr "argo> ";
        s <- getLine;
        catch (do
        {
            r <- let {?context = "input"} in evaluateWithDirs dirs s;
            case r of
            {
                ActionValue action -> do
                {
                    result <- action;
                    putStrLn (show result);
                };
                _ -> putStrLn (show r);
            };
        }) (\(e :: IOException) -> do
        {
            putStrLn (ioeGetErrorString e);
        });
        interact dirs;
    };

    main :: IO ();
    main = do
    {
        args <- getArgs;
        let
        {
            (dirs,mFilePath,appArgs) = interpretArgs args;
        };
        case mFilePath of
        {
            Nothing -> do
            {
                istty <- queryTerminal stdInput;
                if istty
                 then do
                 {
                    hSetBuffering stdout NoBuffering;
                    interact dirs;
                 }
                 else run "stdin" dirs appArgs getContents;
            };
            Just filePath -> run filePath dirs appArgs (readFile filePath);
        };
    };
}
