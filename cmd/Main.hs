module Main where
{
    import Prelude hiding (getContents, readFile);
    import Data.Argo;
    import System.Environment;
    import System.IO.UTF8;

    interpretArgs :: [String] -> ([FilePath],Maybe FilePath,[String]);
    interpretArgs ("-I":dir:rest) = case interpretArgs rest of
    {
        (dirs,mFilePath,appArgs) -> (dir:dirs,mFilePath,appArgs);
    };
    interpretArgs ("-":rest) = ([],Nothing,rest);
    interpretArgs (s:rest) = ([],Just s,rest);
    interpretArgs [] = ([],Nothing,[]);

    main :: IO ();
    main = do
    {
        args <- getArgs;
        let
        {
            (dirs,mFilePath,appArgs) = interpretArgs args;
        };
        s <- case mFilePath of
        {
            Nothing -> getContents;
            Just filePath -> readFile filePath;
        };
        r :: Value <- evaluateWithDirs dirs s;
        _ :: Value <- fromValue (applyValue r (toValue appArgs));
        return ();
    };
}
