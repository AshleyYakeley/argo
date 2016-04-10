module Data.Argo.TextFile where
{
    import Codec.Binary.UTF8.String;
    import System.IO (stdin,stdout,Handle,FilePath);
    import qualified Data.ByteString as B;
    
    import Import;

    hPutStr :: Handle -> String -> IO ();
    hPutStr handle s = B.hPutStr handle $ B.pack $ encode s;

    putStr :: String -> IO ();
    putStr = hPutStr stdout;

    putStrLn :: String -> IO ();
    putStrLn s = putStr $ s ++ "\n";

    readFile :: FilePath -> IO String;
    readFile path = do
    {
        bytes <- B.readFile path;
        return $ decode $ B.unpack bytes;
    };
    
    hGetContents :: Handle -> IO String;
    hGetContents handle = do
    {
        bs <- B.hGetContents handle;
        return $ decode $ B.unpack bs;
    };
    
    getContents :: IO String;
    getContents = hGetContents stdin;
    
    hGetLine :: Handle -> IO String;
    hGetLine handle = do
    {
        bs <- B.hGetLine handle;
        return $ decode $ B.unpack bs;
    };
    
    getLine :: IO String;
    getLine = hGetLine stdin;
}
