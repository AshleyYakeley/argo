{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.File(fileFunctions) where
{
    import Import;
    import System.IO(hClose);
    import System.FilePath;
    import System.Directory hiding (getCurrentDirectory,setCurrentDirectory,isSymbolicLink);
    import System.Posix.Files;
    import System.Posix.Temp;
    import qualified Data.ByteString as B;
    import Data.Argo.Object;
    import Data.Argo.Value;
    import Data.Argo.StdLib.Types();

    fileType :: FileStatus -> String;
    fileType fs | isRegularFile fs = "file";
    fileType fs | isDirectory fs = "directory";
    fileType fs | isSymbolicLink fs = "slink";
    fileType fs | isNamedPipe fs = "pipe";
    fileType fs | isSocket fs = "socket";
    fileType fs | isBlockDevice fs = "block";
    fileType fs | isCharacterDevice fs = "char";
    fileType _ = "unknown";

    instance ToValue FileStatus where
    {
        toValue fs = toValue (MkObject
        [
            ("owner",toValue (fileOwner fs)),
            ("group",toValue (fileGroup fs)),
            ("size",toValue (fileSize fs)),
            ("type",toValue (fileType fs))
        ]);
    };

    pathStatus :: FilePath -> IO (Maybe FileStatus);
    pathStatus path = do
    {
        ex <- fileExist path;
        if ex then do
        {
            st <- getFileStatus path;
            return (Just st);
        }
        else return Nothing;
    };

    pathType :: FilePath -> IO (Maybe String);
    pathType path = do
    {
        mstat <- pathStatus path;
        return (fmap fileType mstat);
    };

    pathConcat :: [FilePath] -> FilePath;
    pathConcat [p] = p;
    pathConcat (p:pp) = combine p (pathConcat pp);
    pathConcat [] = ".";

    withTempFile :: (FilePath -> IO Value) -> IO Value;
    withTempFile f = do
    {
        (path,h) <- mkstemp "/tmp/temp.";
        hClose h;
        result <- f path;
        removeLink path;
        return result;
    };

    withTempDir :: (FilePath -> IO Value) -> IO Value;
    withTempDir f = do
    {
        path <- mkdtemp "/tmp/temp.";
        result <- f path;
        removeDirectoryRecursive path;
        return result;
    };

    directoryContents :: FilePath -> IO [String];
    directoryContents dir = do
    {
        items <- getDirectoryContents dir;
        return (filter goodItem items);
    } where
    {
        goodItem "." = False;
        goodItem ".." = False;
        goodItem _ = True;
    };

    fileFunctions :: (?context :: String) => [(String,Value)];
    fileFunctions =
    [
        ("path-concat",toValue pathConcat),
        ("path-split",toValue splitPath),
        ("path-split-name",toValue splitFileName),
        ("path-rename",toValue rename),
        ("path-touch",toValue touchFile),
        ("path-setown",toValue setOwnerAndGroup),
        ("path-status",toValue pathStatus),
        ("path-type",toValue pathType),
        ("file-get",toValue B.readFile),
        ("file-set",toValue B.writeFile),
        ("file-append",toValue B.appendFile),
        ("file-remove",toValue removeLink),
        ("file-rename",toValue renameFile),
        ("file-copy",toValue copyFile),
        ("file-link",toValue createLink),
        ("directory-create",toValue (createDirectoryIfMissing True)),
        ("directory-remove",toValue removeDirectoryRecursive),
        ("directory-contents",toValue directoryContents),
        ("slink-create",toValue createSymbolicLink),
        ("slink-get",toValue readSymbolicLink),
        ("slink-setown",toValue setSymbolicLinkOwnerAndGroup),
        ("temp-file-with",toValue withTempFile),
        ("temp-directory-with",toValue withTempDir)
    ];
}
