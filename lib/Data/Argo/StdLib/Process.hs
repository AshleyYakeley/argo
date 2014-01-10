{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Argo.StdLib.Process(processFunctions) where
{
    import Import;
    import Control.Exception;
    import Control.Concurrent;
    import Data.ByteString;
    import System.Exit;
    import System.IO.Error;
    import System.Posix.Types;
    import System.Posix.IO;
    import System.Posix.Env;
    import System.Posix.Directory;
    import System.Posix.Process;
    import System.Posix.Signals;
    import System.Posix.User;
    import Foreign.Ptr;
    import Foreign.Marshal;
    import Data.Argo.Object;
    import Data.Argo.Value;
    import Data.Argo.StdLib.Types();


    instance ToValue ExitCode where
    {
        toValue ExitSuccess = toValue ();
        toValue (ExitFailure code) = toValue code;
    };

    processStatus :: (?context :: String) => ProcessStatus -> Maybe String -> Value;
    processStatus (Exited _) Nothing = toValue "exited";
    processStatus (Exited code) (Just "error") = toValue code;
    processStatus (Terminated _) Nothing = toValue "terminated";
    processStatus (Terminated signal) (Just "signal") = toValue signal;
    processStatus (Stopped _) Nothing = toValue "stopped";
    processStatus (Stopped signal) (Just "signal") = toValue signal;
    processStatus _ (Just _) = toValue ();

    failProcess :: ProcessStatus -> IO ();
    failProcess (Exited ExitSuccess) = return ();
    failProcess p = fail ("process: " ++ (show p));

    instance ToValue ProcessStatus where
    {
        toValue ps = toValue (processStatus ps);
    };

    waitProcess :: (?context :: String) => ProcessID -> IO ProcessStatus;
    waitProcess pid = do
    {
        mps <- getProcessStatus True False pid;
        case mps of
        {
            Just ps -> return ps;
            Nothing -> failC "process still running after wait";
        };
    };

    processID :: (?context :: String) => ProcessID -> String -> Value;
    processID (CPid pid) "id" = toValue pid;
    processID pid "check" = toValue (getProcessStatus False True pid);
    processID pid "wait" = toValue (waitProcess pid);
    processID pid "signal" = toValue (\signal -> signalProcess signal pid);
    processID pid "terminate" = toValue (signalProcess softwareTermination pid);
    processID pid "kill" = toValue (signalProcess killProcess pid);
    processID _ _ = toValue ();

    instance ToValue ProcessID where
    {
        toValue pid = toValue (processID pid);
    };

    fdReadBytes :: Fd -> ByteCount -> IO (Maybe ByteString);
    fdReadBytes fd nbytes = allocaBytes (fromIntegral nbytes) $ \buf -> do
    {
        rc <- fdReadBuf fd buf nbytes;
        case rc of
        {
            0 -> return Nothing;
            n -> do
            {
                bs <- packCStringLen (castPtr buf,fromIntegral n);
                return (Just bs);
            };
        };
    };

    pushAll :: forall m a. (Monad m) => m (Maybe a) -> (Maybe a -> m ()) -> m ();
    pushAll from to = let
    {
        f :: m ();
        f = do
        {
            ma <- from;
            to ma;
            case ma of
            {
                Just _ -> f;
                Nothing -> return ();
            };
        };
    } in f;

    startProcess :: (?context :: String) => Object Value -> IO ProcessID;
    startProcess fargs = let
    {
        cmdpath = case objectLookup fargs "" of
        {
            Just v -> fromValue v;
            Nothing -> errorC "missing path";
        };
        args = case objectLookup fargs "args" of
        {
            Just a -> fromValue a;
            Nothing -> [];
        };
        env = fmap (unObject . fromValue) (objectLookup fargs "env");
        mOutPush = fmap fromValue (objectLookup fargs "out-push");
    } in do
    {
        mfd <- case mOutPush of
        {
            Just outPush -> do
            {
                (readFrom,writeTo) <- createPipe;
                _ <- forkIO (do
                {
                    pushAll (fdReadBytes readFrom 10000) outPush;
                    closeFd readFrom;
                    closeFd writeTo;
                });
                return (Just writeTo);
            };
            Nothing -> return Nothing;
        };
        forkProcess (do
        {
            setContext fargs;
            -- set real userid to the effective userid
            ruid <- getRealUserID;
            if ruid == 0 then do
            {
                euid <- getEffectiveUserID;
                setEffectiveUserID ruid;
                setUserID euid;
            }
            else return ();
            case mfd of
            {
                Just fd -> do
                {
                    _ <- dupTo fd stdOutput;
                    return ();
                };
                Nothing -> return ();
            };
            executeFile cmdpath False args env;
        });
    };

    runProcess :: (?context :: String) => Object Value -> IO ();
    runProcess fargs = do
    {
        pid <- startProcess fargs;
        ps <- waitProcess pid;
        failProcess ps;
    };

    getContext :: (?context::String) => IO (Object Value);
    getContext = do
    {
        ruid <- getRealUserID;
--        rusername <- getLoginName;
        rgid <- getRealGroupID;
        euid <- getEffectiveUserID;
--        eusername <- getEffectiveUserName;
        egid <- getEffectiveGroupID;
        groups <- getGroups;
        env <- getEnvironment;
        wd <- getWorkingDirectory;
        return (MkObject
        [
            ("real-user",toValue ruid),
--            ("real-username",toValue rusername),
            ("real-group",toValue rgid),
            ("user",toValue euid),
--            ("username",toValue eusername),
            ("group",toValue egid),
            ("groups",toValue groups),
            ("environment",toValue env),
            ("wd",toValue wd)
        ]);
    };

    setContext :: (?context :: String) => Object Value -> IO ();
    setContext args = do
    {
        case objectLookup args "user" of
        {
            Just uid -> setEffectiveUserID (fromValue uid);
            Nothing -> return ();
        };
        case objectLookup args "group" of
        {
            Just gid -> setEffectiveGroupID (fromValue gid);
            Nothing -> return ();
        };
        case objectLookup args "environment" of
        {
            Just env -> setEnvironment (fromValue env);
            Nothing -> return ();
        };
        case objectLookup args "wd" of
        {
            Just wd -> changeWorkingDirectory (fromValue wd);
            Nothing -> return ();
        };
    };

    withContext :: (?context :: String) =>
     Object Value -> IO Value -> IO Value;
    withContext args f0 = let
    {
        f1 = withThing getEnvironment setEnvironment (fmap fromValue (objectLookup args "environment")) f0;
        -- low-privilege user must be "inside" group and WD
        f2 = withThingCheck getEffectiveUserID setEffectiveUserID (fmap fromValue (objectLookup args "user")) f1;
        f3 = withThingCheck getEffectiveGroupID setEffectiveGroupID (fmap fromValue (objectLookup args "group")) f2;
        f4 = withThingCheck getWorkingDirectory changeWorkingDirectory (fmap fromValue (objectLookup args "wd")) f3;
    } in f4;

    withThing :: IO a -> (a -> IO ()) -> Maybe a -> IO Value -> IO Value;
    withThing _getter _setter Nothing f = f;
    withThing getter setter (Just thing) f = do
    {
        oldthing <- getter;
        setter thing;
        finally f (setter oldthing);
    };

    withThingCheck :: (Eq a) => IO a -> (a -> IO ()) -> Maybe a -> IO Value -> IO Value;
    withThingCheck _getter _setter Nothing f = f;
    withThingCheck getter setter (Just thing) f = do
    {
        oldthing <- getter;
        if thing == oldthing then f
        else do
        {
            setter thing;
            finally f (setter oldthing);
        };
    };

    instance ToValue UserEntry where
    {
        toValue ue = toValue (MkObject
        [
            ("name",toValue (userName ue)),
            ("password",toValue (userPassword ue)),
            ("id",toValue (userID ue)),
            ("group",toValue (userGroupID ue)),
            ("gecos",toValue (userGecos ue)),
            ("home",toValue (homeDirectory ue)),
            ("shell",toValue (userShell ue))
        ]);
    };

    checkUserEntry :: IO UserEntry -> IO (Maybe UserEntry);
    checkUserEntry f = catch (do
    {
        ue <- f;
        return (Just ue);
    }) (\(ex :: IOError) -> if ioeGetErrorType ex == doesNotExistErrorType
     then return Nothing
     else throw ex);

    getUserEntryForNameMaybe :: String -> IO (Maybe UserEntry);
    getUserEntryForNameMaybe s = checkUserEntry (getUserEntryForName s);

    getUserEntryForIDMaybe :: UserID -> IO (Maybe UserEntry);
    getUserEntryForIDMaybe uid = checkUserEntry (getUserEntryForID uid);

    processFunctions :: (?context :: String) => [(String,Value)];
    processFunctions =
    [
        ("context-get",toValue getContext),
        ("context-with",toValue withContext),
        ("exec-start",toValue startProcess),
        ("exec-run",toValue runProcess),
        ("userentry-id-get",toValue getUserEntryForIDMaybe),
        ("userentry-name-get",toValue getUserEntryForNameMaybe)
    ];
}
