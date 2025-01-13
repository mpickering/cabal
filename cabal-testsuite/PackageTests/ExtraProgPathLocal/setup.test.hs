import Test.Cabal.Prelude
-- Test package local extra-prog-path works.
main = cabalTest $ do
    skipIfWindows
    env <- getTestEnv
    liftIO $ appendFile (testCurrentDir env </> "cabal.project") $ "\npackage client\n  extra-prog-path:" ++ (testCurrentDir env </> "scripts2/")
    addToPath (testTmpDir env </> "scripts/") $ cabal "v2-build" ["client"]
