import Distribution.Simple
import Distribution.Simple.Haddock (haddock, hscolour)
import Distribution.PackageDescription

main = defaultMainWithHooks (simpleUserHooks {
    preHaddock = \a f -> do
        print a
        print f
        return (Just $ emptyBuildInfo { hsSourceDirs = ["noqqsrc"]},[]) })

