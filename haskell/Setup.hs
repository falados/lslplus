import Distribution.Simple
import Distribution.Simple.Haddock (haddock, hscolour)
import Distribution.PackageDescription

main = defaultMainWithHooks (simpleUserHooks {
    preHaddock = \_ _ -> return (Just $ emptyBuildInfo { hsSourceDirs = ["noqqsrc"]},[]) })

