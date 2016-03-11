import PNGCreate
import System.Exit (exitSuccess, exitFailure)


tests :: [Bool]
tests = []


runTests tests = do
    if (and tests)
        then exitSuccess
        else do
            print tests
            exitFailure


main :: IO ()
main = do
    runTests tests
    return ()
