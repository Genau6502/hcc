module TestSuite where

import Control.Monad
import TestTypes
import TokeniserTest
import ParserTest

tests :: [TestGroup]
tests = [ tokeniserTests, parseAtomTests, parseExprTests, parseStmtTests, parseDeclaratorTests, parseAbstractDeclaratorTests
    ]

runTests :: IO ()
runTests = runTestGroups tests

testgroup :: String -> [TestCase] -> TestGroup
testgroup name ts = TestGroup (name, ts)

runTestGroup :: TestGroup -> IO ()
runTestGroup (TestGroup (name, ts)) = do
    putStrLn ("Running test group " ++ name)
    i <- foldM (flip runTest) 0 ts
    putStrLn ("Test group " ++ name ++ ": " ++ show i ++ "/" ++ show (length ts) ++ " tests passed\n")

runTestGroups :: [TestGroup] -> IO ()
runTestGroups (g:gs) = do
    runTestGroup g
    runTestGroups gs
runTestGroups [] = return ()

runTest :: TestCase -> Int -> IO Int
runTest (TestCase name passed act expected) i = 
    let (res, totalPassed) = if passed 
            then ("PASS", i + 1) 
            else ("FAIL", i) 
    in do
        putStrLn $ "  Running test " ++ name ++ " - " ++ res
        if not passed 
            then do 
                putStrLn $ "    Expected: " ++ expected
                putStrLn $ "    Actual:   " ++ act
            else return ()
            
        return totalPassed
