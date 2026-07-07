module TestTypes where

newtype TestGroup = TestGroup (String, [TestCase])

data TestCase = TestCase 
    { testName   :: String
    , testPassed :: Bool
    , testActual :: String
    , testExpect :: String
    }

-- Set the precedence so we don't need parentheses
infixr 0 -:
infix  1 -->

(-->) :: (Eq a, Show a) => a -> a -> TestCase
act --> expected = TestCase 
    { testName   = "Unnamed Test"
    , testPassed = act == expected
    , testActual = show act
    , testExpect = show expected
    }

(-:) :: String -> TestCase -> TestCase
name -: test = test { testName = name }