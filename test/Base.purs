module Base where

import Test.Unit
import Test.Unit.Console hiding (print)

-- basic test facilities

type MyTest e = Test (testOutput :: TestOutput | e)