module Main where
import Week8
import Data.Tree
import qualified Employee as E

main :: IO ()

main = do
    companyRaw <- readFile "/home/dsemenov/var/companies"
    let (E.GL _ fun) = maxFun $ read companyRaw
     in print fun
