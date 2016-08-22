{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment as E
import Control.Monad as M

-- | prints usage how to
usage :: IO()
usage = do
    putStrLn "u1 <firstDiviser> <secondDiviser> <upperBound>"

main :: IO ()
main = do
    args <- E.getArgs
    case args of
        (firstdArg: seconddArg: upperboundArg:_)-> do
            let firstDiv::Int = read firstdArg
                secondDiv:: Int = read seconddArg
                upperBound:: Int = read upperboundArg
                helper = solver firstDiv secondDiv -- closure to solver
            -- for every number in range, print answer
            case () of
                _| firstDiv == 0 -> putStrLn "error: firstDiviser is zero"
                _| secondDiv == 0 -> putStrLn "error: secondDiviser is zero"
                _| otherwise -> do
                    forM_ [ 0 .. upperBound ] $ \current -> do
                        putStrLn $ helper current
        _ -> usage

{-|
If divisible by both the first divisor and second divisor print "Door 1"
If divisible by just the first divisor then print "Door2"
If divisible by just the second divisor then print "Door 3"
If not divisible by either divisor then print the dividend itself.
 -}
solver:: Int-> Int-> Int-> String
solver firstDiv secondDiv upperbound
    | upperbound `mod` firstDiv == 0 && upperbound `mod` secondDiv == 0 = "Door 1"
    -- ^ If divisible by both the first divisor and second divisor print "Door 1"
    | upperbound `mod` firstDiv == 0 = "Door 2"
    -- ^ If divisible by just the first divisor then print "Door2"
    | upperbound `mod` secondDiv == 0 = "Door 3"
    -- ^ If divisible by just the second divisor then print "Door 3"
    | otherwise = show upperbound
    -- ^ If not divisible by either divisor then print the dividend itself.
