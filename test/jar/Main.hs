module Main where

import ClassyPrelude
import Jar
import Jar.Noun


-- Test Values -----------------------------------------------------------------

a, b :: Noun
a = "aaaaaaaa"
b = "bbbbbbbb"
c = "cccccccc"
d = "dddddddd"

isPin :: Noun
isPin =
    ( 0
    % 474213282665
    % 1
    % ( 0
      % ( 2
        % ( 1
          % (0 % 0 % 1 % (2%1))
          % (0 % 0 % 3 % (2%0))
          % (0 % 0 % 2 % (2%0))
          % 0
          )
        )
      % 1
      )
    )

isPin' :: Noun
isPin' =
    ( (0 % 0)
    % (0 % 0 % 0 % (2%0))
    % (0 % 0 % 1 % (2%0))
    )


-- Tests -----------------------------------------------------------------------

checkTrip :: Noun -> IO ()
checkTrip n = do
    print ("CHECK"::Text, n)
    showJarBits n
    case capJarTest n of
        Left err         -> error (show err)
        Right vl | vl==n -> putStrLn "OK"
        Right vl         -> do print n
                               print vl
                               error "NOT EQUALS"

main :: IO ()
main = do
    checkTrip 8
    checkTrip isPin'
    checkTrip isPin

    for_ [0..2048] (checkTrip . mkAtom)

    for_ [0..2048] \(mkAtom -> i) -> do
        checkTrip (i%i)
        checkTrip ((i%i) % (i%i))

    for_ [0..2048] \n -> do
        let i = mkAtom n
        let j = mkAtom (n*n*n)
        checkTrip ( ((i%i) % (i%a))
                  % j
                  % (5 % c % d)
                  % ((2%i) % b % (i%i))
                  )
