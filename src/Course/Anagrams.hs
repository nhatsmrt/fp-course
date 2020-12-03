{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
-- perm :: chars = list char
-- content :: str = list char
--
anagrams str fp = (\content -> intersectBy equalIgnoringCase (lines content) (permutations str)) <$> (readFile fp)

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase s1 s2 = eq (toLower <$> s1) (toLower <$> s2)
  where
    eq s1 s2 = case (s1, s2) of
      (Nil, Nil) -> True
      (Nil, _) -> False
      (_, Nil) -> False
      (h1 :. t1, h2 :. t2) -> (h1 == h2) && (eq t1 t2)
