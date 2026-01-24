{- |
Module      : DAX
Description : DAX (Data eXpression) Era - Conditional rendering and basic logic
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : PMPL-1.0-or-later
Maintainer  : hyperpolymath

DAX provides conditional rendering for templates based on metadata values.
Simplified MVP version for v1.0.
-}

module DAX
    ( evalCondition
    , processConditionals
    ) where

import qualified Data.Map.Strict as Map
import Types (Context, FlexiText(..))

-- | Evaluate a simple condition like "phase == alpha"
evalCondition :: Context -> String -> Bool
evalCondition ctx condition =
    let trimmed = trim condition
    in case parseCondition trimmed of
        Just (key, op, value) ->
            case Map.lookup key ctx of
                Just (FlexiText actual _) -> compareValues op actual value
                Nothing -> False
        Nothing -> False

-- | Parse condition string into (key, operator, value)
parseCondition :: String -> Maybe (String, String, String)
parseCondition s
    | "==" `isInfixOf` s = splitOn "==" s >>= \(k, v) -> Just (trim k, "==", trim v)
    | "!=" `isInfixOf` s = splitOn "!=" s >>= \(k, v) -> Just (trim k, "!=", trim v)
    | otherwise = Nothing
  where
    splitOn :: String -> String -> Maybe (String, String)
    splitOn needle haystack =
        case breakOn needle haystack of
            (before, after) | not (null after) ->
                Just (before, drop (length needle) after)
            _ -> Nothing

    breakOn :: String -> String -> (String, String)
    breakOn needle haystack = go "" haystack
      where
        go acc [] = (reverse acc, "")
        go acc str@(x:xs)
            | needle `isPrefixOf` str = (reverse acc, str)
            | otherwise = go (x:acc) xs

-- | Compare values based on operator
compareValues :: String -> String -> String -> Bool
compareValues "==" a b = a == b
compareValues "!=" a b = a /= b
compareValues _ _ _ = False

-- | Process {{#if}} conditionals in template (simplified)
processConditionals :: Context -> String -> String
processConditionals _ template = template  -- Simplified for v1.0

-- Helper functions
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
