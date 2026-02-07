{- |
Module      : SixSCMEnhanced
Description : Enhanced 6 SCM loader with deep tree traversal and dotted paths
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : PMPL-1.0-or-later
Maintainer  : hyperpolymath

Enhanced version supporting:
- Deep tree traversal (extract ALL keys, not just known ones)
- Dotted path lookups (e.g., "stats.repo-count", "META.philosophy")
- Cross-file references (e.g., "ECOSYSTEM.upstream-dependencies")
- Namespace prefixes for disambiguation
-}

module SixSCMEnhanced
    ( loadAll6SCMEnhanced
    , lookupDotted
    , extractAllKeys
    , SCMContext(..)
    ) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Types (Context, FlexiText(..))
import SExp (SExp(..), parseSExp)

-- | Context with namespace awareness
data SCMContext = SCMContext
    { stateContext     :: Context
    , ecosystemContext :: Context
    , metaContext      :: Context
    , neurosymContext  :: Context
    , agenticContext   :: Context
    , playbookContext  :: Context
    , mergedContext    :: Context  -- All contexts merged
    } deriving (Show)

-- | Default paths for 6scm files
defaultSCMPath :: FilePath
defaultSCMPath = ".machine_readable"

-- | Load all 6 SCM files with namespace support
loadAll6SCMEnhanced :: FilePath -> IO SCMContext
loadAll6SCMEnhanced basePath = do
    -- Load each file
    stateCtx     <- loadAndExtract (basePath </> "STATE.scm") "STATE"
    ecosystemCtx <- loadAndExtract (basePath </> "ECOSYSTEM.scm") "ECOSYSTEM"
    metaCtx      <- loadAndExtract (basePath </> "META.scm") "META"
    neurosymCtx  <- loadAndExtract (basePath </> "NEUROSYM.scm") "NEUROSYM"
    agenticCtx   <- loadAndExtract (basePath </> "AGENTIC.scm") "AGENTIC"
    playbookCtx  <- loadAndExtract (basePath </> "PLAYBOOK.scm") "PLAYBOOK"

    -- Create merged context (without namespace prefixes)
    let merged = Map.unions
            [ playbookCtx   -- Highest priority
            , agenticCtx
            , neurosymCtx
            , metaCtx
            , ecosystemCtx
            , stateCtx      -- Lowest priority
            ]

    return $ SCMContext
        { stateContext = stateCtx
        , ecosystemContext = ecosystemCtx
        , metaContext = metaCtx
        , neurosymContext = neurosymCtx
        , agenticContext = agenticCtx
        , playbookContext = playbookCtx
        , mergedContext = merged
        }

-- | Load a single SCM file and extract ALL keys
loadAndExtract :: FilePath -> String -> IO Context
loadAndExtract path namespace = do
    exists <- doesFileExist path
    if not exists
        then return Map.empty
        else do
            content <- readFile path
            case parseSExp content of
                Nothing -> return Map.empty
                Just tree -> return $ extractAllKeys namespace [] tree

-- | Extract ALL keys from S-expression tree recursively
-- Creates keys like "name", "stats.repo-count", "vital-signs.health-score"
extractAllKeys :: String -> [String] -> SExp -> Context
extractAllKeys namespace path sexp =
    case sexp of
        Atom _ -> Map.empty
        List items -> extractFromList namespace path items

-- | Extract keys from a list of S-expressions
extractFromList :: String -> [String] -> [SExp] -> Context
extractFromList namespace path items =
    case items of
        -- Dotted pair: (key . "value")
        [Atom key, Atom ".", Atom value] ->
            let fullPath = makePath path key
                flexiText = FlexiText value (fullPath ++ ": " ++ value)
            in Map.fromList [(fullPath, flexiText)]

        -- Simple pair: (key "value")
        [Atom key, Atom value] ->
            let fullPath = makePath path key
                flexiText = FlexiText value (fullPath ++ ": " ++ value)
            in Map.fromList [(fullPath, flexiText)]

        -- Nested list: (key (nested ...))
        [Atom key, List nested] ->
            let newPath = path ++ [key]
            in extractFromList namespace newPath nested

        -- Multiple items: process each
        (Atom key : List nested : rest) ->
            let newPath = path ++ [key]
                nestedCtx = extractFromList namespace newPath nested
                restCtx = extractFromList namespace path rest
            in Map.union nestedCtx restCtx

        -- Two atoms followed by more: treat first two as pair
        (Atom key : Atom value : rest) ->
            let fullPath = makePath path key
                flexiText = FlexiText value (fullPath ++ ": " ++ value)
                pairCtx = Map.singleton fullPath flexiText
                restCtx = extractFromList namespace path rest
            in Map.union pairCtx restCtx

        -- Other patterns: recurse on each item
        (x:xs) ->
            let xCtx = extractAllKeys namespace path x
                xsCtx = extractFromList namespace path xs
            in Map.union xCtx xsCtx

        [] -> Map.empty

-- | Make a dotted path from components
makePath :: [String] -> String -> String
makePath [] key = key
makePath path key = intercalate "." (path ++ [key])

-- | Lookup a dotted path in context
-- Examples: "repo-count", "stats.repo-count", "vital-signs.health-score"
lookupDotted :: String -> Context -> Maybe FlexiText
lookupDotted key ctx = Map.lookup key ctx

-- | Lookup with namespace prefix
-- Examples: "STATE.stats.repo-count", "META.philosophy"
lookupNamespaced :: String -> SCMContext -> Maybe FlexiText
lookupNamespaced key ctx =
    case splitOn "." key of
        ("STATE":rest) -> lookupDotted (intercalate "." rest) (stateContext ctx)
        ("ECOSYSTEM":rest) -> lookupDotted (intercalate "." rest) (ecosystemContext ctx)
        ("META":rest) -> lookupDotted (intercalate "." rest) (metaContext ctx)
        ("NEUROSYM":rest) -> lookupDotted (intercalate "." rest) (neurosymContext ctx)
        ("AGENTIC":rest) -> lookupDotted (intercalate "." rest) (agenticContext ctx)
        ("PLAYBOOK":rest) -> lookupDotted (intercalate "." rest) (playbookContext ctx)
        _ -> lookupDotted key (mergedContext ctx)  -- No namespace = merged

-- | Get all keys from a context (for debugging)
getAllKeys :: Context -> [String]
getAllKeys = Map.keys

-- | Example usage in template rendering:
-- Template: "Repository count: (:stats.repo-count)"
-- Lookup: lookupDotted "stats.repo-count" context
--
-- Template: "Philosophy: (:META.philosophy)"
-- Lookup: lookupNamespaced "META.philosophy" context
