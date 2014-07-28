{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( generateCoverallsFromTix ) where

import           Data.Aeson
import           Data.Aeson.Types ()
import           Data.List
import qualified Data.Map.Strict as M
import           System.Exit (exitFailure)
import           Trace.Hpc.Coveralls.Config
import           Trace.Hpc.Coveralls.Lix
import           Trace.Hpc.Coveralls.Paths
import           Trace.Hpc.Coveralls.Types
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Mix hiding (readMix)
import           Trace.Hpc.Tix
import           Trace.Hpc.Util
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

type ModuleCoverageData = (
    String,    -- file source code
    Mix,       -- module index data
    [Integer]) -- tixs recorded by hpc

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageValue]

-- Is there a way to restrict this to only Number and Null?
type CoverageValue = Value

type LixConverter = Lix -> SimpleCoverage

strictConverter :: LixConverter
strictConverter = map $ \lix -> case lix of
    Full       -> Number 1
    Partial    -> Number 0
    None       -> Number 0
    Irrelevant -> Null

looseConverter :: LixConverter
looseConverter = map $ \lix -> case lix of
    Full       -> Number 2
    Partial    -> Number 1
    None       -> Number 0
    Irrelevant -> Null

toSimpleCoverage :: LixConverter -> Int -> [CoverageEntry] -> SimpleCoverage
toSimpleCoverage convert lineCount = convert . toLix lineCount

getExprSource :: [String] -> MixEntry -> [String]
getExprSource source (hpcPos, _) = subSubSeq startCol endCol subLines
    where subLines = subSeq startLine endLine source
          startLine = startLine' - 1
          startCol = startCol' - 1
          (startLine', startCol', endLine, endCol) = fromHpcPos hpcPos

coverageToJson :: LixConverter -> FilePath -> ModuleCoverageData -> Value
coverageToJson converter filePath (source, mix, tixs) = object [
    "name" .= filePath,
    "source" .= source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage converter lineCount mixEntryTixs
          lineCount = length $ lines source
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ lines source

toCoverallsJson :: String -> String -> LixConverter -> TestSuiteCoverageData -> Value
toCoverallsJson serviceName jobId converter testSuiteCoverageData = object [
    "service_job_id" .= jobId,
    "service_name" .= serviceName,
    "source_files" .= toJsonCoverageList testSuiteCoverageData]
    where toJsonCoverageList = map (uncurry $ coverageToJson converter) . M.toList

mergeModuleCoverageData :: ModuleCoverageData -> ModuleCoverageData -> ModuleCoverageData
mergeModuleCoverageData (source, mix, tixs1) (_, _, tixs2) =
    (source, mix, zipWith (+) tixs1 tixs2)

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = foldr1 (M.unionWith mergeModuleCoverageData)

readMix' :: String -> TixModule -> IO Mix
readMix' name tix = readMix [getMixPath name tix] (Right tix)

readMix :: [String]                 -- ^ Dir Names
        -> Either String TixModule  -- ^ module wanted
        -> IO Mix
readMix dirNames mod' = do
   let modName = case mod' of
                    Left str -> str
                    Right tix -> tixModuleName tix
   res <- sequence [ (do contents <- readFile (mixName dirName modName)
                         print contents
                         case reads contents of
                           [(r@(Mix _ _ h _ _),cs)]
                                | all isSpace cs
                               && (case mod' of
                                     Left  _   -> True
                                     Right tix -> h == tixModuleHash tix
                                  ) -> do
                                    print "success"
                                    return $ Just r
                                | all isSpace cs -> do
                                  print "not accord hash of tix"
                                  return Nothing
                                | (case mod' of
                                     Left  _   -> True
                                     Right tix -> h == tixModuleHash tix
                                  ) -> do
                                    print "cs is not only space"
                                    return Nothing
                           _ -> do
                             print "fail pattern match"
                             return $ Nothing
                         ) `catchIO` (\ _ -> do
                                     print "catch error"
                                     return $ Nothing)
                   | dirName <- dirNames
                   ]
   case catMaybes res of
     [r] -> return r
     xs@(_:_) -> error $ "found " ++ show(length xs) ++ " instances of " ++ modName ++ " in " ++ show dirNames
     _        -> error $ "can not find " ++ modName ++ " in " ++ show dirNames

mixName :: FilePath -> String -> String
mixName dirName name = dirName ++ "/" ++ name ++ ".mix"

-- | Create a list of coverage data from the tix input
readCoverageData :: String                   -- ^ test suite name
                 -> [String]                 -- ^ excluded source folders
                 -> IO TestSuiteCoverageData -- ^ coverage data list
readCoverageData testSuiteName excludeDirPatterns = do
    tixPath <- getTixPath testSuiteName
    mtix <- readTix tixPath
    case mtix of
        Nothing -> error ("Couldn't find the file " ++ tixPath) >> exitFailure
        Just (Tix tixs) -> do
            mixs <- mapM (readMix' testSuiteName) tixs
            let files = map filePath mixs
            sources <- mapM readFile files
            let coverageDataList = zip4 files sources mixs (map tixModuleTixs tixs)
            let filteredCoverageDataList = filter sourceDirFilter coverageDataList
            return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4

-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String   -- ^ CI name
                         -> String   -- ^ CI Job ID
                         -> Config   -- ^ hpc-coveralls configuration
                         -> IO Value -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId config = do
    testSuitesCoverages <- mapM (`readCoverageData` excludedDirPatterns) testSuiteNames
    return $ toCoverallsJson serviceName jobId converter $ mergeCoverageData testSuitesCoverages
    where excludedDirPatterns = excludedDirs config
          testSuiteNames = testSuites config
          converter = case coverageMode config of
              StrictlyFullLines -> strictConverter
              AllowPartialLines -> looseConverter
