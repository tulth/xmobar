{-#LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Mem (memConfig, runMem, totalMem, usedMem) where

import Xmobar.Plugins.Monitors.Common
import System.Console.GetOpt
#ifdef FREEBSD
import System.BSD.Sysctl (sysctlReadUInt)
#else
import qualified Data.Map as M
#endif

data MemOpts = MemOpts
  { usedIconPattern :: Maybe IconPattern
  , freeIconPattern :: Maybe IconPattern
  , availableIconPattern :: Maybe IconPattern
  }

defaultOpts :: MemOpts
defaultOpts = MemOpts
  { usedIconPattern = Nothing
  , freeIconPattern = Nothing
  , availableIconPattern = Nothing
  }

options :: [OptDescr (MemOpts -> MemOpts)]
options =
  [ Option "" ["used-icon-pattern"] (ReqArg (\x o ->
     o { usedIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["free-icon-pattern"] (ReqArg (\x o ->
     o { freeIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["available-icon-pattern"] (ReqArg (\x o ->
     o { availableIconPattern = Just $ parseIconPattern x }) "") ""
  ]

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["usedbar", "usedvbar", "usedipat", "freebar", "freevbar", "freeipat",
        "availablebar", "availablevbar", "availableipat",
        "usedratio", "freeratio", "availableratio",
        "total", "free", "buffer", "cache", "available", "used"] -- available replacements

#ifdef FREEBSD
parseMEM :: IO [Float]
parseMEM = do stats <- mapM sysctlReadUInt [
                "vm.stats.vm.v_page_size"
                , "vm.stats.vm.v_page_count"
                , "vm.stats.vm.v_free_count"
                , "vm.stats.vm.v_active_count"
                , "vm.stats.vm.v_inactive_count"
                , "vm.stats.vm.v_wire_count"
                , "vm.stats.vm.v_cache_count"]

              let [ pagesize, totalpages, freepages, activepages, inactivepages, wiredpages, cachedpages ] = fmap fromIntegral stats
                  usedpages = activepages + wiredpages + cachedpages
                  availablepages = inactivepages + cachedpages + freepages
                  bufferedpages = activepages + inactivepages + wiredpages

                  available = availablepages * pagesize
                  used = usedpages * pagesize
                  free = freepages * pagesize
                  cache = cachedpages * pagesize
                  buffer = bufferedpages * pagesize
                  total = totalpages * pagesize

                  usedratio = usedpages / totalpages
                  freeratio = freepages / totalpages
                  availableratio = availablepages / totalpages

              return [usedratio, freeratio, availableratio, total, free, buffer, cache, available, used]

#else
fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content = map words $ take 8 $ lines file
           info = M.fromList $ map (\line -> (head line, (read $ line !! 1 :: Float) / 1024)) content
           [total, free, buffer, cache] = map (info M.!) ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]
           available = M.findWithDefault (free + buffer + cache) "MemAvailable:" info
           used = total - available
           usedratio = used / total
           freeratio = free / total
           availableratio = available / total
       return [usedratio, freeratio, availableratio, total, free, buffer, cache, available, used]
#endif

totalMem :: IO Float
totalMem = fmap ((*1024) . (!!1)) parseMEM

usedMem :: IO Float
usedMem = fmap ((*1024) . (!!6)) parseMEM

formatMem :: MemOpts -> [Float] -> Monitor [String]
formatMem opts (r:fr:ar:xs) =
    do let f = showDigits 0
           mon i x = [showPercentBar (100 * x) x, showVerticalBar (100 * x) x, showIconPattern i x]
       sequence $ mon (usedIconPattern opts) r
           ++ mon (freeIconPattern opts) fr
           ++ mon (availableIconPattern opts) ar
           ++ map showPercentWithColors [r, fr, ar]
           ++ map (showWithColors f) xs
formatMem _ _ = replicate 10 `fmap` getConfigValue naString

runMem :: [String] -> Monitor String
runMem argv =
    do m <- io parseMEM
       opts <- io $ parseOptsWith options defaultOpts argv
       l <- formatMem opts m
       parseTemplate l
