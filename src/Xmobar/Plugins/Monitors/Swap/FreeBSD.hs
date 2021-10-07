-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Swap.FreeBSD
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Swap.FreeBSD (parseMEM) where

import System.BSD.Sysctl (sysctlReadUInt, sysctlReadULong)

isEnabled :: IO Bool
isEnabled = do
  enabled <- sysctlReadUInt "vm.swap_enabled"
  return $ enabled == 1

parseMEM' :: Bool -> IO [Float]
parseMEM' False = return []
parseMEM' True = do
  swapIn <- sysctlReadUInt "vm.stats.vm.v_swapin"
  swapTotal <- sysctlReadULong "vm.swap_total"
  let tot = toInteger swapTotal
      free = tot - toInteger swapIn

  return $ res (fromInteger tot) (fromInteger free)
  where
    res :: Float -> Float -> [Float]
    res _ 0 = []
    res tot free = [(tot - free) / tot, tot, tot - free, free]

parseMEM :: IO [Float]
parseMEM = do
  enabled <- isEnabled
  parseMEM' enabled
