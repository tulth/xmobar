-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu.FreeBSD
-- Copyright   :  (c) 2011, 2017 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Cpu.FreeBSD (parseCpu
                                           , CpuDataRef
                                           , cpuData) where

import Xmobar.Plugins.Monitors.Cpu.Common (CpuData(..))
import Data.IORef (IORef, readIORef, writeIORef)
import System.BSD.Sysctl (sysctlPeekArray)

-- kern.cp_time data from the previous iteration for computing the difference
type CpuDataRef = IORef [Word]

cpuData :: IO [Word]
cpuData = sysctlPeekArray "kern.cp_time" :: IO [Word]

parseCpu :: CpuDataRef -> IO CpuData
parseCpu cref = do
    prev <- readIORef cref
    curr <- cpuData
    writeIORef cref curr
    let diff = map fromIntegral $ zipWith (-) curr prev
        user = head diff
        nice = diff !! 1
        system = diff !! 2
        intr = diff !! 3
        idle = diff !! 4
        total = user + nice + system + intr + idle
    return CpuData
      { cpuUser = user/total
      , cpuNice = nice/total
      , cpuSystem = (system+intr)/total
      , cpuIdle = idle/total
      , cpuIowait = 0
      , cpuTotal = user/total
      }
