---
title: "In-program GC stats for GHC"
date: 2011-07-20 09:00:00
slug: in-program-gc-stats-for-ghc
categories: [GHC]
comments:
    - id: 2823
      author: David Leuschner
      date: "2011-07-20 20:24:54"
      content: "Yeah!  That's just what we were looking for. Such an API would be absolutely great!  We currently track the external memory usage of our long running server process but that doesn't tell us much because we don't know how much of this big heap is \"live\".  We tried to capture the output of +RTS -Sfilename but GHC is too smart and uses block-buffering on the file which introduces a long delay.  We wouldn't be picky about the details of the API - any easy to implement variant would be great to have.  I think we'd favor fast availability over a stable API so we could get going, learn and then improve..."
    - id: 2824
      author: Felipe Lessa
      date: "2011-07-21 00:02:53"
      content: "Why are the record fields inside IO?  Doesn't make a lot of sense for me =(.  Other than that, looks pretty good =D."
    - id: 2825
      author: Johan Tibell
      date: "2011-07-21 03:03:52"
      content: |
        I'd love to have this. Some comments:
        
        * Please be clear about the concurrency guarantees in your API. For example, I wouldn't expect to see a consistent set of stats if I had to call multiple readIORefs in a row, while I would expect to see something consistent if I called readIORef once and got a record.
        
        * Make the API general enough to handle the possibility of changing the number of generations via the command line. E.g. does lastMajor GC make sense with 4 generations?
        
        * Does GHC already keep these counter around and it's only a matter of reading them? I would start out by offering an API that surfaces whatever GHC stores at the moment and thinking about aggregation (per task, etc) in v2, when we have a better understanding of the use cases.
        
        * I find numGenerations clearer than just generations.
    - id: 2835
      author: Edward Z. Yang
      date: "2011-07-22 07:22:20"
      content: |
        Felipe: Oops! That's been fixed.
        
        Johan: Yeah, the consistency guarantees you'd get for records makes it more likely that I'll end up implementing that, despite the extra memory needs of it. Generations are also a good point. Most of the functions came straight out of rts/Stats.c, so they should be "easy" in that sense.
    - id: 2848
      author: Johan Tibell
      date: "2011-07-24 10:49:08"
      content: "In what sense do records need more memory? Just to allocate them when the user asks for GC stats or do you plan to keep a record of GC stats that you update in each GC, regardless if the user asks for the stats or not? Aren't the counters maintained as a C struct already?"
    - id: 2849
      author: Edward Z. Yang
      date: "2011-07-24 15:56:05"
      content: "The allocation when the user asks for them. The counters are already maintained in C structs, so that's not a problem (it's not very much memory either.)"
    - id: 2852
      author: Johan Tibell
      date: "2011-07-25 10:35:41"
      content: "Ah OK. I think that allocation is quite reasonable. I don't expect the user to poll that quickly."
---

I’ll be at this year’s [Hac Phi](http://www.haskell.org/haskellwiki/Hac_φ) (coming up in a week and a half), and I am planning on working on in-program garbage collector statistics for GHC. There is nothing really technically difficult about this task (we just need to expose some functions in the RTS), but it’s not been done yet and I know quite a few performance-minded and long-running-server-minded folks who would love to see this functionality.

My question for you is this: what would you like such an API to look like? What things should it offer, how would you like to interact with it?

Here’s one sample API to get the ball rolling:

    module GHC.RTS.Stats where

    -- Info is not collected unless you run with certain RTS options.  If
    -- you are planning on using this on a long-running server, costs of the
    -- options would be good to have (we also probably need to add extra
    -- options which record, but have no outwardly visible effect.)

    -- Read out static parameters that were provided via +RTS

    generations :: IO Int

    ---------------------------------------------------------------------
    -- Full statistics

    -- Many stats are internally collected as words. Should be publish
    -- words?

    -- Names off of machine readable formats

    bytesAllocated :: IO Int64
    numGCs :: IO Int64
    numByteUsageSamples :: IO Int64
    averageBytesUsed :: IO Int64 -- cumulativeBytesUsed / numByteUsageSamples
    maxBytesUsed :: IO Int64
    -- peakMemoryBlocksAllocated :: IO Int64
    peakMegabytesAllocated :: IO Int64
    initCpuSeconds :: IO Double
    initWallSeconds :: IO Double
    mutatorCpuSeconds :: IO Double
    mutatorWallSeconds :: IO Double
    gcCpuSeconds :: IO Double
    gcWallSeconds :: IO Double

    -- Wouldn't be too unreasonable to offer a data structure with all of
    -- this?  Unclear.  At least, it would prevent related data from
    -- desynchronizing.

    data GlobalStats = GlobalStats
        { g_bytes_allocated :: Int64
        , g_num_GCs :: Int64
        , g_num_byte_usage_samples :: Int64
        , g_average_bytes_used :: Int64
        , g_max_bytes_used :: Int64
        , g_peak_megabytes_allocated :: Int64
        , g_init_cpu_seconds :: Double
        , g_init_wall_seconds :: Double
        , g_mutator_cpu_seconds :: Double
        , g_mutator_wall_seconds :: Double
        , g_gc_cpu_seconds :: Double
        , g_gc_wall_seconds :: Double
        }
    globalStats :: IO GlobalStats
    generationStats :: Int -> IO GlobalStats

    ---------------------------------------------------------------------
    -- GC statistics

    -- We can't offer a realtime stream of GC events, because they come
    -- to fast. (Test? eventlog comes to fast, maybe GC is manageable,
    -- but you don't want to trigger GC in your handler.)

    data GCStats = GCStats
        { gc_alloc :: Int64
        , gc_live :: Int64
        , gc_copied :: Int64
        , gc_gen :: Int
        , gc_max_copied :: Int64
        , gc_avg_copied :: Int64
        , gc_slop :: Int64
        , gc_wall_seconds :: Int64
        , gc_cpu_seconds :: Int64
        , gc_faults :: Int
        }
    lastGC :: IO GCStats
    lastMajorGC :: IO GCStats
    allocationRate :: IO Double

    ---------------------------------------------------------------------
    -- Parallel GC statistics

    data ParGCStats = ParGCStats
        { par_avg_copied :: Int64
        , par_max_copied :: Int64
        }
    parGCStats :: IO ParGCStats
    parGCNodes :: IO Int64


    ---------------------------------------------------------------------
    -- Threaded runtime statistics
    data TaskStats = TaskStats
        -- Inconsistent naming convention here: mut_time or mut_cpu_seconds?
        -- mut_etime or mut_wall_seconds? Hmm...
        { task_mut_time :: Int64
        , task_mut_etime :: Int64
        , task_gc_time :: Int64
        , task_gc_etime :: Int64
        }

    ---------------------------------------------------------------------
    -- Spark statistics

    data SparkStats = SparkStats
        { s_created :: Int64
        , s_dud :: Int64
        , s_overflowed :: Int64
        , s_converted :: Int64
        , s_gcd :: Int64
        , s_fizzled :: Int64
        }
    sparkStats :: IO SparkStats
    sparkStatsCapability :: Int -> IO SparkStats
