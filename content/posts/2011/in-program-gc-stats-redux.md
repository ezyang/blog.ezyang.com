---
title: "In-program GC stats redux"
date: 2011-08-03 09:00:08
slug: in-program-gc-stats-redux
categories: [GHC]
comments:
    - id: 2879
      author: Johan Tibell
      date: "2011-08-03 12:21:42"
      content: "It's not clear to me if all the stats are just for the last GC or if some (e.g. num_byte_usage_samples and cumulative_bytes_used) somehow carry over from previous GCs. Perhaps that part could be clarified in the documentation?"
    - id: 2880
      author: Felipe Lessa
      date: "2011-08-03 12:36:05"
      content: "Will it be able to make it into GHC 6.2?  I guess not, right?  =/"
    - id: 2881
      author: Edward Z. Yang
      date: "2011-08-03 13:09:54"
      content: |
        Johan: Yes, some of the stats are cumulative. I can clarify that. (Originally, I considered giving per-GC stats, but since you're not getting information every GC, you'll be losing lots. Cumulative makes it easier to track.)
        
        Felipe: Yeah, unfortunately 7.2 was branched before I landed the commit.
    - id: 2981
      author: Eric Kow
      date: "2011-09-21 17:20:34"
      content: "Nice hackathon-ing!  I tried using this with a Git version of GHC (7.3.20110909) but it seems that most of the stats are 0 (particularly the one I think I'm interested in, current_bytes_used).  Is that normal?  peak_megabytes_allocated seems to return something I can use, on the other hand."
    - id: 2982
      author: Eric Kow
      date: "2011-09-21 18:14:41"
      content: "OH! You have to use the -T RTS option (noticed in the Git log).  Beg your pardon :-)"
---

Hac Phi was quite productive (since I managed to get two blog posts out of it!) On Saturday I committed a new module `GHC.Stats` to base which implemented a modified subset of [the API I proposed previously.](http://blog.ezyang.com/2011/07/in-program-gc-stats-for-ghc/) Here is the API; to use it you’ll need to compile GHC from Git. Please test and let me know if things should get changed or clarified!

    -- | Global garbage collection and memory statistics.
    data GCStats = GCStats
        { bytes_allocated :: Int64 -- ^ Total number of bytes allocated
        , num_gcs :: Int64 -- ^ Number of garbage collections performed
        , max_bytes_used :: Int64 -- ^ Maximum number of live bytes seen so far
        , num_byte_usage_samples :: Int64 -- ^ Number of byte usage samples taken
        -- | Sum of all byte usage samples, can be used with
        -- 'num_byte_usage_samples' to calculate averages with
        -- arbitrary weighting (if you are sampling this record multiple
        -- times).
        , cumulative_bytes_used :: Int64
        , bytes_copied :: Int64 -- ^ Number of bytes copied during GC
        , current_bytes_used :: Int64 -- ^ Current number of live bytes
        , current_bytes_slop :: Int64 -- ^ Current number of bytes lost to slop
        , max_bytes_slop :: Int64 -- ^ Maximum number of bytes lost to slop at any one time so far
        , peak_megabytes_allocated :: Int64 -- ^ Maximum number of megabytes allocated
        -- | CPU time spent running mutator threads.  This does not include
        -- any profiling overhead or initialization.
        , mutator_cpu_seconds :: Double
        -- | Wall clock time spent running mutator threads.  This does not
        -- include initialization.
        , mutator_wall_seconds :: Double
        , gc_cpu_seconds :: Double -- ^ CPU time spent running GC
        , gc_wall_seconds :: Double -- ^ Wall clock time spent running GC
        -- | Number of bytes copied during GC, minus space held by mutable
        -- lists held by the capabilities.  Can be used with
        -- 'par_max_bytes_copied' to determine how well parallel GC utilized
        -- all cores.
        , par_avg_bytes_copied :: Int64
        -- | Sum of number of bytes copied each GC by the most active GC
        -- thread each GC.  The ratio of 'par_avg_bytes_copied' divided by
        -- 'par_max_bytes_copied' approaches 1 for a maximally sequential
        -- run and approaches the number of threads (set by the RTS flag
        -- @-N@) for a maximally parallel run.
        , par_max_bytes_copied :: Int64
        } deriving (Show, Read)

    -- | Retrieves garbage collection and memory statistics as of the last
    -- garbage collection.  If you would like your statistics as recent as
    -- possible, first run a 'performGC' from "System.Mem".
    getGCStats :: IO GCStats
