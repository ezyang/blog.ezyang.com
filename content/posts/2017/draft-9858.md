---
title: "Compact regions coming to GHC 8.2"
date: 2017-02-18 20:27:23
slug: 
draft: true
categories: [Miscellaneous]
math: true
---

I've spilled much ink on this blog about [Backpack](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst). Today, however, I want to tell you about another feature that is coming with GHC 8.2: **compact regions.** A compact region contains the entirety of a fully-evaluated, immutable data structure in a contiguous chunk of memory. Data inside a compact region is not traced during garbage collection. We [wrote a paper about it](http://ezyang.com/compact.html).

When would you want to use a compact region? The simplest use case is this: you have some extremely large, long-lived, pointer data structure which GHC has uselessly been tracing when you have a major collection. If you place this structure in a compact region, after the initial cost of copying the data into the region, you should see a speedup in your major GC runs.

**Garbage collection savings.** It's a little difficult to construct a compelling, small example showing the benefit, but here is a very simple case from the `nofib` test suite, the `spellcheck` program. `spellcheck` is a very simple program which reads a dictionary into a set, and then tests an input word-by-word to see if it is in the set or not (yes, it is a *very* simple spell checker):

    import System.Environment (getArgs)
    import qualified Data.Set as Set
    import System.IO

    main = do
      [file1,file2] <- getArgs
      dict <- readFileLatin1 file1
      input <- readFileLatin1 file2
      let set = Set.fromList (words dict)
      let tocheck = words input
      print (filter (`Set.notMember` set) tocheck)

    readFileLatin1 f = do
      h <- openFile f ReadMode
      hSetEncoding h latin1
      hGetContents h

Converting this program to use a compact region on the dictionary is very simple: add `import Data.Compact`, and convert `let set = Set.fromList (words dict)` to read `set <- fmap getCompact (compact (Set.fromList (words dict)))`. Breaking down the new line: `compact` takes an argument `a` which must be pure and immutable (we check this by requesting the data implement the `NFData` type class), and then copies it into a compact region. This function returns a `Compact a` pointer, which is simultaneously a handle to the compact region as well as the data you copied into it. You get back the actual `a` data that lives in the region using `getCompact`.

Using the sample `nofib` input ([words](https://github.com/ghc/nofib/blob/master/gc/spellcheck/words) and [input](https://github.com/ghc/nofib/blob/master/gc/spellcheck/input)), we can take a look at our GC stats before and after the change. To make the effect more pronounced, I've reduced the allocation area size to 256K, so that we do more major collections. Here are the stats with the original:

    1,606,462,200 bytes allocated in the heap
      727,499,032 bytes copied during GC
       24,050,160 bytes maximum residency (21 sample(s))
          107,144 bytes maximum slop
               71 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0      6119 colls,     0 par    0.743s   0.754s     0.0001s    0.0023s
    Gen  1        21 colls,     0 par    0.608s   0.611s     0.0291s    0.0582s

    INIT    time    0.000s  (  0.000s elapsed)
    MUT     time    2.012s  (  2.024s elapsed)
    GC      time    1.350s  (  1.365s elapsed)
    EXIT    time    0.000s  (  0.000s elapsed)
    Total   time    3.363s  (  3.389s elapsed)

    %GC     time      40.2%  (40.3% elapsed)

    Alloc rate    798,416,807 bytes per MUT second

    Productivity  59.8% of total user, 59.7% of total elapsed

Here are the stats with compact regions:

    1,630,448,408 bytes allocated in the heap
      488,392,976 bytes copied during GC
       24,104,152 bytes maximum residency (21 sample(s))
           76,144 bytes maximum slop
               55 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0      6119 colls,     0 par    0.755s   0.770s     0.0001s    0.0017s
    Gen  1        21 colls,     0 par    0.147s   0.147s     0.0070s    0.0462s

    INIT    time    0.000s  (  0.000s elapsed)
    MUT     time    1.999s  (  2.054s elapsed)
    GC      time    0.902s  (  0.918s elapsed)
    EXIT    time    0.000s  (  0.000s elapsed)
    Total   time    2.901s  (  2.972s elapsed)

    %GC     time      31.1%  (30.9% elapsed)

    Alloc rate    815,689,434 bytes per MUT second

    Productivity  68.9% of total user, 69.1% of total elapsed

You can see that while the version of the program with compact regions allocates slightly more (since it performs a copy on the set), it copies nearly half as much data during GC, reducing the time spent in major GCs by a factor of three. On this particular example, you don't actually save that much time overall (since the bulk of execution is spent in the mutator)--a reminder that one should always measure before one optimizes.

**Serializing to disk.** You can take the data in a compact region and save it to disk, so that you can load it up at a later point in time. This functionality is provided by `Data.Compact.Serialized`: `withSerializedCompact` will give you a series of pointers which point directly to the memory address of the beginning of the compact region (without doing any copies); you can then blast out the data using your favorite mechanism.

Right now the API is not very user friendly, but here is a fully worked example on how to save a compact region to a file, and then read it back out:

    {-# LANGUAGE ScopedTypeVariables #-}
    import System.Environment (getArgs)
    import qualified Data.Set as Set
    import System.IO
    import Data.Compact
    import Data.Compact.Serialized
    import Foreign.Ptr
    import Foreign.Storable
    import Foreign.Marshal.Alloc
    import Control.Monad

    main = do
      [dict_file, out_file] <- getArgs
      dict <- readFileLatin1 dict_file
      c <- compact (Set.fromList (words dict))
      withBinaryFile out_file WriteMode $ \h ->
        withSerializedCompact c $ \sc -> do
          -- Write out the metadata header
          hPutStorable h (serializedCompactRoot sc)
          forM_ (serializedCompactBlockList sc) $ \(ptr, l) -> do
            hPutStorable h ptr
            hPutStorable h l
          hPutStorable h nullPtr
          -- Write out the payload
          forM_ (serializedCompactBlockList sc) $ \(ptr, l) ->
            hPutBuf h ptr (fromIntegral l)

      mb_r <- withBinaryFile out_file ReadMode $ \h -> do
        -- Read out the metadata header
        root <- hGetStorable h
        let go h xs = do
                ptr <- hGetStorable h
                if ptr == nullPtr
                    then return (reverse xs)
                    else do l <- hGetStorable h
                            go h ((ptr, l):xs)
        blocks <- go h []
        let sc = SerializedCompact {
                    serializedCompactBlockList = blocks,
                    serializedCompactRoot = root
                 }
        -- Read the payload into memory
        importCompact sc $ \ptr l -> void $ hGetBuf h ptr (fromIntegral l)

      print (fmap getCompact mb_r == Just (getCompact c))

    hPutStorable :: forall a. Storable a => Handle -> a -> IO ()
    hPutStorable h a =
        alloca $ \ptr -> do
            poke ptr a
            hPutBuf h ptr (sizeOf (undefined :: a))

    hGetStorable :: forall a. Storable a => Handle -> IO a
    hGetStorable h =
        alloca $ \ptr -> do
            hGetBuf h ptr (sizeOf (undefined :: a))
            peek ptr

    readFileLatin1 f = do
      h <- openFile f ReadMode
      hSetEncoding h latin1
      hGetContents h

The key thing is that we need to write out the `SerializedCompact` data structure as a metadata header, so that GHC knows how to interpret the data in the binary region. Maybe we will make this API more user friendly prior to GHC 8.2 release.

**Where to get it.** Data.Compact is provided by a new boot library "compact"; the Haddocks are available [here](https://downloads.haskell.org/~ghc/master/libraries/html/compact/index.html)

Thanks Ian-Woo Kim for encouraging me to write this post.
