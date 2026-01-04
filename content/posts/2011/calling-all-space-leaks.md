---
title: "Calling all space leaks"
date: 2011-05-11 16:00:15
slug: calling-all-space-leaks
categories: [Haskell]
math: true
comments:
    - id: 2404
      author: Don Stewart
      date: "2011-05-11 19:38:46"
      content: |
        How about the little known, but somewhat surprising fact, that you can't use lazy `modify*`-style functions without leaking space.
        
        http://www.haskell.org/pipermail/haskell-cafe/2009-June/063139.html
    - id: 2405
      author: Edward Z. Yang
      date: "2011-05-11 19:41:55"
      content: |
        Hee hee hee. That’s a good one. :-)
        
        On a related note, I don’t think this code in Data.IORef.Strict actually works...
        
        <pre>atomicModifyIORef :: (NFData sa, NFData sb) => IORef sa -> (sa -> (sa, sb)) -> SIO sb
        atomicModifyIORef ref f = SIO $ do x <- IO.atomicModifyIORef ref (rnf' . f)
                                           rnf x `seq` return x
                -- since the result of 'f' is a pair which has to forced 
          where rnf' x = rnf x `seq` x</pre>
    - id: 2406
      author: elaforge
      date: "2011-05-11 21:30:50"
      content: |
        A simple one:
        
        update rec = rec { rec_value = if x then new else rec_value rec, ... other fields }
        
        The problem is that if rec_value is not forced for a long time, it builds up thunks, even if 'x' was False.  I wound up not setting rec_value at all if x was not true, but nowadays I think I'd just strictify the record fields.  It's just a case of the lazy record update leak, but I wasn't expecting it because 'x' was usually False.
    - id: 2407
      author: Tyler Breisacher
      date: "2011-05-12 04:53:31"
      content: |
        I'm still pretty new to Haskell, slowly working my way through Real World Haskell. But they do mention space leaks in that book, and I *think* this code I wrote recently is probably space-leaky because `solve` will return an expression which reduces to 1+(1+(1+(1+(1+(1+...)))...)))
        
        https://gist.github.com/968189
    - id: 2410
      author: fuzxxl
      date: "2011-05-12 15:29:03"
      content: "This should stream nicely, although it doesn't. https://gist.github.com/969253"
    - id: 2411
      author: yachris
      date: "2011-05-12 19:18:45"
      content: |
        Seems like this code:
        
        <code>
        leaky_length xs = length' xs 0
          where length' [] n = n
                length' (x:xs) n = length' xs (n + 1)
        </code>
        
        should be absolutely straightforward tail recursion... does Haskell not do tail recursion optimization?
    - id: 2412
      author: Edward Z. Yang
      date: "2011-05-12 19:20:10"
      content: "yachris: Yep, that function is tail recursive. The problem is more insidious: n + 1 is a thunk, and you may notice that n is not forced until the list is empty, so the chain of thunks builds up on the heap instead."
    - id: 2413
      author: yachris
      date: "2011-05-12 19:21:12"
      content: |
        Whoops, sorry, should have google'd first:
        
        http://stackoverflow.com/questions/412919/how-does-haskell-tail-recursion-work
    - id: 2418
      author: ivan
      date: "2011-05-13 05:03:47"
      content: |
        <code>
        f _ = (a, b)
          where
            a     = space !! 10000      -- time intensive
            b     = space !! 10001      -- time intensive
            space = [0..10001]          -- space intensive
        
        main = do
          let list = fmap f [1..]
          print $ fmap fst list       -- a is forced, not forcing b means space is not freed
          print $ list
        </code>
    - id: 2427
      author: Edward Z. Yang
      date: "2011-05-16 06:09:06"
      content: "Tyler: Your code doesn't \"space leak\" per se; the real problem is that you fail to manage a tail-call, which means that the stack will grow as you evaluate that function."
    - id: 2428
      author: Edward Z. Yang
      date: "2011-05-16 06:38:38"
      content: |
        fuzxxl: I diagnosed your space leak: the problem is that fromListWith is not strict w.r.t. the combining function. So if you swap that out with:
        
        <code>fromListWith' :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a 
        fromListWith' f xs
          = foldl' ins empty xs
          where
            ins t (k,x) = insertWith' f k x t</code>
        
        it uses constant space. Note that the original does stream nicely (the hint is that the heap profile shows lots of thunks, *not* cons cells.
    - id: 2430
      author: ivan
      date: "2011-05-16 10:45:52"
      content: "Interesting. So my http://blog.ezyang.com/2011/05/calling-all-space-leaks/#comment-2418 looks like a selector leak. Should I expect ghc to do the right strictness annotation here?"
    - id: 2431
      author: Edward Z. Yang
      date: "2011-05-16 10:48:31"
      content: "No, I actually think your leak is due to the full laziness transformation. But I haven't experimentally verified yet."
    - id: 2492
      author: ivan
      date: "2011-05-23 16:50:40"
      content: "In fact MFE transformation makes it not leak. My original problem was more complex with no MFE transformation possible. Without optimization, this simple example leaks. For every member of the `list', in the pair `(a, b)' only `a' is forced and because `b' is not, the whole intermediate datastructure (`space') has to be kept in memory."
---

I’m currently collecting non-stack-overflow space leaks, in preparation for a future post in the Haskell Heap series. If you have any interesting space leaks, especially if they’re due to laziness, send them my way.

Here’s what I have so far (unverified: some of these may not leak or may be stack overflows. I’ll be curating them soon).

    import Control.Concurrent.MVar

    -- http://groups.google.com/group/fa.haskell/msg/e6d1d5862ecb319b
    main1 = do file <- getContents
               putStrLn $ show (length $ lines file) ++ " " ++
                          show (length $ words file) ++ " " ++
                          show (length file) 

    -- http://www.haskell.org/haskellwiki/Memory_leak
    main2 = let xs = [1..1000000::Integer]
            in print (sum xs * product xs)

    -- http://hackage.haskell.org/trac/ghc/ticket/4334
    leaky_lines                   :: String -> [String]
    leaky_lines ""                =  []
    leaky_lines s                 =  let (l, s') = break (== '\n') s
                                     in  l : case s' of
                                                  []      -> []
                                                  (_:s'') -> leaky_lines s''

    -- http://stackoverflow.com/questions/5552433/how-to-reason-about-space-complexity-in-haskell
    data MyTree = MyNode [MyTree] | MyLeaf [Int]

    makeTree :: Int -> MyTree
    makeTree 0 = MyLeaf [0..99]
    makeTree n = MyNode [ makeTree (n - 1)
                      , makeTree (n - 1) ]

    count2 :: MyTree -> MyTree -> Int
    count2 r (MyNode xs) = 1 + sum (map (count2 r) xs)
    count2 r (MyLeaf xs) = length xs

    -- http://stackoverflow.com/questions/2777686/how-do-i-write-a-constant-space-length-function-in-haskell
    leaky_length xs = length' xs 0
      where length' [] n = n
            length' (x:xs) n = length' xs (n + 1)

    -- http://stackoverflow.com/questions/3190098/space-leak-in-list-program
    leaky_sequence [] = [[]]
    leaky_sequence (xs:xss) = [ y:ys | y <- xs, ys <- leaky_sequence xss ]

    -- http://hackage.haskell.org/trac/ghc/ticket/917
    initlast :: (()->[a]) -> ([a], a)
    initlast xs = (init (xs ()), last (xs ()))

    main8 = print $ case initlast (\()->[0..1000000000]) of
                     (init, last) -> (length init, last)

    -- http://hackage.haskell.org/trac/ghc/ticket/3944
    waitQSem :: MVar (Int,[MVar ()]) -> IO ()
    waitQSem sem = do
       (avail,blocked) <- takeMVar sem
       if avail > 0 then
         putMVar sem (avail-1,[])
        else do
         b <- newEmptyMVar
         putMVar sem (0, blocked++[b])
         takeMVar b

    -- http://hackage.haskell.org/trac/ghc/ticket/2607
    data Tree a = Tree a [Tree a] deriving Show
    data TreeEvent = Start String
                    | Stop
                    | Leaf String
                    deriving Show
    main10 = print . snd . build $ Start "top" : cycle [Leaf "sub"]
    type UnconsumedEvent = TreeEvent        -- Alias for program documentation
    build :: [TreeEvent] -> ([UnconsumedEvent], [Tree String])
    build (Start str : es) =
            let (es', subnodes) = build es
                (spill, siblings) = build es'
            in (spill, (Tree str subnodes : siblings))
    build (Leaf str : es) =
            let (spill, siblings) = build es
            in (spill, Tree str [] : siblings)
    build (Stop : es) = (es, [])
    build [] = ([], [])
