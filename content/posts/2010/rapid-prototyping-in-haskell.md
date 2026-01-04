---
title: "Rapidly prototyping scripts in Haskell"
date: 2010-10-18 09:00:08
slug: rapid-prototyping-in-haskell
categories: [Haskell, Python, SIPB]
math: true
comments:
    - id: 1304
      author: Brian Sniffen
      date: "2010-10-18 18:26:18"
      content: |
        Nice post—and it's nice to see someone else advocating Haskell, the Ultimate Scripting Language.  
        
        Surely you're not using only IO.  You're using Maybe and List as well.  In some places, I bet you're using (Monad m =&gt; m).
        
        Would you really have to use liftIO if you used a Reader?  I'd expect the MonadIO constraint and ReaderT Monad Transformer to do the lifting for you.
    - id: 1305
      author: Edward Z. Yang
      date: "2010-10-18 18:32:27"
      content: |
        Brian: Ah yes, I am. Just not monadically, and definitely not in my do blocks. :-)
        
        Any calls to functions that have IO a at the right hand side of their type signature would have to be lifted: which essentially translates into "any code that interfaces with libraries." This ends up being a lot of code, in this particular use-case.
    - id: 1306
      author: Alon
      date: "2010-10-18 18:40:44"
      content: |
        That's a cool post. I'm still playing around with haskell, haven't really tried using it (except a solver of a 100 square puzzle - which I failed to debug yet..). Trying out a code snippet of yours I couldn't remember which module to import for isPrefixOf :) So I'm not there yet, but thanks for making me realize I could try honing my haskell skills this way, by scripting, instead of python, which is my usual workhorse when bash fails.
        
        What would be really cool is if you did a screencast of a hack like this ldap thing, that would be a cool real world tutorial!
    - id: 1307
      author: Edward Z. Yang
      date: "2010-10-18 18:49:49"
      content: |
        You can always look up what module a function lives in with Hoogle or Hayoo; good style is to explicitly import functions from unusual modules and leave the common ones implicit, which is what I tried to do. isPrefixOf is, in this case, in Data.List. Writing simple command line programs is definitely a good way of honing one section of your Haskell repertoire, but it's not the only thing to work on. :-)
        
        I've never done a screencast before; I'm not really sure how suitable it is a communication medium, but I'll keep it in mind next time I have a hack session.
    - id: 1308
      author: zMan
      date: "2010-10-18 22:58:44"
      content: |
        Could you give a pointer as to how to do: "several HTTP requests in parallel."?
        
        Thanks for the post!
    - id: 1309
      author: Simon Michael
      date: "2010-10-19 00:09:17"
      content: "Great post!"
    - id: 1310
      author: Edward Z. Yang
      date: "2010-10-19 04:32:53"
      content: |
        zMan: Perhaps that's a topic worth touching on a post of itself. Here's one possible way of doing it, using Orc:
        
        <pre>import Network.HTTP
        
        import Control.Concurrent
        import Control.Concurrent.Spawn (pool)
        import Control.Exception
        import Control.Monad
        
        import Orc (runOrc, liftIO)
        
        requests = [ "http://www.yahoo.com"
                   , "http://haskell.org"
                   , "http://www.google.com"
                   , "http://www.delicious.com"
                   , "http://www.galois.com"
                   ]
        
        main = runOrc (getRequests requests f)
            where f (Left e)  = putStrLn (show e)
                  f (Right x) = print (rspHeaders x) >> putStrLn "\n"
        
        poolSize = 3
        
        getRequests requests m = do
            poolGuard <- liftIO $ pool poolSize
            msum (map (liftIO . poolGuard . f) requests)
            where f = m <=< simpleHTTP . getRequest</pre>
        
        The implementation of pool is backed by a semaphore.
    - id: 1312
      author: Johan Tibell
      date: "2010-10-19 09:17:41"
      content: "Perhaps it's time for a new getopt like library that's based on Duncan's idea of using monoids to represent command line flags."
    - id: 1313
      author: Edward Z. Yang
      date: "2010-10-19 10:28:20"
      content: "Yeah, I really do like the monoidal idea."
    - id: 1314
      author: solrize
      date: "2010-10-19 11:45:03"
      content: |
        Generally nice post, though my own experience (coming from Python to Haskell) hasn't been that great for this sort of thing.
        
        First, a minor but persistent annoyance: in both Python and Haskell, scripts often need to import a bunch of library modules.  In Python, I'd say
        
            import os, re, urllib, threading, sys ...
        
        in Haskell, I need a separate import directive for each module, so my Python scripts usually start with a few lines of imports but Haskell scripts have screens full of imports before getting to any actual code.
        
        Second, Haskell (including compiled Haskell) scripts tend to be dog slow compared to Python.  That's because while the Python interpreter proper is quite inefficient, one can usually write Python scripts to do their heavy lifting in calls to Python's standard library modules, which are carefully coded in C.  If I want to extract certain info from all the lines in a multi-gigabyte file, in Python I just iterate through the file and analyze the lines with the regexp module and it's fairly fast.  In Haskell, I usually write the script with Strings, find it unacceptably slow, rewrite with ByteString and it typically remains slower than Python even after the rewrite.  Also, for purity purposes I'm uncomfortable with lazy I/O but I don't yet understand iteratees, which in any case add much more complexity.
        
        Haskell's standard libraries are quite deficient compared to Python's, and their docs aren't as good.  If you include Hackage, stuff is still missing, and the docs are even worse.
        
        I usually write a Python script and test it on a small file (like a few kilobytes).  Once it works, I run it on bigger files (for the stuff I do, that often means multi-gigabyte files where the scripts run for hours).  I then sprinkle timestamp and logging statements into the processing loops so that I can monitor what the script is doing as it runs.  In Haskell that's not so easy unless every function gets converted to an I/O action, usually a pain.
        
        Threading and concurrency in Python is quite easy to use if you adopt an Erlang style of having threads communicate through Queues (like Haskell's Chans).  But Python's stdlib supports bounded queues that can automatically prevent producer threads from outrunning consumer threads.  You can program in a style where you handle billions of items by throwing them on a queue, and let worker threads process them.  Haskell's Chans are unbounded so if you use that style, you overrun memory.
        
        I repeatedly have the experience of writing a short script in Haskell and getting it to work on small files rather easily, but then spend hours of frustration dealing with space leaks and memory exhaustion when I run it on bigger files.  That includes the usual pile-ups of unevaluated thunks that blow the stack almost immediately, but also more insidious leaks where the memory footprint slowly grows through the processing of billions of items, so I don't notice a problem until the script has been running for a very long time.  I've sometimes been completely unable to debug these leaks even with a lot of help from #haskell.  I end up rewriting the script in python, which doesn't have this sort of problem.
        
        It would probably be helpful at some Haskell sprint for a few people to print out the Python standard library manual, and implement everything in it in Haskell.  I'd be willing to help with this (I'd enjoy a chance to do some pair programming with an experience Haskeller) but even after a few years of on-and-off Haskell experimentation, I don't seem to be able to do this type of thing myself in any reasonably productive way.
        
        At this point I think of Haskell as very interesting but almost at a dead end in some ways.  DDC looks promising as a Haskell successor and I hope it can get wider attention and/or influence future Haskell development.
    - id: 1315
      author: Edward Z. Yang
      date: "2010-10-19 12:45:48"
      content: |
        Hey solrize, thanks for the thought provoking comment. Point by point:
        
        Re import statements: Yes. Though, I'm fairly used to there being some measure of junk at the top of files (whether it's a copyright statement or license or something else) so it doesn't bother me at all.
        
        Re sifting through a single large input stream: It’s definitely a case where the “obvious” thing becomes problematic. However, I think that we should be able to get better performance than Python on the task by applying a few well defined methods, and I invite you to throw down the gauntlet with a fully fleshed out problem instance.
        
        Re docs: Sure, although I think the base documentation is actually in reasonably good shape these days. Which reminds me: I need to submit documentation patches for Orc...
        
        Re logging: Yes. Debug.Trace (which uses unsafePerformIO) might not be a bad idea. IO heavy programs which do little computation also have a relatively easy time fitting in logging statements.
        
        Re bounded queues: The great thing about Haskell is that abstraction is cheap. Here's your bounded queue:
        
        <pre>newBoundedChan n = do
          sem <- newQSem n
          queue <- newChan
          return (BoundedChan sem queue)</pre>
        
        The rest of the operations are left as an exercise to the reader. :-)
        
        Re space leaks: I don’t have a good answer here. I suspect it’s something that can be fixed with proper style, but I haven’t the slightest what that style is.
        
        Re going through the Python standard library: Definitely, and Don has suggested this before.
        
        Re DDC: It’s definitely an interesting design space, although I’m secretly hoping that GHC can steal all the good ideas. :-)
---

I’ve been having some vicious fun over the weekend hacking up a little tool called [MMR Hammer](http://github.com/ezyang/mmr-hammer) in Haskell. I won’t bore you with the vagaries of multimaster replication with Fedora Directory Server; instead, I want to talk about rapidly prototyping scripts in Haskell—programs that are characterized by a low amount of computation and a high amount of IO. Using this script as a case study, I’ll describe how I approached the problem, what was easy to do and what took a little more coaxing. In particular, my main arguments are:

1.  In highly specialized scripts, you can get away with not specifying top-level type signatures,
2.  The IO monad is the only monad you need, and finally
3.  You *can* and *should* write hackish code in Haskell, and the language will impose just the right amount of rigor to ensure you can clean it up later.

I hope to convince you that Haskell can be a great language for prototyping scripts.

*What are the characteristics of rapidly prototyping scripts?* There are two primary goals of rapid prototyping: to get it *working*, and to get it working *quickly.* There are a confluence of factors that feed into these two basic goals:

- Your requirements are immediately obvious—the problem is an exercise of getting your thoughts into working code. (You might decide later that your requirements are wrong.)
- You have an existing API that you want to use, which let’s you say “I want to set the X property to Y” instead of saying “I will transmit a binary message of this particular format with this data over TCP.” This should map onto your conception of what you want to do.
- You are going to manually test by repeatedly executing the code path you care about. Code that you aren’t developing actively will in general not get run (and may fail to compile, if you have lots of helper functions). Furthermore, running your code should be fast and not involve a long compilation process.
- You want to avoid shaving yaks: solving unrelated problems eats up time and prevents your software from working; better to hack around a problem now.
- Specialization of your code for your specific use-case is good: it makes it easier to use, and gives a specific example of what a future generalization needs to support, if you decide to make your code more widely applicable in the future (which seems to happen to a lot of prototypes.)
- You’re not doing very much computationally expensive work, but your logic is more complicated than is maintainable in a shell script.

*What does a language that enables rapid prototyping look like?*

- It should be concise, and at the very least, not make you repeat yourself.
- It should “come with batteries,” and at least have the important API you want to use.
- It should be interpreted.
- It should be well used; that is, what you are trying to do should exist somewhere in the union of what other people have already done with the language. This means you are less likely to run into bizarre error conditions in code that no one else runs.
- It should have a fast write-test-debug cycle, at least for small programs.
- The compiler should not get in your way.

*General prototyping in Haskell.* If we look at our list above, Haskell has several aspects that recommend it. GHC has a `runghc` command which allows you to interpret your script, which means for quick prototyping. Functional programming encourages high amounts of code reuse, and can be extremely concise when your comfortable with using higher-order functions. And, increasingly, it’s growing a rather large set of batteries. In the case of LDAP MMR, I needed a bindings for the OpenLDAP library, which [John Goerzen](http://hackage.haskell.org/package/LDAP) had already written. A great start.

*The compiler should not get in your way.* This is perhaps the most obvious problem for any newcomer to Haskell: they try to some pedestrian program and the compiler starts bleating at them with a complex type error, rather than the usual syntax error or runtime error. As they get more acquainted with Haskell, their mental model of Haskell’s type system improves and their ability to fix type errors improves.

The million dollar question, then, is how well do you have to know Haskell to be able to quickly resolve type errors? I argue, in the case of rapid prototyping in Haskell, *not much at all!*

One simplifying factor is the fact that the functions you write will usually *not* be polymorphic. Out of the 73 fully implemented functions in MMR Hammer, only six have inferred nontrivial polymorphic type signatures, all but one of these is only used single type context.

For these signatures, `a` is always `String`:

    Inferred type: lookupKey :: forall a.
                                [Char] -> [([Char], [a])] -> [a]

    Inferred type: lookupKey1 :: forall a.
                                 [Char] -> [([Char], [a])] -> Maybe a

`m` is always `IO`, `t` is always `[String]` but is polymorphic because it’s not used in the function body:

    Inferred type: mungeAgreement :: forall (m :: * -> *).
                                     (Monad m) =>
                                     LDAPEntry -> m LDAPEntry

    Inferred type: replicaConfigPredicate :: forall t (m :: * -> *).
                                             (Monad m) =>
                                             ([Char], t) -> m Bool

`a` here is always `(String, String, String)`; however, this function is one of the few truly generic ones (it’s intended to be an implementation of `msum` for `IO`):

    Inferred type: tryAll :: forall a. [IO a] -> IO a

And finally, our other truly generic function, a convenience debugging function:

    Inferred type: debugIOVal :: forall b. [Char] -> IO b -> IO b

I claim that for highly specific, prototype code, GHC will usually infer fairly monomorphic types, and thus you don’t need to add very many explicit type signatures to get good errors. You may notice that MMR Hammer has almost *no* explicit type signatures—I argue that for monomorphic code, this is OK! Furthermore, this means that you only need to know how to *use* polymorphic functions, and not how to write them. (To say nothing of more advanced type trickery!)

*Monads, monads, monads.* I suspect a highly simplifying assumption for scripts is to avoid using any monad besides IO. For example, the following code *could* have been implemented using the Reader transformer on top of IO:

    ldapAddEntry ldap (LDAPEntry dn attrs) = ...
    ldapDeleteEntry ldap (LDAPEntry dn _ ) = ...
    printAgreements ldap = ...
    suspendAgreements ldap statefile = ...
    restoreAgreements ldap statefile = ...
    reinitAgreements ldap statefile = ...

But with only one argument being passed around, which was essentially required for any call to the API (so I would have done a bit of `ask` calling anyway), so using the reader transformer would have probably increased code size, as all of my LDAP code would have then needed to be lifted with `liftIO`.

Less monads also means less things to worry about: you don’t have to worry about mixing up monads and you can freely use `error` as a shorthand for bailing out on a critical error. In IO these get converted into exceptions which are propagated the usual way—because they are strings, you can’t write very robust error handling code, but hey, prototypes usually don’t have error handling. In particular, it’s good for a prototype to be brittle: to prefer to error out rather than to do some operation that may be correct but could also result in total nonsense.

Hanging lambda style also makes writing out code that uses bracketing functions very pleasant. Here are some example:

    withFile statefile WriteMode $ \h ->
        hPutStr h (serializeEntries replicas)

    forM_ conflicts $ \(LDAPEntry dn attrs) ->
        putStrLn dn

Look, no parentheses!

*Reaping the benefits.* Sometimes, you might try writing a program in another language for purely pedagogical purposes. But otherwise, if you know a language, and it works well for you, you won’t really want to change unless there are compelling benefits. Here are the compelling benefits of writing your code in Haskell:

- When you’re interacting with the outside world, you will fairly quickly find yourself wanting some sort of concurrent execution: maybe you want to submit a query but timeout if it doesn’t come back in ten seconds, or you’d like to do several HTTP requests in parallel, or you’d like to monitor a condition until it is fulfilled and then do something else. Haskell makes doing this sort of thing ridiculously easy, and this is a rarity among languages that can also be interpreted.

- Because you don’t have automatic tests, once you’ve written some code and manually verified that it works, you want it to stay working even when you’re working on some other part of the program. This is hard to guarantee if you’ve built helper functions that need to evolve: if you change a helper function API and forget to update all of its call sites, your code will compile but when you go back and try running an older codepath you’ll find you’ll have a bunch of trivial errors to fix. Static types make this go away. Seriously.

- Haskell gives you really, really cheap abstraction. Things you might have written out in full back in Python because the more general version would have required higher order functions and looked ugly are extremely natural and easy in Haskell, and you truly don’t have to say very much to get a lot done. A friend of mine once complained that Haskell encouraged you to spend to much time working on abstractions; this is true, but I also believe once you’ve waded into the fields of Oleg once, you’ll have a better feel in the future for when it is and isn’t appropriate.

- Rigorous NULL handling with Maybe gets you thinking about error conditions earlier. Many times, you will want to abort because you don’t want to bother dealing with that error condition, but other times you’ll want to handle things a little more gracefully, and the explicit types will always remind you when that is possible:

      case mhost of
          (Just host) -> do
              let status = maybe "no status found" id mstatus
              printf ("%-" ++ show width ++ "s : %s\n") host status 
          _ -> warnIO ("Malformed replication agreement at " ++ dn)

- Slicing and dicing input in a completely ad hoc way is doable and concise:

      let section = takeWhile (not . isPrefixOf "profile") . tail
                  . dropWhile (/= "profile default") $ contents
          getField name = let prefix = name ++ " "
                          in evaluate . fromJust . stripPrefix prefix
                                      . fromJust . find (isPrefixOf prefix)
                                      $ section

  But at the same time, it’s not too difficult to rip out this code for a real parsing library for not too many more lines of code. This an instance of a more general pattern in Haskell, which is that moving from brittle hacks to robust code is quite easy to do (see also, static type system.)

*Some downsides.* Adding option parsing to my script was unreasonably annoying, and after staring at cmdargs and cmdlib for a little bit, I decided to roll my own with getopt, which ended up being a nontrivial chunk of code in my script anyway. I’m not quite sure what went wrong here, but part of the issue was my really specialized taste in command line APIs (based off of Git, no less), and it wasn’t obvious how to use the higher level libraries to the effect I wanted. This is perhaps witnessed by the fact that most of the major Haskell command line applications also roll their own command parser. More on this on another post.

Using LDAP was also an interesting exercise: it was a fairly high quality library that worked, but it wasn’t comprehensive (I ended up submitting a patch to support `ldap_initialize`) and it wasn’t battle tested (it had no workaround for a longstanding bug between OpenLDAP and Fedora DS—more on that in another post too.) This is something that gets better with time, but until then expect to work closely with upstream for specialized libraries.
