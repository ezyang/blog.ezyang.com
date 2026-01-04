---
title: "Haskell: Not pure enough?"
date: 2011-05-02 09:00:06
slug: haskell-not-pure-enough
categories: [Haskell]
comments:
    - id: 2293
      author: Chris Smith
      date: "2011-05-02 11:35:18"
      content: |
        We can pretty clearly dismiss #4, on the basis that there is no possible implementation of a general purpose programming language that cannot run out of memory.  Perhaps we wish that our programs would never run out of memory, but this is a pipe dream; limited resources is a necessary concession to the operational world.  Even if you insist, as in Agda or Coq, that functions be proven to be total, they can still fail due to running out of resources.  The ability to artificially limit these resources (which can happen at several layers: operating system kernel or runtime system / virtual machine) doesn't fundamentally change that.
        
        Then #1 through #3 seem to be different statements of exactly the same issue.  I'm still not clear on the distinction you make between "run time" and "compile time" schools.  Perhaps it's because my sympathies don't lie in the compile-time school... Is the claim that it's okay for different target platforms to make a difference, but it's not okay for RTS options to be the distinguishing factor?  I can think of no reasonable basis for making that distinction.
        
        I'm loosely in the run time school, I suppose, but all the while understanding that I'll eventually need to move to the mathematical school if and when it becomes common to build Haskell applications with mobile code across heterogeneous architectures.
    - id: 2295
      author: Anonymous
      date: "2011-05-02 12:02:19"
      content: "I'm wondering whether `getArgs` and `getEnvironment` should be \"pure\" values as well then, since they won't change once the program is executed..."
    - id: 2297
      author: Johan Tibell
      date: "2011-05-02 13:00:01"
      content: |
        The "This integer need not remain consistent from one execution of an application to another execution of the same application." sentence is a remnant from me using Java's hashCode documentation as a basis for the documentation for hashable.
        
        I left it there as I still haven't decided exactly which guarantees I want to make and therefore kept the sentence to discourage people from doing things like writing the hash value to a file, which might result in errors if the hash function used gives different results on different platforms e.g. due to different endianness. Right now none of the Hashable instances vary  from run to run or from compile to compile. Once we start using a better string hashing function (such as MurmurHash) some Hashable instances will give different answers on platforms with different endianess (i.e. from compile to compile).
    - id: 2298
      author: Max
      date: "2011-05-02 14:02:38"
      content: "What is an inconsistent integer? Does Gödel's Incompleteness Theorem have anything to say about them? :-&gt;"
    - id: 2300
      author: Anonymous
      date: "2011-05-02 16:22:16"
      content: |
        I'm pretty much ok with the run-time school.  I think the mathematical school has some good points, but hardware/compilers aren't advanced enough yet; we can talk when IEEE754 is of only historical interest.
        
        Also, having read some of Rob Harper's blog posts, it seems he has a real axe to grind against Haskell.  His argument against laziness via type-level naturals seems particularly disingenuous.
    - id: 2302
      author: Jake McArthur
      date: "2011-05-02 17:05:42"
      content: "The documentation for System.Info.os describes it as \"The operating system on which the program is running.\" So os is actually not permissible for the compile-time school."
    - id: 2303
      author: dude
      date: "2011-05-02 17:15:29"
      content: "wrong 'there' in the first sentence"
    - id: 2305
      author: Edward Z. Yang
      date: "2011-05-02 19:22:53"
      content: |
        Chris: Embedded application developers might consider it a bug if their programming language allows them to use up more space than they explicitly told their program. I do agree that 4 is a little strange in that a runtime setting only results in a non-termination/exception result; as Anonymous points out, if we're OK with runtime trickery, there shouldn't be any problem with making getArgs and getEnvironment pure (actually, getEnvironment is contestable, since you can modify this state.) If a pure value "dynOption_Foo" standing in for some command line option makes you uncomfortable, think of yourself as not entirely in the runtime school.
        
        Johan: Very reasonable. But consider the case where you do want to share this data across machines, and efficiency is not a concern: in this case you would want to be able to parse 64-bit hash values on a 32-bit machine, and it's a bit tricky to arrange for this. This is a very real problem when it comes to initializing cryptographic generators.
        
        Max: I don't think Godel's incompleteness theorem has very much to say on this discussion ;-)
        
        Jake: Ah, good clarification. I should update that.
    - id: 2306
      author: Eric Mertens
      date: "2011-05-02 20:35:16"
      content: |
        To answer anonymous’ question, under the current implementation neither getArgs nor getProgName have a fixed run-time return value due to the existence of the following two functions:
        
        withArgs :: [String] -&gt; IO a -&gt; IO a
        
        withProgName :: String -&gt; IO a -&gt; IO a
        
        Of course there could be an argument to remove the IO wrapper if these functions did not exist.
    - id: 2309
      author: Quentin Moser
      date: "2011-05-03 03:00:30"
      content: |
        I think there's a fourth option worth mentioning here, which I'll call the reproducibility school. Its proponents require that persistent programs (the ones that can save their state somewhere, exit, and be started again) may be suspended, restarted, possibly upgraded, without losing referential transparency. This means, for example, that:
        
        - Values that depend on RTS options might be okay; you can always
        run the program with the same options. This is subject to discussion since in the course of upgrading the program different options might become necessary.
        
        - Unsafely reading a config file at startup is probably out, unless you push on your users the requirement that the file never, ever, change.
        
        - A value that "need[s] not remain consistent from one execution of an application to another execution of the same application" is, in general, definitely out.
        
        - Values that depend on compile-time options should be ok; same caveat as RTS options though.
        
        - A value like "compiledWithGHCVersion :: String" that could change even when the program is recompiled with the same options is out.
        
        - Platform-dependent values like System.os and the size of Int depend on the kind of persistence you want; you might require that an application may be transfered to another machine simply by copying its state over (I probably would), in which case they're out.
        
        
        Since the actual requirements of the reproducibility school are hard to formulate and often subject to change, the safest bet for reproducibilists is to adhere to the mathematical camp. Still, they'll sometimes agree to compromise when there's an easy way of ensuring that a varying value doesn't actually vary throughout a particular set of application runs. (Actually I think the same applies to the distributed-systems school, which therefore shouldn't be conflated with the mathematical school.)
    - id: 2310
      author: Johan Tibell
      date: "2011-05-03 03:08:14"
      content: |
        Edward, I definitely agree there's a need for portable hashes, but the Hashable type class wasn't designed to address that problem. From the documentation: "This class exists for the benefit of hashing-based data structures."
        
        If you want portable hashes use something like SHA-1 or any hash function of the type a -&gt; ByteString.
    - id: 2313
      author: Edward Z. Yang
      date: "2011-05-03 10:13:53"
      content: |
        Quentin: I agree that reproduceability is a very good way to think about it. If you want to support some specific "distributed" style operation, it very much influences how you think about the acceptability of various things. One of the great things about pure Haskell is you can do things like this (XMonad is one notable example).
        
        Johan: Right. But this restriction does mean that there is some limitation on how Hashable may be used. I would like to be able to refer to some concrete "32-bit hash algorithm", and run it whatever architecture I'm on (even if it's less efficient.)
    - id: 2328
      author: Conal Elliott
      date: "2011-05-03 17:42:16"
      content: |
        Hi Edward. I'm delighted to see you raise these issues. I've written about them as well in http://conal.net/blog/posts/notions-of-purity-in-haskell/, where I share two principles that are fundamental to me in denotative/functional programming:
        
        * The value of a closed expression (one not containing free variables) depends solely on the expression itself -- not influenced by the dynamic conditions under which it is executed.
        
        * Every type has a precise, specific, and preferably simple denotation. If an expression e has type T, then the meaning (value) of e is a member of the collection denoted by T.
        
        These principles have implications for the examples you give. And they're much more compelling to me than the various informal ideas I hear about denotations/values being able to depend on mechanistic, non-linguistic notions like program initialization and execution architecture or OS, but not on info that changes during program execution. And it's already easy to see that a precise, simple, and machine-independent semantics will allow us to run "pure" denotative/functional programs in distributed settings, where things like OS &amp; endianness &amp; # of processors vary during a single execution.
        
        Even without foreseeing this distribution issue, these theory/elegance arguments suffice for me, as I know that theory inevitably impacts practice. As Leonardo da Vinci said, "Those who are enamored of practice without theory are like a pilot who goes into a ship without rudder or compass and never has any certainty where he is going. Practice should always be based on a sound knowledge of theory."
    - id: 2330
      author: Edward Z. Yang
      date: "2011-05-03 17:58:57"
      content: |
        Scooped! :-)
        
        You put the bit about bottom a bit more clearly than I do; I do think there is something quantitatively different about a stack overflow and a getArgs :: [String]
        
        It occurs to me that one hacky way you could get out of the Int denotation problem is to allow a "compile with precise semantics" mode that locks down all of these wishy washy machine dependent parameters. Some price to pay for efficient execution...
    - id: 2331
      author: "Tac-Tics"
      date: "2011-05-03 18:01:05"
      content: |
        If you want pure, use a real man's language, like coq.
        
        Haskell is the pragmatic language for functionalphiles.
    - id: 2340
      author: augustss
      date: "2011-05-04 12:13:05"
      content: "@Conal A worthy goal to strive for, but one we cannot quite reach for Turing complete languages since we don't have Turing machines to execute them on."
    - id: 2343
      author: Edward Z. Yang
      date: "2011-05-04 14:16:14"
      content: "augustss: I suggest rereading the difference between lack of denotation (bottom, crash, exception) versus contradictory denotation (two different values). They're quite different things, and a lot of people don't pick up on the distinction."
    - id: 2349
      author: Conal Elliott
      date: "2011-05-04 16:06:59"
      content: "What do Turing machines have to do with the goal I set out, and particularly for things like Int and os?"
    - id: 2385
      author: jensen
      date: "2011-05-07 07:58:29"
      content: |
        @Conal, I think what augustss is trying to get at is that although admirable, pretending that the real world is a mathematical, platonic realm has severe drawbacks.
        
        Take iteratees. In the platonic realm they simply don't exist. They are meant to solve the problem of scarce resources (historically file handles). Economics is a non-concept in your utopia.
        
        I recall that you asked the question at some point about the denotation of iteratees. How about a functional quid pro quo? You give me *your* denotation of allocating/freeing scarce resources, and I'll give you a denotation of iteratees.
    - id: 2390
      author: Conal Elliott
      date: "2011-05-08 21:01:15"
      content: |
        jensen wrote:
        
        &gt; You give me *your* denotation of allocating/freeing scarce resources, and I’ll give you a denotation of iteratees.
        
        Asking this question is like asking for a denotation of garbage collection or of thunk updating in lazy data structures. Instead, we benefit from having these denotationally complex (and thus difficult to reason with) techniques stay out of the denotation and instead exist rather purely in an *implementation of a simple denotational model*. Thanks to this discipline of separating semantics from implementation (denotation from operation), we get both efficiency (thanks to clever implementation) *and* amenability to precise reasoning about correctness (thanks to simple semantics).
        
        &gt; Take iteratees. In the platonic realm they simply don’t exist. They are meant to solve the problem of scarce resources ....
        
        If I understand your assumption here, it's that efficiency-motivated *abstractions* have no corresponding semantics (existence in a platonic realm). Which surprises me. When I program, and especially when I optimize an implementation, I aim for correctness in addition to efficiency. And "correctness" is meaningful (beyond hand-waving) exactly because the abstraction does have a semantics that is defined independently of the evolving implementation.
        
        Yes, iteratees were motivated largely by implementation concerns (though also by semantic concerns with lazy I/O). Many of the libraries I've developed over the last few decades were also similarly motivated by efficiency issues. And my goal is always to obtain efficiency while *simultaneously* providing a semantically simple &amp; rigorous abstraction. For instance, I've implemented a few compilers for graphics, including two generating code that runs insanely fast on GPUs. Why a GPU-targeted compiler instead of a CPU-based interpreter? Speed! And I did so with beautiful, semantically simple &amp; rigorous platonic abstractions. This combination is my utopia.
        
        As another example of combining efficiency and simple, precise semantics, consider memoization. I've written several blog posts on how beautifully memoization can be understood and composed. (See http://conal.net/blog/tag/memoization/.) And what motivates memoization? Efficiency (eliminating redundant computation)! Again, this efficiency is achieved while precisely &amp; correctly supporting a very simple denotation.
    - id: 3871
      author: newbiehaskeller
      date: "2012-07-21 12:50:39"
      content: |
        I think that there is no such a thing as "pure" functional language. All the these terms are just some glorious terms to make impression nothing more. For example when you write putStrLn "Hello world" then the "pure" Haskell calls (through many more calls)
         fdWriteBuf :: Fd
                            -&gt; Ptr Word8    -- ^ Memory containing the data to write
                            -&gt; ByteCount    -- ^ Maximum number of bytes to write
                            -&gt; IO ByteCount -- ^ Number of bytes written
        which in turn calls the impure and sinful write function of the C Runtime.
    - id: 3872
      author: Edward Z. Yang
      date: "2012-07-21 17:26:10"
      content: "newbiehaskeller: I think you slightly miss the point of the post: we're making no claims about purity inside the IO monad; the question here is how pure the language is, even when we're not in IO."
    - id: 26524
      author: Anonymous
      date: "2021-07-26 03:24:04"
      content: "#4 is a necessity for Turing complete programming languages. However, for certain non-Turing complete programming languages, it is possible to compute the max size needed for the stack and memory during compilation. But these languages could not be considered general-purpose by most definitions."
---

It is well known that `unsafePerformIO` is an evil tool by which impure effects can make their way into otherwise pristine Haskell code. But is the rest of Haskell really that pure? Here are a few questions to ask:

1.  What is the value of `maxBound :: Int`?
2.  What is the value of `\x y -> x / y == (3 / 7 :: Double)` with `3` and `7` passed in as arguments?
3.  What is the value of `os :: String` from `System.Info`?
4.  What is the value of `foldr (+) 0 [1..100] :: Int`?

The answers to each of these questions are ill-defined—or you might say they’re well defined, but you need a little extra information to figure out what the actual result is.

1.  The Haskell 98 Report guarantees that the value of `Int` is at least `-2^29` to `2^29 - 1`. But the precise value depends on what implementation of Haskell you’re using (does it need a bit for garbage collection purposes) and whether or not you’re on a 32-bit or 64-bit system.
2.  Depending on whether or not the excess precision of your floating point registers is used to calculate the division, or if the IEEE standard is adhered to, this equality may or may not hold.
3.  Depending on what operating system the program is run on this value will change.
4.  Depending on the stack space allotted to this program at runtime, it may return a result or it may stack overflow.

In some respects, these constructs break referential transparency in an interesting way: while their values are guaranteed to be consistent during a single execution of the program, they may vary between different compilations and runtime executions of our program.

Is this kosher? And if it’s not, what are we supposed to say about the semantics of these Haskell programs?

The topic came up on `#haskell`, and I and a number of participants had a lively discussion about the topic. I’ll try to distill a few of the viewpoints here.

- The *mathematical school* says that all of this is very unsatisfactory, and that their programming languages should adhere to some precise semantics over all compilations and runtime executions. People ought to use arbitrary-size integers, and if they need modular arithmetic specify explicitly how big the modulus is (`Int32`? `Int64`?) `os` is an abomination that should have been put in the `IO` sin bin. As tolkad puts it, “Without a standard you are lost, adrift in a sea of unspecified semantics. Hold fast to the rules of the specification lest you be consumed by ambiguity.” Limitations of the universe we live in are something of an embarrassment to the mathematician, but as long as the program crashes with a nice *stack overflow* they’re willing to live with a partial correctness result. An interesting subgroup is the *distributed systems school* which also care about the assumptions that are being made about the computing environment, but for a very practical reason. If multiple copies of your program are running on heterogeneous machines, you better not make any assumptions about pointer size on the wire.
- The *compile time school* says that the mathematical approach is untenable for real world programming: one should program with compilation in mind. They’re willing to put up with a little bit of uncertainty in their source code programs, but all of the ambiguity should be cleared up once the program is compiled. If they’re feeling particularly cavalier, they’ll write their program with several meanings in mind, depending on the compile time options. They’re willing to put up with stack overflows, which are runtime determined, but are also a little uncomfortable with it. It is certainly better than the situation with `os`, which could vary from runtime to runtime. The mathematicians make fun of them with examples like, “What about a dynamic linker or virtual machine, where some of the compilation is left off until runtime?”
- The *run time school* says “Sod referential transparency across executions” and only care about internal consistency across a program run. Not only are they OK with stack overflows, they’re also OK with command line arguments setting global (pure!) variables, since those don’t change within the executions (they perhaps think `getArgs` should have had the signature `[String]`, not `IO [String]`), or variables that unsafely read in the contents of an external data file at program startup. They [write things in docs](http://hackage.haskell.org/packages/archive/hashable/1.1.1.0/doc/html/Data-Hashable.html) like “This integer need not remain consistent from one execution of an application to another execution of the same application.” Everyone else sort of shudders, but it’s a sort of guilty pleasure that most people have indulged in at some point or another.

So, which school are you a member of?

*Postscript.* Since Rob Harper has recently posted another [wonderfully iconoclastic blog post](http://existentialtype.wordpress.com/2011/05/01/of-course-ml-has-monads/), and because his ending remarks are tangentially related to the topic of this post (purity), I thought I couldn’t help but sneak in a few remarks. Rob Harper states:

> So why don’t we do this by default? Because it’s not such a great idea. Yes, I know it sounds wonderful at first, but then you realize that it’s pretty horrible. Once you’re in the IO monad, you’re stuck there forever, and are reduced to Algol-style imperative programming. You cannot easily convert between functional and monadic style without a radical restructuring of code. And you inevitably need unsafePerformIO to get anything serious done. In practical terms, you are deprived of the useful concept of a benign effect, and that just stinks!

I think Harper overstates the inability to write functional-style imperative programs in Haskell (conversions from functional to monadic style, while definitely annoying in practice, are relatively formulaic.) But these practical concerns do influence the day-to-day work of programmers, and as we’ve seen here, purity comes in all sorts of shades of gray. There is design space both upwards and downwards of Haskell’s current situation, but I think to say that purity should be thrown out entirely is missing the point.
