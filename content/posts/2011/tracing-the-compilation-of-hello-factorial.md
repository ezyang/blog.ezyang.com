---
title: "Tracing the compilation of Hello Factorial!"
date: 2011-04-13 12:27:18
slug: tracing-the-compilation-of-hello-factorial
categories: [GHC, Haskell]
comments:
    - id: 2045
      author: Baughn
      date: "2011-04-13 15:11:50"
      content: |
        This was brilliant.
        
        But any chance we could have a commentary on the optimized output, then? :-)
    - id: 2047
      author: fuzxxl
      date: "2011-04-13 16:05:27"
      content: |
        Thank you for this nice post. It gave me one of these enlighting moments when you suddently understand what the output of core means. I would e very happy to see some commentary on optimized output too.
        
        Idea for someone who looks for a coding project: Write a program that rewrites core in a more human-readable way.
    - id: 2049
      author: Edward Z. Yang
      date: "2011-04-13 19:11:33"
      content: |
        fuzxxl: It would be quite nice if the outputted Core was sufficiently machine friendly that we could have independent tools that parse Core. It would mean you could use GHC as the front-end, but then put your own backend, for example for compiling to JavaScript.
        
        Looks like there's demand for optimized output commentary. I'll see what I can put together.
    - id: 2050
      author: Ben Lippmeier
      date: "2011-04-13 21:34:53"
      content: "I added some flags to the current head GHC to make printing the core nicer. They're not well documented yet though. I usually use -ddump-simpl -dsuppress-coercions -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-idinfo -dsuppress-type-signatures -dppr-case-as-let -dppr-cols200. Only -dsuppress-coercions and -dsuppress-module-prefixes works in GHC 7.0.3."
    - id: 2051
      author: Anonymous
      date: "2011-04-13 23:23:47"
      content: |
        cool story bro
        
        no, really, I learned a lot. great post.
    - id: 2052
      author: sajkr
      date: "2011-04-14 02:56:06"
      content: |
        Excellent!
        
        I'm writing a paper covering the basics of graph reduction, lambda calculus, and Haskell's execution model. It's due in two days.
        
        I cannot express my thankfulness for this blog post - it's exactly what I need, exactly when I need it. I had planned to demonstrate the compilation process on a simple factorial function, and pure luck brought me here.
    - id: 2055
      author: Andrew Pennebaker
      date: "2011-04-14 04:03:20"
      content: |
        Great post as always!
        
        Simon Peyton Jones described Haskell using System F as its under-the-hood language in "A History of Haskell". Has Core replaced System F, or what?
        
        http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/index.htm
    - id: 2056
      author: Paul
      date: "2011-04-14 04:41:42"
      content: "Brilliant, thanks !"
    - id: 2057
      author: Edward Z. Yang
      date: "2011-04-14 04:56:36"
      content: "Andrew: Core is ~essentially System FC, but there are some variations so I guess we use a different name."
    - id: 2058
      author: Edward Z. Yang
      date: "2011-04-14 05:00:37"
      content: "Ben: Any chance of making a single option that wraps all of those options into one? :-)"
    - id: 2059
      author: plesn
      date: "2011-04-14 15:25:42"
      content: "I don't get the part about pattern matching in core : why is __DEFAULT case first, and why is there an additionnal argument in fail_dgt (realWorld#) ?"
    - id: 2061
      author: Edward Z. Yang
      date: "2011-04-14 18:36:00"
      content: "Good questions. The __DEFAULT case is first because it means we can easily find it (it's always the first case branch.) I'm not so sure about when we need to pass around RealWorld#, though it has to do with enforcing ordering, IIRC."
    - id: 2062
      author: Tyr
      date: "2011-04-14 22:01:47"
      content: "I'm going to have to read this again when I can dedicate more brain power to it.  But this is exactly what I want to learn.  I love compilers and Haskell and want to understand ghc and perhaps even understand it to the point where I could help with it."
    - id: 5943
      author: Jason Dagit
      date: "2013-01-26 15:47:19"
      content: |
        Ed, you probably already know this, but there are two libraries on Hackage that can parse GHC's core:
          * http://hackage.haskell.org/package/extcore
          * http://hackage.haskell.org/package/core
        
        I suspect that if your object language is strict, then translating from STG (which doesn't have an external parser) is much nicer.
    - id: 6029
      author: Nick Frisby
      date: "2013-04-11 15:56:07"
      content: |
        Well, I'm late to this party, but I recently learned the answer to plesn's question. Summary: The RealWorld token is a cheap way to express "this is not a thunk!" in the Core syntax. If mistaken for a thunk the compiler will not manipulate it in ways that risk duplicating its computation. Some of those manipulations, though, are quite useful optimizations. FYI, the CPR below stands for the Constructed Product Result, which is described in an approachable (and easily found) publication.
        
        Nice post, Ed.
        
        Relevant excerpt from DsUtils.lhs:
        
        Note [Failure thunks and CPR]
        ...
        Reason: we know that a failure point is always a "join point" and is
        entered at most once.  Adding a dummy 'realWorld' token argument makes
        it clear that sharing is not an issue.  And that in turn makes it more
        CPR-friendly.  This matters a lot: if you don't get it right, you lose
        the tail call property.  For example, see Trac #3403.
    - id: 8708
      author: "Haskell Profiling and Optimization | NP-Incompleteness"
      date: "2014-10-08 15:26:38"
      content: "[&#8230;] Upon writing this post, I stumbled into Zyang&#8217;s posts, which seem to delve into great detail on the core functionality. I didn&#8217;t have time to read, but I&#8217;ve bookmark those for future reading: Unraveling the mystery of the IO monad and Tracing the compilation of Hello Factorial!. [&#8230;]"
    - id: 13686
      author: "Thinking About Performance | Chad Austin"
      date: "2015-04-28 15:29:34"
      content: "[&#8230;] later.) Any &quot;typical&quot; CPU-bound Haskell program is going to spend most of its time in stg_upd_frame_info and stg_ap_*_info anyway. [&#8230;]"
    - id: 14526
      author: "プロファイラは本当に必須か？―パフォーマンスについて考える | コンピュータサイエンス | POSTD"
      date: "2015-06-04 05:00:45"
      content: "[&#8230;] 1つ目の出来事は、redditでBufferBuilderを発表した際に、初期のコメントで次のような質問をもらったことです。「Haskellを実行する時に、プロファイラを利用してコードのどの部分で処理が遅くなっているのか確認しましたか？」というものでした。もっともな質問ですが、私の答えは&#8221;ノー&#8221;でした。プロファイラは利用していません。なぜなら、Haskellのプロファイラで確認できる段階では既に遅すぎるからです。私はバッファを構築する効率的な方法を既に知っていました。境界チェックをしてから、保存（シングルバイトの場合）または複製（バイト列の場合）をするやり方です。これ以上の命令は意味がありません。ですから、BufferBuilderの開発にあたって最初にやったことは、ベンチマークやプロファイラの実行ではなく、生成されたアセンブリを読み込み、どのようにHaskellを命令に変換するかというメンタルモデルを構築することでした（ベンチマークは後ほど登場します）。&#8221;典型的&#8221;なCPUバウンドのHaskellプログラムは、どんなものであっても結局はstg_upd_frame_infoとstg_ap_*_infoに大半の時間を費やすこととなります。 [&#8230;]"
    - id: 20924
      author: "Dive into GHC: Pipeline | 神刀安全网"
      date: "2016-07-05 17:44:40"
      content: "[&#8230;] Edward Yang’s Blog [&#8230;]"
    - id: 22294
      author: Rune K. Svendsen
      date: "2018-03-15 03:29:25"
      content: |
        Thank you for writing  this.
        
        It would be really great if I could read the GHC Core code snippets on mobile, but unfortunately long lines are cut off.
---

It is often said that the *factorial function* is the “Hello World!” of the functional programming language world. Indeed, factorial is a singularly useful way of testing the pattern matching and recursive facilities of FP languages: we don’t bother with such “petty” concerns as input-output. In this blog post, we’re going to trace the compilation of factorial through the bowels of GHC. You’ll learn how to read Core, STG and Cmm, and hopefully get a taste of what is involved in the compilation of functional programs. Those who would like to play along with the GHC sources can check out the [description of the compilation of one module on the GHC wiki.](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain) We won’t compile with optimizations to keep things simple; perhaps an optimized factorial will be the topic of another post!

The examples in this post were compiled with GHC 6.12.1 on a 32-bit Linux machine.

# Haskell

    $ cat Factorial.hs

We start in the warm, comfortable land of Haskell:

    module Factorial where

    fact :: Int -> Int
    fact 0 = 1
    fact n = n * fact (n - 1)

We don’t bother checking if the input is negative to keep the code simple, and we’ve also specialized this function on `Int`, so that the resulting code will be a little clearer. But other then that, this is about as standard Factorial as it gets. Stick this in a file called `Factorial.hs` and you can play along.

# Core

    $ ghc -c Factorial.hs -ddump-ds

Haskell is a big, complicated language with lots of features. This is important for making it pleasant to code in, but not so good for machine processing. So once we’ve got the majority of user visible error handling finished (typechecking and the like), we desugar Haskell into a small language called Core. At this point, the program is still functional, but it’s a bit wordier than what we originally wrote.

We first see the Core version of our factorial function:

    Rec {
    Factorial.fact :: GHC.Types.Int -> GHC.Types.Int
    LclIdX
    []
    Factorial.fact =
      \ (ds_dgr :: GHC.Types.Int) ->
        let {
          n_ade :: GHC.Types.Int
          LclId
          []
          n_ade = ds_dgr } in
        let {
          fail_dgt :: GHC.Prim.State# GHC.Prim.RealWorld -> GHC.Types.Int
          LclId
          []
          fail_dgt =
            \ (ds_dgu :: GHC.Prim.State# GHC.Prim.RealWorld) ->
              *_agj n_ade (Factorial.fact (-_agi n_ade (GHC.Types.I# 1))) } in
        case ds_dgr of wild_B1 { GHC.Types.I# ds_dgs ->
        letrec { } in
        case ds_dgs of ds_dgs {
          __DEFAULT -> fail_dgt GHC.Prim.realWorld#; 0 -> GHC.Types.I# 1
        }
        }

This may look a bit foreign, so here is the Core re-written in something that has more of a resemblance to Haskell. In particular I’ve elided the binder info (the type signature, `LclId` and `[]` that precede every binding), removed some type signatures and reindented:

    Factorial.fact =
        \ds_dgr ->
            let n_ade = ds_dgr in
            let fail_dgt = \ds_dgu -> n_ade * Factorial.fact (n_ade - (GHC.Int.I# 1)) in
            case ds_dgr of wild_B1 { I# ds_dgs ->
                case ds_dgs of ds_dgs {
                    __DEFAULT -> fail_dgt GHC.Prim.realWorld#
                    0 -> GHC.Int.I# 1
                }
            }

It’s still a curious bit of code, so let’s step through it.

- There are no longer `fact n = ...` style bindings: instead, everything is converted into a lambda. We introduce anonymous variables prefixed by `ds_` for this purpose.
- The first let-binding is to establish that our variable `n` (with some extra stuff tacked on the end, in case we had defined another `n` that shadowed the original binding) is indeed the same as `ds_dgr`. It will get optimized away soon.
- Our recursive call to `fact` has been mysteriously placed in a lambda with the name `fail_dgt`. What is the meaning of this? It’s an artifact of the pattern matching we’re doing: if all of our other matches fail (we only have one, for the zero case), we call `fail_dgt`. The value it accepts is a faux-token `GHC.Prim.realWorld#`, which you can think of as unit.
- We see that our pattern match has been desugared into a case-statement on the *unboxed* value of `ds_dgr`, `ds_dgs`. We do one case switch to unbox it, and then another case switch to do the pattern match. There is one extra bit of syntax attached to the case statements, a variable to the right of the `of` keyword, which indicates the evaluated value (in this particular case, no one uses it.)
- Finally, we see each of the branches of our recursion, and we see we have to manually construct a boxed integer `GHC.Int.I# 1` for literals.

And then we see a bunch of extra variables and functions, which represent functions and values we implicitly used from Prelude, such as multiplication, subtraction and equality:

    $dNum_agq :: GHC.Num.Num GHC.Types.Int
    LclId
    []
    $dNum_agq = $dNum_agl
    *_agj :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int
    LclId
    []
    *_agj = GHC.Num.* @ GHC.Types.Int $dNum_agq
    -_agi :: GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int
    LclId
    []
    -_agi = GHC.Num.- @ GHC.Types.Int $dNum_agl
    $dNum_agl :: GHC.Num.Num GHC.Types.Int
    LclId
    []
    $dNum_agl = GHC.Num.$fNumInt
    $dEq_agk :: GHC.Classes.Eq GHC.Types.Int
    LclId
    []
    $dEq_agk = GHC.Num.$p1Num @ GHC.Types.Int $dNum_agl
    ==_adA :: GHC.Types.Int -> GHC.Types.Int -> GHC.Bool.Bool
    LclId
    []
    ==_adA = GHC.Classes.== @ GHC.Types.Int $dEq_agk
    fact_ado :: GHC.Types.Int -> GHC.Types.Int
    LclId
    []
    fact_ado = Factorial.fact
    end Rec }

Since `+`, `*` and `==` are from type classes, we have to lookup the dictionary for each type `dNum_agq` and `dEq_agk`, and then use this to get our actual functions `*_agj`, `-_agi` and `==_adA`, which are what our Core references, *not* the fully generic versions. If we hadn’t provided the `Int -> Int` type signature, this would have been a bit different.

# Simplified Core

    ghc -c Factorial.hs -ddump-simpl

From here, we do a number of optimization passes on the core. Keen readers may have noticed that the unoptimized Core allocated an unnecessary thunk whenever `n = 0`, the `fail_dgt`. This inefficiency, among others, is optimized away:

    Rec {
    Factorial.fact :: GHC.Types.Int -> GHC.Types.Int
    GblId
    [Arity 1]
    Factorial.fact =
      \ (ds_dgr :: GHC.Types.Int) ->
        case ds_dgr of wild_B1 { GHC.Types.I# ds1_dgs ->
        case ds1_dgs of _ {
          __DEFAULT ->
            GHC.Num.*
              @ GHC.Types.Int
              GHC.Num.$fNumInt
              wild_B1
              (Factorial.fact
                 (GHC.Num.-
                    @ GHC.Types.Int GHC.Num.$fNumInt wild_B1 (GHC.Types.I# 1)));
          0 -> GHC.Types.I# 1
        }
        }
    end Rec }

Now, the very first thing we do upon entry is unbox the input `ds_dgr` and pattern match on it. All of the dictionary nonsense has been inlined into the `__DEFAULT` branch, so `GHC.Num.* @ GHC.Types.Int GHC.Num.$fNumInt` corresponds to multiplication for `Int`, and `GHC.Num.- @ GHC.Types.Int GHC.Num.$fNumInt` corresponds to subtraction for `Int`. Equality is nowhere to be found, because we could just directly pattern match against an unboxed `Int`.

There are a few things to be said about boxing and unboxing. One important thing to notice is that the case statement on `ds_dgr` forces this variable: it may have been a thunk, so some (potentially large) amount of code may run before we proceed any further. This is one of the reasons why getting backtraces in Haskell is so hard: we care about where the thunk for `ds_dgr` was allocated, not where it gets evaluated! But we don’t know that it’s going to error until we evaluate it.

Another important thing to notice is that although we unbox our integer, the result `ds1_dgs` is not used for anything other than pattern matching. Indeed, whenever we would have used `n`, we instead use `wild_B1`, which corresponds to the fully evaluated version of `ds_dgr`. This is because all of these functions expect *boxed* arguments, and since we already have a boxed version of the integer lying around, there's no point in re-boxing the unboxed version.

# STG

    ghc -c Factorial.hs -ddump-stg

Now we convert Core to the spineless, tagless, G-machine, the very last representation before we generate code that looks more like a traditional imperative program.

    Factorial.fact =
        \r srt:(0,*bitmap*) [ds_sgx]
            case ds_sgx of wild_sgC {
              GHC.Types.I# ds1_sgA ->
                  case ds1_sgA of ds2_sgG {
                    __DEFAULT ->
                        let {
                          sat_sgJ =
                              \u srt:(0,*bitmap*) []
                                  let {
                                    sat_sgI =
                                        \u srt:(0,*bitmap*) []
                                            let { sat_sgH = NO_CCS GHC.Types.I#! [1];
                                            } in  GHC.Num.- GHC.Num.$fNumInt wild_sgC sat_sgH;
                                  } in  Factorial.fact sat_sgI;
                        } in  GHC.Num.* GHC.Num.$fNumInt wild_sgC sat_sgJ;
                    0 -> GHC.Types.I# [1];
                  };
            };
    SRT(Factorial.fact): [GHC.Num.$fNumInt, Factorial.fact]

Structurally, STG is very similar to Core, though there’s a lot of extra goop in preparation for the code generation phase:

- All of the variables have been renamed,
- All of the lambdas now have the form `\r srt:(0,*bitmap*) [ds_sgx]`. The arguments are in the list at the rightmost side: if there are no arguments this is simply a thunk. The first character after the backslash indicates whether or not the closure is re-entrant (r), updatable (u) or single-entry (s, not used in this example). Updatable closures can be rewritten after evaluation with their results (so closures that take arguments can’t be updateable!) Afterwards, the [static reference table](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/CAFs) is displayed, though there are no interesting static references in our program.
- `NO_CCS` is an annotation for profiling that indicates that no cost center stack is attached to this closure. Since we’re not compiling with profiling it’s not very interesting.
- Constructor applications take their arguments in square brackets: `GHC.Types.I# [1]`. This is not just a stylistic change: in STG, constructors are required to have *all* of their arguments (e.g. they are saturated). Otherwise, the constructor would be turned into a lambda.

There is also an interesting structural change, where all function applications now take only variables as arguments. In particular, we’ve created a new `sat_sgJ` thunk to pass to the recursive call of factorial. Because we have not compiled with optimizations, GHC has not noticed that the argument of `fact` will be immediately evaluated. This will make for some extremely circuitous intermediate code!

# Cmm

    ghc -c Factorial.hs -ddump-cmm

Cmm (read “C minus minus”) is GHC’s high-level assembly language. It is similar in scope to LLVM, although it looks more like C than assembly. Here the output starts getting large, so we’ll treat it in chunks. The Cmm output contains a number of data sections, which mostly encode the extra annotated information from STG, and the entry points: `sgI_entry`, `sgJ_entry`, `sgC_ret` and `Factorial_fact_entry`. There are also two extra functions `__stginit_Factorial_` and `__stginit_Factorial` which initialize the module, that we will not address.

Because we have been looking at the `STG`, we can construct a direct correspondence between these entry points and names from the STG. In brief:

- `sgI_entry` corresponded to the thunk that subtracted 1 from `wild_sgC`. We’d expect it to setup the call to the function that subtracts `Int`.
- `sgJ_entry` corresponded to the thunk that called `Factorial.fact` on `sat_sgI`. We’d expect it to setup the call to `Factorial.fact`.
- `sgC_ret` is a little different, being tagged at the end with `ret`. This is a return point, which we will return to after we successfully evaluate `ds_sgx` (i.e. `wild_sgC`). We’d expect it to check if the result is `0`, and either “return” a one (for some definition of “return”) or setup a call to the function that multiplies `Int` with `sgJ_entry` and its argument.

Time for some code! Here is `sgI_entry`:

    sgI_entry()
            { has static closure: False update_frame: <none>
              type: 0
              desc: 0
              tag: 17
              ptrs: 1
              nptrs: 0
              srt: (Factorial_fact_srt,0,1)
            }
        ch0:
            if (Sp - 24 < SpLim) goto ch2;
            I32[Sp - 4] = R1; // (reordered for clarity)
            I32[Sp - 8] = stg_upd_frame_info;
            I32[Sp - 12] = stg_INTLIKE_closure+137;
            I32[Sp - 16] = I32[R1 + 8];
            I32[Sp - 20] = stg_ap_pp_info;
            I32[Sp - 24] = base_GHCziNum_zdfNumInt_closure;
            Sp = Sp - 24;
            jump base_GHCziNum_zm_info ();
        ch2: jump stg_gc_enter_1 ();
    }

There’s a bit of metadata given at the top of the function, this is a description of the *info table* that will be stored next to the actual code for this function. You can look at `CmmInfoTable` in `cmm/CmmDecl.hs` if you’re interested in what the values mean; most notably the tag 17 corresponds to `THUNK_1_0`: this is a thunk that has in its environment (the free variables: in this case `wild_sgC`) a single pointer and no non-pointers.

Without attempting to understand the code, we can see a few interesting things: we are jumping to `base_GHCziNum_zm_info`, which is a [Z-encoded name](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames) for `base GHC.Num - info`: hey, that’s our subtraction function! In that case, a reasonable guess is that the values we are writing to the stack are the arguments for this function. Let’s pull up the STG invocation again: `GHC.Num.- GHC.Num.$fNumInt wild_sgC sat_sgH` (recall `sat_sgH was a constant 1).`base_GHCziNum_zdfNumInt_closure`is Z-encoded`base GHC.Num \$fNumInt`, so there is our dictionary function.`stg_INTLIKE_closure+137`is a rather curious constant, which happens to point to a statically allocated closure representing the number`1`. Which means at last we have`I32\[R1 + 8\]`, which must refer to`wild_sgC`(in fact`R1`is a pointer to this thunk’s closure on the stack.)  You may ask, what do`stg_ap_pp_info`and`stg_upd_frame_info`do, and why is`base_GHCziNum_zdfNumInt_closure`at the very bottom of the stack? The key is to realize that in fact, we’re placing three distinct entities on the stack: an argument for`base_GHCziNum_zm_info`, a`stg_ap_pp_info`object with a closure containing`I32\[R1 + 8\]`and`stg_INTLIKE_closure+137`, and a`stg_upd_frame_info`object with a closure containing`R1`. We’ve delicately setup a Rube Goldberg machine, that when run, will do the following things:  1. Inside`base_GHCziNum_zm_info`, consume the argument`base_GHCziNum_zdfNumInt_closure`and figure out what the right subtraction function for this dictionary is, put this function on the stack, and then jump to its return point, the next info table on the stack,`stg_ap_pp_info`.  2. Inside`stg_ap_pp_info`, consume the argument that`base_GHCziNum_zm_info`created, and apply it with the two arguments`I32\[R1 + 8\]`and`stg_INTLIKE_closure+137`. (As you might imagine,`stg_ap_pp_info`is very simple.)  3. The subtraction function runs and does the actual subtraction. It then invokes the next info table on the stack`stg_upd_frame_info`with this argument.  4. Because this is an updateable closure (remember the`u`character in STG?), will`stg_upd_frame_info`the result of step 3 and use it to overwrite the closure pointed to by`R1`(the original closure of the thunk) with a new closure that simply contains the new value. It will then invoke the next info table on the stack, which was whatever was on the stack when we entered`sgI_Entry`.  Phew! And now there’s the minor question of`if (Sp - 24 \< SpLim) goto ch2;`which checks if we will overflow the stack and bugs out to the garbage collector if so.`sgJ_entry`does something very similar, but this time the continuation chain is`Factorial_fact`to`stg_upd_frame_info`to the great beyond. We also need to allocate a new closure on the heap (`sgI_info`), which will be passed in as an argument::      sgJ_entry()             { has static closure: False update_frame: <none>               type: 0               desc: 0               tag: 17               ptrs: 1               nptrs: 0               srt: (Factorial_fact_srt,0,3)             }         ch5:             if (Sp - 12 < SpLim) goto ch7;             Hp = Hp + 12;             if (Hp > HpLim) goto ch7;             I32[Sp - 8] = stg_upd_frame_info;             I32[Sp - 4] = R1;             I32[Hp - 8] = sgI_info;             I32[Hp + 0] = I32[R1 + 8];             I32[Sp - 12] = Hp - 8;             Sp = Sp - 12;             jump Factorial_fact_info ();         ch7:             HpAlloc = 12;             jump stg_gc_enter_1 ();     }  And finally,`sgC_ret`actually does computation::      sgC_ret()             { has static closure: False update_frame: <none>               type: 0               desc: 0               tag: 34               stack: []               srt: (Factorial_fact_srt,0,3)             }         ch9:             Hp = Hp + 12;             if (Hp > HpLim) goto chb;             _sgG::I32 = I32[R1 + 3];             if (_sgG::I32 != 0) goto chd;             R1 = stg_INTLIKE_closure+137;             Sp = Sp + 4;             Hp = Hp - 12;             jump (I32[Sp + 0]) ();         chb:             HpAlloc = 12;             jump stg_gc_enter_1 ();         chd:             I32[Hp - 8] = sgJ_info;             I32[Hp + 0] = R1;             I32[Sp + 0] = Hp - 8;             I32[Sp - 4] = R1;             I32[Sp - 8] = stg_ap_pp_info;             I32[Sp - 12] = base_GHCziNum_zdfNumInt_closure;             Sp = Sp - 12;             jump base_GHCziNum_zt_info ();     }  ...though not very much of it. We grab the result of the case split from`I32\[R1 + 3\]`(R1 is a tagged pointer, which is why the offset looks weird.) We then check if its zero, and if it is we shove`stg_INTLIKE_closure+137`(the literal 1) into our register and jump to our continuation; otherwise we setup our arguments on the stack to do a multiplication`base_GHCziNum_zt_info`. The same dictionary passing dance happens. And that’s it!  While we’re here, here is a brief shout-out to “Optimised Cmm”, which is just Cmm but with some minor optimisations applied to it. If you’re *really* interested in the correspondence to the underlying assembly, this is good to look at.  ::    ghc -c Factorial.hs -ddump-opt-cmm  Assembly ------------  ::    ghc -c Factorial.hs -ddump-asm  Finally, we get to assembly. It’s mostly the same as the Cmm, minus some optimizations, instruction selection and register allocation. In particular, all of the names from Cmm are preserved, which is useful if you’re debugging compiled Haskell code with GDB and don’t feel like wading through assembly: you can peek at the Cmm to get an idea for what the function is doing.  Here is one excerpt, which displays some more salient aspects of Haskell on x86-32::      sgK_info:     .Lch9:             leal -24(%ebp),%eax             cmpl 84(%ebx),%eax             jb .Lchb             movl $stg_upd_frame_info,-8(%ebp)             movl %esi,-4(%ebp)             movl $stg_INTLIKE_closure+137,-12(%ebp)             movl 8(%esi),%eax             movl %eax,-16(%ebp)             movl $stg_ap_pp_info,-20(%ebp)             movl $base_GHCziNum_zdfNumInt_closure,-24(%ebp)             addl $-24,%ebp             jmp base_GHCziNum_zm_info     .Lchb:             jmp *-8(%ebx)  Some of the registers are pinned to registers we saw in Cmm. The first two lines are the stack check, and we can see that`%ebp`is always set to the value of`Sp`.`84(%ebx)`must be where`SpLim`; indeed,`%ebx`stores a pointer to the`BaseReg`structure, where we store various “register-like” data as the program executes (as well as the garbage collection function, see`\*-8(%ebx)`). Afterwards, a lot of code moves values onto the stack, and we can see that`%esi`corresponds to`R1`. In fact, once you’ve allocated all of these registers, there aren’t very many general purpose registers to actually do computation in: just`%eax`and`%edx`.  Conclusion -------------  That’s it: factorial all the way down to the assembly level! You may be thinking several things:  * *Holy crap! The next time I need to enter an obfuscated C contest, I’ll just have GHC generate my code for me.* GHC’s internal operational model is indeed very different from any imperative language you may have seen, but it is very regular, and once you get the hang of it, rather easy to understand.  * *Holy crap! I can’t believe that Haskell performs at all!* Remember we didn’t compile with optimizations at all. The same module compiled with`-O\`\` is considerably smarter.

Thanks for reading all the way! Stay tuned for the near future, where I illustrate action on the Haskell heap in comic book format.
