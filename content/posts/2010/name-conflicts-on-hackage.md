---
title: "Name conflicts on Hackage"
date: 2010-05-05 09:00:12
slug: name-conflicts-on-hackage
categories: [Haskell]
comments:
    - id: 386
      author: Ivan Lazar Miljenovic
      date: "2010-05-05 20:49:18"
      content: |
        "I also don't count modules that export everything from the global namespace because they omitted where. "
        
        As far as I know, all module declarations _must_ contain "where"; what they don't have to contain is an explicit export list.  Is that what you meant?
    - id: 387
      author: Anonymous
      date: "2010-05-05 20:55:42"
      content: |
        This is a really great idea - thanks for doing this.
        
        Is the FFI code in HackageCollision.hs simply for efficiency?
    - id: 388
      author: Edward Z. Yang
      date: "2010-05-05 22:45:53"
      content: |
        Lazar, yes that's what I mean. Whoops!
        
        Anonymous, the file reading code I cribbed from this page: http://users.aber.ac.uk/afc/stricthaskell.html#semiclosed
    - id: 389
      author: Ivan Lazar Miljenovic
      date: "2010-05-06 01:57:55"
      content: "Well, my first name is \"Ivan\", not \"Lazar\" :p (and I'm the guy that goes by \"ivanm\" on #haskell)."
    - id: 390
      author: Edward Z. Yang
      date: "2010-05-06 02:06:16"
      content: "Double-oops. :o)"
    - id: 393
      author: Roel van Dijk
      date: "2010-05-06 06:25:18"
      content: "Unicode operators are curiously missing from you overview of all infix operators. It might be because packages that use them usually also use the UnicodeSyntax language extension, which can't be parsed by haskell-src. Would be interesting to try it with the haskell-src-exts package."
    - id: 394
      author: Ivan Lazar Miljenovic
      date: "2010-05-06 08:59:40"
      content: "Wow, if you're only using haskell-src (I admit I hadn't looked at your code), I wonder what proportion of packages on Hackage you've actually analysed..."
    - id: 4064
      author: Ben Millwood
      date: "2012-09-02 19:43:58"
      content: "Seems like you've switched to HSE now, but not even that can handle CPP... I wonder if a \"stupid\" parser would not be better?"
    - id: 4065
      author: Edward Z. Yang
      date: "2012-09-02 19:46:05"
      content: "Yes, the CPP and also HSC files give HSE a bit of trouble. I also can't tell if HSE is actually picking up Unicode operators... (but I am too lazy to roll my own parser...)"
---

*Attention Conservation Notice.* Unqualified identifiers that are used the most on Hackage.

Perhaps you dread the error message:

    Ambiguous occurrence `lookup'
    It could refer to either `Prelude.lookup', imported from Prelude
                          or `Data.Map.lookup', imported from Data.Map

It is the message of the piper that has come to collect his dues for your unhygenic unqualified unrestricted module import style.

Or perhaps your a library writer and trying to think up of a new symbol for your funky infix combinator, but you aren't sure what other libraries have used already.

I took [the archive (TAR)](http://hackage.haskell.org/cgi-bin/hackage-scripts/archive.tar) of the latest Hackage packages for everything, whipped up a script to extract all unqualified names exported by public modules, and then counted up the most used.

*Disclaimer.* Data constructors and record fields, unless they were explicitly exported, are not included in this count. I also don't count modules that export *everything* from the global namespace because they omitted a list of names to export. Counts are per module, and not per package. CPP and HSC files were not counted, due to limitations of haskell-src-exts.

*Top twenty identifiers (as of September 2, 2012).*

    106 empty
    69 insert
    69 toList
    66 fromList
    56 null
    54 singleton
    44 run
    42 encode
    41 decode
    41 delete
    39 size
    37 theModule
    35 member
    32 parse
    31 get
    30 lookup
    30 union
    29 Name
    29 space
    28 Node

*Top twenty infix operators (as of September 2, 2012).*

    25 !
    19 <>
    17 <+>
    14 </>
    11 <$>
    10 //
    10 ><
     9 .:
     9 <$$>
     9 ∅
     8 &
     8 .=
     8 <?>
     8 <||>
     8 \\
     8 |>
     7 #
     7 $$
     7 *.
     7 <->

The exclamation mark has earned the reputation as an "indexing" operator, and unsurprisingly is at the top. I hear from Edward Kmett that `<>` is making its way into the base as `mappend`, which is welcome, although might suck for the other six modules which redefined it for their own nefarious purposes.

*All infix operators, sorted by usage and then lexicographically (as of September 2, 2012).*

    ! <> <+> </> <$> // >< .: <$$> ∅ & .= <?> <||> \\ |> # $$ *. <-> <. <//>
    <| <|> ==> >. ||. ∈ ∉ !! &&. ++ +++ /=. <=. =: ==. >=. ∋ ∌ ∩ ∪ .|. :->
    <: ? ∆ ∖ .&. .* .-. <&> <.> << === ?? @@ \/ ^^ |+ |- ||| ~~ !!! !> !? ##
    $+$ += +> -<- .*. .:? .<. .==. .>. /=? /\ :- :> :~> <$?> <+< <=> <=? <?
    <|?> =. ==? =~ >-> >=? >? @# ^ ~> ¬ ∘ ∧ ∨ ≡ ≢ ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊈ ⊉ !: $# $>
    $~ % %> && &&? &= ** *|* + --> ->- -| . .!= .!=. .&&. .&.? .*> .+ .++.
    .+. ... ./. ./\. .:: .<=. .=. .=> .>=. .\/. .| .||. :* :+ :. := :=: <*.
    <*> <++ <++> <..> <:> <<|> <== <|*|> =$= >+> >=> >>>= >|< ?> ?>= @@@ ^#
    ^$ ^: ^^^ |* || ||* ||+ ||? ~: ~? ≠   ≮ ≯ ⊕ ⧺ !$ !$? !. !=. !>>= #! #!!
    #~~ $ $! $$$ $$$? $$+ $$++ $$+- $$= $- $. $.// $/ $// $= $=! $? $| $~!
    %% %&& %+ %/= %: %< %<= %== %>= %|| &#& &&& &+ &. &.// &/ &// &=# &> &@
    &| * *! *& *&&&* *&* ***** ****/* ****/*** ****//* ****//*** ****|*
    ****|*** ****||* ****||*** ***/* ***/** ***/**** ***//* ***//**
    ***//**** ***|* ***|** ***|**** ***||* ***||** ***||**** **. **/* **/***
    **//* **//*** **> **|* **|*** **||* **||*** */* */** */*** */**** *//*
    *//** *//*** *//**** *<<<* *=* *=. *=>* *> *>>>* *? *@ *^ *|** *|***
    *|**** *||* *||** *||*** *||**** +% ++. ++> ++>> ++@ +/+ +: +:+ +=. +>>
    +@ +^ +| - -!- -$ -->> -/\- -: -< -<< -<=- -=. -=> ->> -?- -?-> -?> -?>>
    -@ -\/- -^ -|- -~> .! .# .$. .- .--. .->. .... ./ ./= ./=. .:. .::: .<
    .<<. .<= .== .>>. .@ .@$ .@~ .\. .|| .~ .~. / /+/ /- /. /<-. /=: />/ /^
    /| /~ /~? :*: :+: :-: :<-> :<: :<=: :<> :<~> :=+ :><: :~ <! <#$> <$| <%
    <&&> <* <+ <-$ <-- <-. <-: </=? <<! <</ <<: <<< <<? <<\ <<| <<~ <=! <=:
    <==? <=@ <=@@ <>>= <?< <??> <@ <@> <@@ <~ =$ =$$= =*= =/= =< =<< =<<!
    =<<< =<= =<>= =<? ==! =>> =~= =≪ >! >$$< >$< >*> >-- >-< >: >:> >=! >=:
    >== >===> >=>=> >=@ >=@@ >> >>-> >>. >>= >>=# >>== >>=\/ >>=|\/ >>=||
    >>=||| >>> >>@ >?> >@ >@@ >||< ?! ?+ ?/= ?: ?< ?<= ?= ?== @! @= @==? @=?
    @? @?= @?== \== ^% ^-^ ^. ^>>= ^@ ^^. |#| |$> |*| |-> |-| |. |/ |// |:
    |<- |= |=> |=| |? |@ |\ |\\ |||| ~/= ~== ~=? ~?= ~|||~ ~||~ ~|~ ~~# ~~>
    ~~? ~~~> · ·× × ×· ÷ ⇒ ⇔ ∀ ∃ ≫ ≫= ⊛ ⊥ ⊨ ⊭ ⊲ ⊳ ⋅ ⋈ ⋘ ⋙ ▷ ◁ ★ 

It's a veritable zoo! (I'm personally reminded of Nethack.)

*Source.* The horrifying code that drove this exercise can be found at [Github](http://github.com/ezyang/hackage-query). I used the following shell one-liner:

    for i in *; do for j in $i/*; do cd $j; tar xf *.tar.gz; cd ../..; done; done

to extract all of the tarballs inside the tar file.

*Postscript.* It would be neat if someone could fix the discrepancies that I described earlier and do a more comprehensive/correct search over this space.
