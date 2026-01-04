---
title: "Don't Repeat Yourself is context dependent"
date: 2010-10-22 09:00:52
slug: dry-is-context-dependen
categories: [Personal, Software Engineering]
comments:
    - id: 1327
      author: gasche
      date: "2010-10-22 12:15:24"
      content: |
        The bad thing about repetition in software is not repetition per se, it's the creation of implicit constraints in the code. When you carelessly copy/paste a given piece of code, you create an implicit dependency between the two positions : when changing one, you should also change the other.
        
        This decreases maintainability, increase the cognitive load on the programmer (you cannot make local edit anymore, you have to worry about "were should I reflect the change ?"), and is certainly the *best* way, in my experience, to introduce subtle bugs in your software.
        
        It's much better if the relation between the two places is implicit. The best situation is when you use an intra-linguistic mechanism to ensure that, in case you change one of the parts, you'll be somehow reminded to change the other. For example, it is not a problem (re. code duplication) to use a possibly long, informative variable name in a given block of code. Of course you duplicate some information (you tell numerous time that this is the `min_pos`, instead of just calling it `p`), but in any decent programming language a change in one of the occurences would result in unbound variable error.
        If the particular kind of duplication cannot be maintained by intra-linguistic means, you can also use a less mechanic safeguard, such as a code comment "Please remember to also change this file ..." at both places. It's much less comfortable, but still infinitely better than an unspoken dependency.
        
        From that point of view, you can actually also deduce that code redundancy is not always harmful : that two piece of code looks the same does *not* mean that they are implicitly synchronized. For example, you could describe a world where two different persons happen to have the same age. You will have something like `let age_X = 153 ...` and, later in the code, `let age_Y = 153 ....`. It certainly doesn't make sense to try to factorize this out, writing `let age_of_X_and_Y = 153 and age_X = age_of_X_and_Y and age_Y ...`.
        
        There is something more dynamic to duplication/repetition that simply "look for similar pattern in the data (code, rules)". It depends on the history of the data. When you consciously copy data to another place, you're almost always creating bad redundancy. When two identical data come from different paths, it's generally not a good idea to factorize them (because they'll probably diverge again later).
        
        
        Your particular example is not necessarily an instance of code duplication. Are the "full rules" and "differential rules" maintained separately, or can one of them be automatically generated from the other ? If the rules of the general game were to change, would you automatically want to update your derived/specialized rule ?
    - id: 1330
      author: Edward Z. Yang
      date: "2010-10-22 18:04:07"
      content: |
        Hello gasche,
        
        I agree with most of what you said: if we can replace duplication with simply a reference to some centralized location of the information, maintainability improves. But I also think when we make things implicit simplicity can suffer (though clarity can improve), and there’s a definite tension there. Ideally we don’t duplicate data and have the computer assist our understanding, but this is quite hard and tricky to do right, and Java IDEs usually take the opposite approach.
        
        For these game rules, we could automatically generate differential rules with ``diff -u``, but the resulting output would not be very user friendly. The best way is probably to write out the errata in English, which with our current NLP abilities is not automatable and introduces duplication. However, since games are usually a “run a few times” deal, and aren’t interested in tracking upstream changes to the original rules document, this duplication does not result in a maintainability problem. This is one of the reasons why I used it as an example: it’s a really clear cut case where duplication is OK.
    - id: 1331
      author: Matt Hellige
      date: "2010-10-22 20:11:40"
      content: |
        Of course, it would in many cases be more preferable not to have to choose one or the other at all, but rather to choose an orthogonal view (no repetition) and a linear view (everything inlined), since both have clear pros and cons. This is, of course, something that we badly miss in virtually all programming environments.
        
        But, more concretely, you claim: "Java’s insistence on writing things out means it’s difficult for programmers to make a big mess." This is complete nonsense. Java programmers routinely make absolutely enormous messes. On the other hand, it does make it difficult to make a mess that is small, but severe. But is this really such a great thing?
        
        After all, having to read 5,000 lines of code to determine that a project is a mess is far more frustrating than being able to tell after 50...
    - id: 1332
      author: Edward Z. Yang
      date: "2010-10-22 20:32:33"
      content: "I’m not particularly interested in defending the Java example (I don’t even like Java!) so I’ve reworded it."
    - id: 1340
      author: Brian Sniffen
      date: "2010-10-24 17:59:35"
      content: |
        Don't repeat yourself, but it's fine to make others repeat themselves.  For example, tagged serializations (e.g., XML) are much safer than untagged binary serialization (ASN.1 DER).  We're often willing to pay the cost of a much more expensive parser to be sure that we're reading a self-describing message.
        
        It's fine to make the computer repeat itself.  Erlang's very-efficient VM copies data from thread to thread with each message—turns out to be easier than handling cross-thread garbage collection.
        
        Similarly, the best format for rules might be the standard rules document with game-specific changes... and marginalia noting unusual changes.  Humans don't read documents in sequence; they jump and skip and scan.  \marginpar is your friend.  Well.  Not actually your friend, since each marginpar is a float and they tend to bunch up and wreck the document.  But marginpar is useful.
    - id: 1383
      author: Fritz Ruehr
      date: "2010-10-31 03:08:54"
      content: |
        As someone who teaches programming, I often present the development of a program to my students. In this context, I often see exactly the kind of tension you're talking about here: when students first see the program, they need to see it in a form with a lot of the "repetition" still in place. On the other hand, I want to "bring them along for the ride" and show them how I re-factored the design and the code to generalize and thus cut out some of the repetition. 
        
        I've done some (very limited so far) experiments with sequences of side-by-side, before-and-after "panels" of code: the left side shows the original form (relatively speaking), the right side the modified form. Color highlighting and such helps draw out the differences (a la some implementations of diff utilities). Multiple successive pages show how several stages of re-factoring proceed towards a final version.
        
        Students have reacted very well to this sort of presentation, but I can imagine using it even (especially?) for larger, real-world programs: we keep a set of successive "diffs", along with appropriate documentation (in an extreme version, perhaps even something like Knuth's literate programming). That way, newcomers and old hands alike could approach the code in a way which supports their differing needs: you can look at it in a naive form, full of redundancies, or see it in a final form, highly abstracted.
        
        It's trickiest when the actual functionality (or types, or specification, etc.) of the code changes along the way: obviously the old code can't quite so easily be seen as what is actually there in the final version. 
        
        But when the type, spec., intent, etc. remains the same, you can even imagine that the naive code is what's running, rather then the refined version.
        
        (My pedagogical example along these lines are in Haskell, free of side-effects, and thus especially easy to imagine being substituted one for the other.)
---

I am a member of a group called the [Assassins’ Guild](http://web.mit.edu/assassin/www/). No, we don’t kill people, and no, we don’t play the game Assassin. Instead, we write and run competitive live action role-playing games: you get some game rules describing the universe, a character sheet with goals, abilities and limitations, and we set you loose for anywhere from four hours to ten days. In this context, I’d like to describe a situation where applying the rule [Don’t Repeat Yourself](http://en.wikipedia.org/wiki/Don't_repeat_yourself) can be harmful.

The principle of *Don’t Repeat Yourself* comes up in a very interesting way when game writers construct game rules. The game rules are rather complex: we’d like players to be able to do things like perform melee attacks, stab each other in the back, conjure magic, break down doors, and we have to do this all without actually injuring anyone or harming any property, so, in a fashion typical of MIT students, we have “mechanics” for performing these in-game actions (for example, in one set of rules, a melee attack can be declared with “Wound 5”, where 5 is your combat rating, and if another individual has a CR of 5 or greater, they can declare “Resist”; otherwise, they have to role-play falling down unconscious and bleeding. It’s great fun.) Because there are so many rules necessary to construct a reasonable base universe, there is a vanilla, nine-page rules sheet that most gamewriters adapt for their games.

Of course, the rules aren’t always the same. One set of GMs (the people who write and run the game) may decide that a single CR rating is not enough, and that people should have separate attack and defense ratings. Another set of GMs might introduce robotic characters, who cannot die from bleeding out. And so forth.

So when we give rules out to players, we have two possibilities: we can repeat ourselves, and simply give them the full, amended set of rules. Or we can avoid repeating ourselves, and give out the standard rules and a list of errata—the specific changes made in our universe. We tend to repeat ourselves, since it’s easier to do with our game production tools. But an obvious question to ask is, which approach is better?

The answer is, of course, *it depends.*

- Veteran players who are well acquainted with the standard set of rules don’t need the entire set of rules given to them every time they play a game; instead, it would be much easier and more efficient for them if they were just given the errata sheet, so they can go, “Oh, hm, that’s different, ok” and go and concoct strategies for this altered game universe. This is particularly important for ten-days, where altered universe rules can greatly influence plotting and strategy.
- For new players who have never played a game before, being given a set of rules and then being told, “Oh, but disregard that and that and here is an extra condition for that case” would be very confusing! The full rules, repeated for the first few times they play a game, is helpful.

I think this same principle applies to *Don’t Repeat Yourself* as applied in software development. It’s good and useful to adopt a compact, unique representation for any particular piece of code or data, but don’t forget that a little bit of redundancy will greatly help out people learning your system for the first time! And to get the best of both worlds, you shouldn’t even have to repeat yourself: you should make the computer do it for you.

*Postscript.* For the curious, here is a [PDF of the game rules](http://web.mit.edu/~ezyang/Public/dangerous-scenario.pdf) we used for a game I wrote in conjunction with Alex Gurany and Jonathan Chapman, *The Murder of Jefferson Douglass* (working name *A Dangerous Game*).

*Postscript II.* When has repeating yourself been considered good design?

- Perl wants programmers to have to say as little as possible to get the job done, and this has given it a reputation as a “write only language.”
- Not all code that looks the same should be refactored into a function; there should be some logical unity to what is factored out.
- Java involves writing copious amounts of code: IDEs generate code for `hashCode` and `equals`, and you possibly tweak it after the fact. Those who like Java controversially claim that this prevents Java programmers from doing too much damage (though some might disagree.)
- When you write essays, even if you’ve already defined a term fifty pages ago, it’s good to refresh a reader’s memory. This is especially true for math textbooks.
- Haskell challenges you to abstract as much mathematically sound structure as possible. As a result, it makes people’s heads hurt, leads to combinator zoos up to the wazoo. But it’s also quite beneficial for even moderately advanced users.

Readers are encouraged to come up with more examples.
