---
title: "On checked exceptions and proof obligations"
date: 2011-02-18 09:00:05
slug: on-checked-exceptions-and-proof-obligations
categories: [Software Engineering]
comments:
    - id: 1777
      author: Dave King
      date: "2011-02-18 14:48:29"
      content: |
        Hey Eric, 
        
        I'm a big fan of the blog.  I've done a lot of Java programming (a fair amount using java.reflect and a lot more not using it).
        
        It would be interesting to eliminate checked exceptions like this, but I feel that this is an edge case.  A lot of the checked exceptions in the java.reflect (MethodNotFoundException) etc, are not indicative of the usual patterns of using checked exceptions.  Exception handling in this case is a case of "stupid compiler doesn't know enough", but this hasn't been my typical experience when dealing with exceptions.
        
        IOException extending Exception (checked) forces a lot of libraries to extend IOException (for example, LockReleaseFailedException in lucene because locks are implemented as files) or wrap their own exceptions.
        
        In Clean Code, Robert Martin recommends using unchecked exceptions where possible.  I think that this ends up being the best compromise when dealing with library code.  Not fundamentally correct but the easiest answer for a lot of programming.
    - id: 1778
      author: Edward Z. Yang
      date: "2011-02-18 17:09:08"
      content: |
        Hello Dave,
        
        Thanks for your comment. Unfortunately, I don't quite understand your third paragraph: are you saying that because IOException is far too broad an exception, it's kind of useless?
        
        I do believe unchecked exceptions can play a role similar to assertions, in that we can handwave away cases we don't care about, but make sure we fail fast if there is a problem.  However, I do think that ideally we should not be making the same assertions over and over again: make it once, and then have the compiler propagate it as far as possible.
    - id: 1779
      author: Thomas
      date: "2011-02-19 07:02:13"
      content: "Could you elaborate on the connection between dependent types, checked exceptions, proofs, and the programmer's expectations about safety properties of a given (fragment of a) program (i.e. bad things don't happen)? I have the feeling there's something interesting here, but I can't seem to make it precise enough in my mind to understand exactly what point you are making."
    - id: 1780
      author: Dave King
      date: "2011-02-19 11:03:40"
      content: |
        Hey Eric,
        
        Sorry for the ambiguity.  A dependent-typed solution for making Java exceptions less hairy would work if most checked exceptions were of the form of 'you violated the expected call behavior on this object'.
        
        However, the most common checked exceptions that (in my experience) Java programmers end up dealing with are more like IOExceptions: errors that are the result of interacting with an external nondeterministic environment.
        
        You may be interested in http://www.oracle.com/technetwork/articles/entarch/effective-exceptions-092345.html -- Barry Ruzek argues that unchecked exceptions should be used for fault conditions.  (He specifically talks about IOException as something that can probably not be recovered from, but since it is checked, lots and lots of code gets written to handle it.)
        
        Unfortunately because of some library decisions most Java libraries (including those in the JDK) overuse checked exceptions.
        
        I think checked exceptions are an idea (like annotated precondition/postconditions ala ESC/Java) that hasn't meshed well with where programming has gone in the last 10 years (emphasis on code quality/readability/good control structure).  New features in Java 1.7 such as automated resource management (http://mail.openjdk.java.net/pipermail/coin-dev/2009-February/000011.html) will make it easier for programmers to clean up their code in the presence of errors, checked/unchecked.
    - id: 1781
      author: Edward Z. Yang
      date: "2011-02-19 12:24:19"
      content: |
        Hey Thomas, looks like I failed to write my post clearly. :-) I'll try to address your question directly. Checked exceptions are part of the type of a method signature: you have to propagate them through or explicitly handle them. Types are perhaps the simplest way we can prove correctness properties about our code. Dependent types are types that can depend on values, including those things that normally occur only at runtime. We can fake dependent types by considering all possible values that can show up during runtime, and then giving "sub-proofs" for each case (corresponding to catch blocks.)
        
        Dave, I see what you're talking about, and it's what I attempt to address in my second response to the skeptic. Essentially, once you convert an (specific) IOException into a runtime exception, you lose information about *under what circumstances this program will crash.* Imagine instead of writing yourself "don't use this program on a non-existent file", your compiler could figure out "this program will crash on a non-existent file; our code assumes all files exist". Actually doing this with Java's checked exceptions would be unbelievably clunky, of course.
        
        It's a bit of a nonsequitor, but automated resource management is a tremendously good idea, and I hope that some day I can actually use something like "killThread" and expect it to do the right thing. But this is a huge cultural thing, and most code doesn't expect to be unceremoniously killed at any given moment.
    - id: 1782
      author: Thomas
      date: "2011-02-19 13:44:58"
      content: "Forgive me, but it's still not clear to me. I know about checked exceptions in Java and dependent types. I think I may have even commented on a previous blog post that I like the idea of deriving implementations from specifications by proving the existence of a function that satisfies the specification. What I gather from your post is that you are talking about exceptions that happen not because something on the outside goes wrong (e.g. I/O operatings failing), but because invariants of datatypes or preconditions of methods are violated. But if we had dependent types in Java and used them properly we could rule out those kinds of behaviour, thus reducing the cases where exceptions are needed to situations in which something bad might happen on the outside. Is that roughly what you're talking about?"
    - id: 1783
      author: Edward Z. Yang
      date: "2011-02-19 18:00:32"
      content: |
        That roughly encapsulates one gist of the argument.
        
        Another way of thinking about it is, suppose that we had checked exception inference, so that we could omit any declarations or handling for checked exceptions until the top level, when they would all cause a compile error. Such a scheme would be completely useless, for a variety of reasons. 
        
        1. The exceptions wouldn't be specific enough. No one needs to know that an application might throw an IOException. Of course it might throw an IOException.
        
        2. There would be a lot of noise sorting out which exceptions we actually do want to deal with (that is, their propagation to the top-level is in error), and which ones we don't (edge cases we don't care about dealing with, including ones we think are impossible). Note that, contrary to exceptions being necessary for something "bad...happen[ing] on the outside", if we handle all possible error conditions (ha!) then we don't need to push the premises to the top level, the same way “if A then B or not B” is tautological.
        
        3. The exceptions wouldn't look anything like what you would expect from some documentation about cases in which the software fails. This is the bit I’m the most interested in when I speak of discharging the proof obligation: we make a high-level assertion of how we expect the world to work, and then we formally derive all of the day-to-day facts necessary to deal with the mundane checked exceptions real code exhibits.
        
        I apologize for being unclear! Maybe I should post a follow-up with more concrete examples.
    - id: 1786
      author: Thomas
      date: "2011-02-20 09:59:30"
      content: "No need to apologize, but I think it would be a good idea to provide examples or some hints at formal arguments. If someone talks about types in programming lanaguages, I usually want to see how they look like, what the \"shape\" of type signatures that accomplish a certain task would be. In this case I'd be interested in how the exceptions you are talking about in point (3) look like (instead of what they don't like look like), what world is formally, how those high-level assertions about its behaviour look like, and how the formal derivations of the \"day-to-day facts\" works. Looking forward to a follow-up post on these issues if you get around to it."
    - id: 21867
      author: "link salad | Free Dissociation"
      date: "2017-03-15 00:16:04"
      content: "[&#8230;] now for something completely different, my friend Ed has an interesting blog post up on checked exceptions and proof obligations. I can&#039;t count the number of times where I&#039;ve written some Java code [&#8230;]"
---

Checked exceptions are a much vilified feature of Java, despite theoretical reasons why it should be a really good idea. The tension is between these two lines of reasoning:

> Well-written programs handle all possible edge-cases, working around them when possible and gracefully dying if not. It's hard to keep track of *all* possible exceptions, so we should have the compiler help us out by letting us know when there is an edge-case that we've forgotten to handle. Thus, checked exceptions offer a mechanism of ensuring we've handled all of the edge-cases.

and

> Frequently checked exceptions are for error conditions that we cannot reasonably recover from close to the error site. Passing the checked exception through all of the intervening code requires each layer to know about all of its exceptions. The psychological design of checked exceptions encourages irresponsible swallowing of exceptions by developers. Checked exceptions don't scale for large amounts of code.

In this post, I suggest another method for managing checked exceptions: prove that the code *cannot* throw such an exception.

"Prove that the code cannot throw an exception?" you might say. "Impossible! After all, most checked exceptions come from the outside world, and surely we can't say anything about what will happen. A demon could just always pick the worst possible scenario and feed it into our code."

My first answer to the skeptic would be that there do indeed exist examples of checked exceptions that happen completely deterministically, and could be shown to be guaranteed not to be thrown. For example, consider this code in the Java reflection API:

    Object o, Field f; // defined elsewhere
    f.setAccessible(true);
    f.get(o);

The last invocation could throw a checked exception `IllegalAccessException`, but assuming that the `setAccessible` call did not fail (which it could, under a variety of conditions), this exception cannot happen! So, in fact, even if it *did* throw an `IllegalAccessException`, it has violated our programmer's expectation of what the API should do and a nice fat runtime error will let us notice what's going on. The call to `setAccessible` *discharges the proof obligation* for the `IllegalAccessException` case.

But this may just be an edge case in a world of overwhelmingly IO-based checked exceptions. So my second answer to the skeptic is that when we program code that interacts with the outside world, we often *don't* assume that a demon is going to feed us the worst possible input data. (Maybe we should!) We have our own internal model of how the interactions might work, and if writing something that's quick and dirty, it may be convenient to assume that the interaction will proceed in such and such a manner. So once we've written all the validation code to ensure that this is indeed the case (throwing a runtime exception akin to a failed assert if it's not), we once again can assume static knowledge that can discharge our proof obligations. Yes, in a way it’s a cop out, because we haven’t proved anything, just told the compiler, “I know what I’m doing”, but the critical extra is that once we’ve established our assumptions, we can prove things with them, and only need to check at runtime what we assumed.

Of course, Java is not going to get dependent types any time soon, so this is all a rather theoretical discussion. But checked exceptions, like types, *are* a form of formal methods, and even if you don’t write your application in a dependently typed language, the field can still give useful insights about the underlying structure of your application.

# Resources

The correspondence between checked exceptions and proofs came to me while listening to Conor McBride's lecture on the [Outrageous Arrows of Fortune](http://personal.cis.strath.ac.uk/~conor/GUtalk.pdf). I hope to do a write up of this talk soon; it clarified some issues about session types that I had been thinking about.

I consulted the following articles when characterizing existing views of Java checked exceptions.

- [Java's checked exceptions were a mistake by Rob Waldhoff](http://radio-weblogs.com/0122027/stories/2003/04/01/JavasCheckedExceptionsWereAMistake.html)
- [Does Java need checked exceptions? by Bruce Eckel](http://www.mindview.net/Etc/Discussions/CheckedExceptions)
