---
title: "Design Patterns in Haskell"
date: 2010-05-03 09:00:32
slug: design-patterns-in-haskel
categories: [Haskell, Programming]
comments:
    - id: 371
      author: Tom Schrijvers
      date: "2010-05-03 09:11:22"
      content: |
        Edward,
        
        Regarding inheritance (not subtyping), William Cook's model of inheritance using open recursion works quite well in Haskell. Our AOSD paper on EffectiveAdvice (http://tomschrijvers.blogspot.com/2009/09/effectiveadvice-aop-mixin-inheritance.html) illustrates how it is used for modelling advice.
        
        Cheers,
        
        Tom
    - id: 372
      author: Jason Orendorff
      date: "2010-05-03 11:58:54"
      content: |
        The Haskell counterpart to iterators is simply the list.
        
        Go4 says the purpose of an iterator is to "Provide a way to access the elements of an aggregate object sequentially without exposing its underlying representation." In other words, iterators are a sweet API through which sequence-consumers can get data from sequence-producers (algorithms or data structures) without exposing any of the producers' implementation details.
        
        In Haskell, the list is this API. Thanks to laziness, list elements can be either stored or computed on demand, just as with iterators in OO languages.
    - id: 373
      author: Brian Sniffen
      date: "2010-05-03 12:43:03"
      content: "I feel like the purpose of the 'Factory' pattern—to provide a smarter constructor than the language's new-object operator—is often met in Haskell with smart constructors."
    - id: 374
      author: Esa Pulkkinen
      date: "2010-05-03 13:10:41"
      content: The Haskell analog of Visitor is definitely the fold family of functions.
    - id: 375
      author: Edward Z. Yang
      date: "2010-05-03 13:26:04"
      content: "Jason and Brian, I've updated those entries with your comments. Tom, it might be an interesting exercise to do a follow up post on \"How to make yourself an Object Oriented system in a Functional Programming language.\" I've updated \"there's no such thing as inheritance\" to something a little weaker: \"there's no built-in inheritance.\""
    - id: 376
      author: Stephen Tetley
      date: "2010-05-03 15:06:44"
      content: |
        Lazy lists or even fmap doesn't really capture the iterator pattern, see this paper:
        
        Jeremy Gibbons and Bruno César dos Santos Oliveira 
        http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
        
        
        Some more functional analogues of oo-patterns in the 'Origami' style are detailed here:
        
        Design Patterns as Higher-Order Datatype-Generic Programs
        http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/hodgp.pdf
    - id: 377
      author: Chuck
      date: "2010-05-03 15:20:43"
      content: |
        I'm not sure all these patterns really map all that cleanly, and certainly not in such a cut-and-dried fashion.  For instance, the entry on Singleton that says "there is no global state except in a monad" strikes me as just plain wrong, or at least so handwavey as to be useless.  The fact that there isn't anything like object identity in Haskell makes lifecycle patterns like Singleton completely _alien_, and comparing them to something different brings to mind the old saw of "if my Aunt had wheels".
        
        Visitor is in fact an adaption from functional programming, which just does it more naturally.  You can reduce it to something as abstract as folds, but as far as real-world use goes, something like Scrap Your Boilerplate (or some modern adaption thereof like uniplate) would be more comparable.
    - id: 378
      author: Edward Z. Yang
      date: "2010-05-03 15:33:06"
      content: |
        Chuck, they most certainly do not! A more useful way of looking at it is, as I've said already on a Reddit comment, is that when an OOP programmers says, "I want to do this, and would have achieved it with FOO pattern in an OOP language; how do I do this in an FP language?" we can achieve a useful mapping between the two.
        
        When a pattern is completely alien in Haskell, I want to give an argument for why that's the case.
    - id: 380
      author: Keith Braithwaite
      date: "2010-05-03 17:48:00"
      content: |
        Nice, and very useful to me in understanding Haskell better as I come from an OO background.
        
        Not what I was hoping for, though. What I was hoping for was the design patterns used by Haskell programmers to write idiomatic Haskell.
    - id: 381
      author: Edward Z. Yang
      date: "2010-05-03 18:11:14"
      content: "Keith, I agree that the title is a little ambiguous (Design Patterns should be italicized.) Actual Haskell design patterns also exist, maybe HaskellWiki is one place to start looking for them."
    - id: 383
      author: Pseudonym
      date: "2010-05-04 03:47:13"
      content: |
        I've had to do this more than once:
        
        {-# NOINLINE internalThing #-}
        internalThing :: IORef Thing
        internalThing = unsafePerformIO (newIORef buildThing)
        
        This reminds me of a singleton.
    - id: 384
      author: Eyal Lotem
      date: "2010-05-04 05:52:13"
      content: |
        Pseudonym: "Had to do this" is probably an exaggeration.
        
        I've always managed without such globals, and almost without IORefs too.
    - id: 4295
      author: Anonymous
      date: "2012-10-03 13:28:24"
      content: |
        Isn´t the Composite Pattern obsolete in Haskell? Can´t one just write an type class instance for lists (or some other container, if it´s more appropriate)?
        
        &gt; instance (Class a) =&gt; Class [a] where ...
        
        Then one can use a list of xs the same way as a single x. Isn´t that the point of Composite?! This is also more orthogonal than the Composite, because there is no need to write a special container.
    - id: 4296
      author: Edward Z. Yang
      date: "2012-10-03 14:43:42"
      content: "Anonymous: Yes, type-classes can also be used in a similar fashion. However, if you can get all of your compositions into a single type, a normal data type can be much more convenient, since it is closed."
    - id: 4352
      author: Johannes Röhl
      date: "2012-10-12 14:39:41"
      content: |
        In my opinion the Visitor description, don`t offer what the visitor pattern does. The Wikipedia article says: "In essence, the visitor allows one to add new virtual functions to a family of classes without modifying the classes themselves;". So the visitor allows dispatching in the presence of "subtyping", i.e. the fact that in OO languages a variable of type A could be indeed an instance of the base class A or one of its subclasses. However I don`t see how your recommendations could help to solve such dispatching problems.
        
        Actually Haskell "as is" supports no subtyping. So, is there no need for the visitor in Haskell? I think it could be helpful, if someone implements subtyping manually (it is for instance useful for heterogeneous containers). However it doesn`t make much sense to discuss the visitor in Haskell without discussing how subtyping could be implemented (or avoided).
        
        I have converted the car elements example from Wikipedia to Haskell [1] (consider its comments in the header). There I emulate subtyping with existential types. I hope it might be useful for someone, who wants to know, how the visitor could be implemented in Haskell.
        
        [1]: https://gist.github.com/3879487
    - id: 4353
      author: Edward Z. Yang
      date: "2012-10-12 14:43:49"
      content: "While subtyping is certainly an elaboration of the basic visitor pattern, my experience is that when the visitor pattern is initially taught, it is most naturally applied in situations where the corresponding functional program would have had an algebraic data type which was pattern matched over (each visitor corresponds to a corresponding case analysis).  In the case that multiple such constructors should be treated equivalently, one could push those constructors inside (with a new data type), or perhaps use a view pattern (though that might make the exhaustiveness checker.)"
    - id: 4772
      author: "OCTO talks ! &raquo; Design Patterns : Saison 2"
      date: "2012-11-16 02:59:26"
      content: "[...] Edward Z. Yang : Liste complète des patterns du GoF en Haskell blog.ezyang.com/2010/05/design-patterns-in-haskel [...]"
    - id: 5983
      author: "Gang of Four Patterns With Type-Classes and Implicits in Scala (Part 2) | Statically Typed"
      date: "2013-03-24 22:44:54"
      content: "[...] we can mitigate the need and boilerplate required for the adapter pattern. In FP languages the adapter morphs into currying, function composition and lifting. This change in dynamic is important to realize, as [...]"
    - id: 12318
      author: "関数型デザインパターンのプレゼン動画をまとめてみた | Futurismo"
      date: "2015-01-24 10:24:28"
      content: "[&#8230;] Haskell での置き換え Design Patterns in Haskell : Inside 206-105 [&#8230;]"
    - id: 22283
      author: "Transforming enterprise integration with reactive streams | Vedalgo"
      date: "2018-03-07 12:35:22"
      content: "[&#8230;] or Haskell, can to a large extent be replaced by simple function composition. As an example, see this article. [&#8230;]"
    - id: 22286
      author: "Transforming enterprise integration with reactive streams &#8211; Cloud Data Architect"
      date: "2018-03-08 01:03:56"
      content: "[&#8230;] or Haskell, can to a large extent be replaced by simple function composition. As an example, see this article. [&#8230;]"
---

*Attention Conservation Notice.* A listing of how Gang of Four design patterns might be equivalently implemented in Haskell. A phrasebook for object-oriented programmers dealing with functional programming concepts.

In their introduction to seminal work *Design Patterns*, the Gang of Four say, "The choice of programming language is important because it influences one's point of view. Our patterns assume Smalltalk/C++-level language features, and that choice determines what can and cannot be implemented easily. If we assumed procedural languages, we might have included design patterns called 'Inheritance,' 'Encapsulation,' and 'Polymorphism.'"

What is easy and what is hard to implement in a functional programming language? I decided to revisit all 23 original Gang of Four design patterns under that lense. My hope is that these results will be useful to Object Oriented Programmers seeking to learn the ways of Functional Programming.

[Strategy](http://en.wikipedia.org/wiki/Strategy_pattern). *First class functions and lambdas.* Any extra data that might be placed as class members is traditionally implemented using closures (which stash the data in a lambda function's environment) or currying (which create implicit closures for function's arguments). Strategies are also powerful because they are polymorphic; type synonyms for function types can play a similar role. Java has recognized anonymous functions as a good idea, and have added facilities for anonymous classes, which are frequently used in this capacity.

[Factory Method](http://en.wikipedia.org/wiki/Factory_method_pattern) and [Template Method](http://en.wikipedia.org/wiki/Template_method_pattern). *Higher-order functions.* Instead of making a subclass, just pass the the function you'd like to vary the behavior of with the function.

[Abstract Factory](http://en.wikipedia.org/wiki/Abstract_factory_pattern), [Builder](http://en.wikipedia.org/wiki/Builder_pattern) and [Bridge](http://en.wikipedia.org/wiki/Bridge_pattern). *Type classes* and *smart constructors.* Type classes are capable of defining functions which creating instances of themselves; all a function needs to do to take advantage of this is to commit itself to returning some value of type `TypeClass a => a` and using only (constructor et alia) functions that the type class exposes. If you're not just constructing values but manipulating them with the general type class interface, you have a Bridge. Smart constructors are functions built on top of the basic data constructor that can do "more", whether this is invariant checking, encapsulation or an easier API, this can correspond to more advanced methods that a factory provides.

[Adapter](http://en.wikipedia.org/wiki/Adapter_pattern), [Decorator](http://en.wikipedia.org/wiki/Decorator_pattern) and [Chain of Responsibility](http://en.wikipedia.org/wiki/Chain-of-responsibility_pattern). *Composition* and *lifting.* Function composition can be used to form a pipeline of data between functions; a foreign function can be sandwiched between two functions that convert to and from the type the function expects, or a function can be composed with another to make it do more things. If the signature stays the same, one or more of the functions was *endomorphic.* If the functions have side effect, it may be Kleisli arrow composition (more plainly spoken as monadic function composition.) Multiple functions can handle the same input using the Reader monad.

[Visitor](http://en.wikipedia.org/wiki/Visitor_pattern). *Equational functions.* Frequently *foldable.* Many functional languages favor grouping the same operation on different data constructors together, in a mathematical equational style. This means similar behaviors are grouped together. Traditional grouping of behavior by "class" is implemented with *type classes.* Visitors typically collapse the data structures they operate on into smaller values, this is seen in the fold family of functions.

[Interpreter](http://en.wikipedia.org/wiki/Interpreter_pattern). *Functions*. Frequently circumvented with an *embedded domain specific language.* Algebraic data types make light-weight abstract syntax trees easy to formulate. Just as Visitor is often used with Interpeter, you'll probably write your interpreting functions with pattern matching. Even better, don't come up with another data type; just use functions and infix operators to say what you mean. Closely related to...

[Command](http://en.wikipedia.org/wiki/Command_pattern). *Monads.* See also *algebraic data types*, frequently *generalized (GADT)*. A pure language will not run your `IO` until `main` touches it, so you can freely pass values of type `IO a` without fear of actually causing the side-effect, though these functions are difficult to serialize (a common motivation behind Command). Parametrization of the action to perform is once again achieved through higher-order functions. GADTs are a little more bulky, but can be seen in places like the [Prompt monad (PDF)](http://themonadreader.files.wordpress.com/2010/01/issue15.pdf), where a GADT is used to represent actions that another function interprets into the `IO` monad; the type gives a statically enforced guarantee of what operations in this data type are allowed to do.

[Composite](http://en.wikipedia.org/wiki/Composite_pattern). Recursive *algebraic data types.* Especially prominent since there's no built-in inheritance.

[Iterator](http://en.wikipedia.org/wiki/Iterator_pattern). *Lazy lists.* Iterators expose an element-by-element access of a data structure without exposing it's external structure; the list is the API for this sort of access and laziness means we don't compute the entirety of the stream until it is necessary. When IO is involved, you might use a real iterator.

[Prototype](http://en.wikipedia.org/wiki/Prototype_pattern). *Immutability.* Modification copies by default.

[Flyweight](http://en.wikipedia.org/wiki/Flyweight_pattern). *Memoising* and *constant applicative forms (CAF).* Instead of calculating the result of an expression, create a data structure that contains all of the results for all possible input values (or perhaps, just the maximum memo). Because it is lazy, the result is not computed until it is needed; because it is a legitimate data structure, the same result is returned on successive computations. CAFs describe expressions that can be lifted into the top-level of a program and whose result can be shared by all other code that references it.

[State](http://en.wikipedia.org/wiki/State_pattern) and [Memento](http://en.wikipedia.org/wiki/Memento_pattern). Unnecessary; state has an explicit representation and thus can always be arbitrarily modified, and it can include functions, which can be changed to change behavior. State as a function (rather than an object or an enumeration), if you will. The encapsulation provided by Memento is achieved by hiding the appropriate constructors or destructors. You can easily automatically manage past and future states in an appropriate monad such as the Undo monad.

[Singleton](http://en.wikipedia.org/wiki/Singleton_pattern). Unnecessary; there is no global state except in a monad, and the monad's type can enforce that only one instance of a record is present; functions exist in a global namespace and are always accessible.

[Facade](http://en.wikipedia.org/wiki/Facade_pattern). *Functions.* Generally less prevalent, since function programming focuses on input-output, which makes the straight-forward version use of a function very short. High generality can require more user friendly interfaces, typically implemented with, well, more functions.

[Observer](http://en.wikipedia.org/wiki/Observer_pattern). One of many concurrency mechanisms, such as channels, asynchronous exceptions and mutable variables. See also *functional reactive programming.*

[Proxy](http://en.wikipedia.org/wiki/Proxy_pattern). *Wrapped data types,* *laziness* and *garbage collector.* See also ref monadic types (IORef, STRef), which give more traditional pointer semantics. Laziness means structures are always created on demand, garbage collection means smart references are not necessary. You can also wrap a data type and only publish accessors that enforce extra restrictions.

[Mediator](http://en.wikipedia.org/wiki/Mediator_pattern). *Monad stacks*. While it's not useful to talk about interactions between objects, due to a preference for stateless code, monad stacks are frequently used to provide a unified interface for code that performs operations in a complex environment.

Comments and suggestions appreciated; I'll be keeping this post up-to-date.
