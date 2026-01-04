---
title: "Parsec: \"try a &lt;|&gt; b\" considered harmful"
date: 2014-05-17 21:46:52
slug: parsec-try-a-or-b-considered-harmful
categories: [Haskell]
comments:
    - id: 6597
      author: Greg Weber
      date: "2014-05-17 23:46:54"
      content: "What Applicative parsing libraries are available and what would you recommend?"
    - id: 6598
      author: Edward Z. Yang
      date: "2014-05-18 02:15:00"
      content: |
        Hm! So there definitely has been a ground-shift with regards to the idiomatic way to write parsers (applicatively instead of using monads), which is what I was alluding to, but when I saw your question and checked the most popular packages, it wasn't obviously the case that they were doing something more efficient when you only used the applicative interface. Certainly attoparsec is not doing anything better, and from a glance, trifecta, parsimony and polyparse don't do anything special either. So I think I misspoke, and I will reword the sentence accordingly.
        
        A more academic parsing library which is more efficient when you use the applicative instance is uu-parsinglib (http://hackage.haskell.org/package/uu-parsinglib) although I do not know why it has not caught on in the wider zeitgeist. (Breadth-first parsing can be a bit idiosyncratic.)
    - id: 6599
      author: Ben Edwards
      date: "2014-05-18 09:43:10"
      content: "The big efficiency win with trifecta is the slicing conbinators. When you know you are thowing the intermediate productions, replace with unit! Of course, this has nothing to do with being monad or applicative. I just thought I'd toss it out there."
    - id: 6600
      author: Sami Liedes
      date: "2014-05-18 09:48:34"
      content: "A while ago I needed to parse something substantial and, being new to Haskell parser combinators, I started with parsec. Eventually I tried attoparsec, and contrary to what everybody says, to me its error messages were generally better and it was easier to use than parsec. I no longer needed to sprinkle my code with magic 'try' in just the right places, and IIRC attoparsec always identified in error messages correctly the set of tokens it would have accepted and the token it got instead, which is just what parsec loses when you use 'try'. The only (admittedly substantial) drawback was that the error messages could not show the position of the error in the input."
    - id: 6601
      author: Greg Weber
      date: "2014-05-18 11:05:49"
      content: "uu-parsing hasn't caught on because it does not scale down to new users on small tasks. In the time it would take me to wrap my head around how to use this lib, I will already be done with the job with Parsec (even if I had to just learn Parsec) because the Parsec documentation is good and the API is easy to understand. I hope uu-parsing will get an API makeover and some good documentation."
    - id: 6602
      author: Edward Kmett
      date: "2014-05-18 11:57:51"
      content: |
        Note: Trifecta explicitly will not move your error messages when backtracking for try. This means you have to put another expected token on.
        
        The usual idiom is to treat the thing you are forced to try as a lexeme and blame the whole structure.
        
        pStmt = try pQualifiedImport &lt;?&gt; "qualified import"
            pImport
        
        The benefit there is if the other parser fails you can get both in the set of expected tokens at the point to which it had to backtrack.
    - id: 6603
      author: Edward Kmett
      date: "2014-05-18 11:58:19"
      content: "There was a &lt;?&gt; in there. =)"
    - id: 6604
      author: Anonymous
      date: "2014-05-18 13:36:46"
      content: "You can try parsek instead. For errors, it has an option to return the error for the longest correctly parsed alternative (not just the rightmost(?) like parsec)."
    - id: 6605
      author: Edward Z. Yang
      date: "2014-05-19 06:57:40"
      content: |
        Ben: So /that's/ what a slicing combinator is! It is not at all clear from the Haddock documentation, "Run a parser, grabbing all of the text between its start and end points" that this is the intended use-case. If you manually cut, much in the same way I'm doing here with 'try', then yes, applicative structure is not needed.
        
        Anonymous: Which is... apparently in the 'Encode' package? The source paper suggests that some sort of breadth-first strategy is also being employed here.
        
        There was also discussion on Reddit: http://www.reddit.com/r/haskell/comments/25ulsv/ezyang_try_a_b_considered_harmful/
    - id: 6606
      author: massysett
      date: "2014-05-20 13:15:23"
      content: |
        Your last paragraph seems to suggest (if I may paraphrase) "well, gee, Parsec is not that great, but a lot of docs say use it, so here's how to use it, but really you might want to look at something faster that doesn't even bother with errors because those faster libraries backtrack automatically."
        
        Parsec is not just a default choice that has a lot of documentation.  Rather, for many jobs it is the best choice.  In many contexts attoparsec is utterly useless precisely because it does not deliver decent error messages.
        
        I have a parser which needs to deliver good error messages, because it parses handwritten files.  If there's an error the user needs to know where it is so he can fix it.  But sure, it would be nice if the parser were faster.  So I tried attoparsec.  I figured I could use attoparsec if it completes successfully but, because its error messages are useless, I could fall back to Parsec and parse the file again and get an error message for those cases in which the parse fails.
        
        It turned out that for this particular grammar attoparsec was not much faster! There may have been something like a 15% speed boost but nothing that made it worthwhile to maintain two parsers.  I also tried happy and alex and saw no appreciable speed gain there either, and their error messages are nowhere near as good as Parsec's.
        
        It seems to me that Parsec has become a whipping boy for complaints of (1) "try is too hard" and (2) "it's too slow" when (1) you need to control backtracking to get good error messages and (2) for many grammars it's not appreciably slower!
    - id: 6732
      author: Krakrjak
      date: "2014-06-02 13:46:00"
      content: "Massysett, 0.12.0 of attoparsec was just released over the weekend.  The change log reports some impressive speed ups.  Might be a good time to revisit your dual parser version of the code."
    - id: 21985
      author: James
      date: "2017-05-18 05:26:17"
      content: "This post was really helpful for me, thanks!"
    - id: 22553
      author: Asad
      date: "2018-10-25 17:47:32"
      content: |
        I don't understand why the error information is thrown away.
        
        When you have an `alt` between several `try`-ed parsers, and the parser has failed to match any of them, why couldn't it have simply hung on to the error information and displayed something to the tune of: "expecting x, y or z, wasn't able to parse x because ..., wasn't able to parse y because ..., wasn't able to parse z because ...", recursively?
        
        Is this difficult or impossible from an engineering/theoretical standpoint?
    - id: 22572
      author: Edward Z. Yang
      date: "2018-11-11 20:19:51"
      content: "Asad: It's a good question. The difficulty is performance; specifically, space usage. Suppose that you never throw away information about try branches; furthermore, suppose you have a deep parse that consists of repeatedly following the second branch of a try'ed parser. If we always hold onto the information on the left hand side, then you'll end up taking O(n) space to the depth. That's bad, and justifies why Parsec is setup the way it is."
---

<div class="container center">

*tl;dr The scope of backtracking try should be minimized, usually by placing it inside the definition of a parser.*

</div>

Have you ever written a Parsec parser and gotten a really uninformative error message? :

    "test.txt" (line 15, column 7):
    unexpected 'A'
    expecting end of input

The line and the column are randomly somewhere in your document, and you're pretty sure you should be in the middle of some stack of parser combinators. But wait! Parsec has somehow concluded that the document should be ending immediately. You noodle around and furthermore discover that the true error is some ways *after* the actually reported line and column.

You think, “No wonder Parsec gets such a bad rep about its error handling.”

------------------------------------------------------------------------

Assuming that your grammar in question is not too weird, there is usually a simple explanation for an error message like this: the programmer sprinkled their code with too many backtracking `try` statements, and the backtracking has destroyed useful error state. In effect, at some point the parser failed for the reason we wanted to report to the user, but an enclosing `try` statement forced the parser to backtrack and try another (futile possibility).

This can be illustrated by way of an example. A Haskeller is playing around with parse combinators and decides to test out their parsing skills by writing a parser for Haskell module imports:

    stmt ::= import qualified A as B
           | import A

Piggy-backing off of Parsec’s built in [token combinators](http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Token.html) (and the sample code), their first version might look something like this:

    import Text.Parsec
    import qualified Text.Parsec.Token as P
    import Text.Parsec.Language (haskellDef)

    data Stmt = QualifiedImport String String | Import String
        deriving (Show)

    pStmt = pQualifiedImport <|> pImport

    pQualifiedImport = do
        reserved "import"
        reserved "qualified"
        i <- identifier
        reserved "as"
        i' <- identifier
        return (QualifiedImport i i')

    pImport = do
        reserved "import"
        i <- identifier
        return (Import i)

    lexer = P.makeTokenParser (haskellDef
        { P.reservedNames = P.reservedNames haskellDef ++ ["qualified", "as"] })
    identifier = P.identifier lexer
    reserved = P.reserved lexer

    parseStmt input = parse (pStmt >> eof) "(unknown)" input

Unfortunately, the parser doesn't work for regular imports—they get this error message:

    *Main> parseStmt "import Foo"
    Left "(unknown)" (line 1, column 8):
    unexpected "F"
    expecting "qualified"

After a little Googling, they discover that [Parsec doesn’t backtrack by default](http://stackoverflow.com/questions/9976388/haskell-text-parsec-combinator-choice-doesnt-backtrack). Well, that’s fine; why not just insert a try into the parser.

    pStmt = try pQualifiedImport <|> pImport

This fixes both parses and suggests the following rule for writing future parsers:

> If I need choice over multiple parsers, but some of these parsers might consume input, I better tack a `try` onto each of the parsers, so that I can backtrack.

Unbeknownst to the user, they have introduced bad error reporting behavior:

    *Main> parseStmt "import qualified Foo s B"
    Left "(unknown)" (line 1, column 17):
    unexpected reserved word "qualified"
    expecting letter or digit or "#"

Wait a second! The error we wanted was that there was an unexpected identifier `s`, when we were expecting `as`. But instead of reporting an error when this occurred, Parsec instead *backtracked*, and attempted to match the `pImport` rule, only failing once that rule failed. By then, the knowledge that one of our choice branches failed had been forever lost.

How can we fix it? The problem is that our code backtracks when we, the developer, know it will be futile. In particular, once we have parsed `import qualified`, we know that the statement is, in fact, a qualified import, and we shouldn’t backtrack anymore. How can we get Parsec to understand this? Simple: *reduce the scope of the try backtracking operator:*

    pStmt = pQualifiedImport <|> pImport

    pQualifiedImport = do
        try $ do
            reserved "import"
            reserved "qualified"
        i <- identifier
        reserved "as"
        i' <- identifier
        return (QualifiedImport i i')

Here, we have moved the `try` from `pStmt` into `pQualifiedImport`, and we only backtrack if `import qualified` fails to parse. Once it parses, we consume those tokens and we are now committed to the choice of a qualified import. The error messages get correspondingly better:

    *Main> parseStmt "import qualified Foo s F"
    Left "(unknown)" (line 1, column 22):
    unexpected "s"
    expecting "as"

The moral of the story: The scope of backtracking try should be minimized, usually by placing it inside the definition of a parser. Some amount of cleverness is required: you have to be able to identify how much lookahead is necessary to commit to a branch, which generally depends on *how* the parser is used. Fortunately, many languages are constructed specifically so that the necessary lookahead is not too large, and for the types of projects I might use Parsec for, I’d be happy to sacrifice this modularity.

------------------------------------------------------------------------

Another way of looking at this fiasco is that Parsec is at fault: it shouldn’t offer an API that makes it so easy to mess up error messages—why can’t it automatically figure out what the necessary lookahead is? While a traditional parser generator can achieve this (and improve efficiency by avoiding backtracking altogether in our earlier example), there are some fundamental reasons why Parsec (and monadic parser combinator libraries like it) cannot automatically [determine what the lookahead needs to be](http://stackoverflow.com/a/7863380/23845). This is one of the reasons (among many) why many Haskellers prefer faster parsers which simply [don’t try to do any error handling at all.](https://hackage.haskell.org/package/attoparsec)

Why, then, did I write this post in the first place? There is still a substantial amount of documentation recommending the use of Parsec, and a beginning Haskeller is more likely than not going to implement their first parser in Parsec. And if someone is going to write a Parsec parser, you might as well spend a little time to limit your backtracking: it can make working with Parsec parsers a *lot* more pleasant.
