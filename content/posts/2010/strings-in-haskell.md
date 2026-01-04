---
title: "How to pick your string library in Haskell"
date: 2010-08-02 09:00:50
slug: strings-in-haskell
categories: [Haskell]
comments:
    - id: 799
      author: Kevin Jardine
      date: "2010-08-02 09:26:46"
      content: |
        Do you see any sign of a convergence in Unicode support to Text (or CompactString which I have never heard of before) ?
        
        In my experience, a lot of current Haskell projects use ByteString for Unicode when by your article they should be using something else.
    - id: 801
      author: Kevin Jardine
      date: "2010-08-02 11:18:21"
      content: |
        Hmmm. I notice that the home page for CompactString:
        
        http://twan.home.fmf.nl/compact-string/
        
        states that:
        
        The library is not finished yet. 
        
        and the last news update on that page is dated March 2007. Could it be that CompactString has been abandoned?
    - id: 808
      author: Edward Z. Yang
      date: "2010-08-02 13:19:32"
      content: "Kevin, re your second comment, that seems to be the case. However, the 0.3.1 version notice notes that all functions are implemented; perhaps the author found the library in its current form satisfactory for his needs."
    - id: 802
      author: Sean Leather
      date: "2010-08-02 11:20:54"
      content: |
        Why did you choose [Char] for ASCII/8-bit, unpacked, lazy text and [Word8] for UTF-8, unpacked, lazy text? Either you are concerned for efficiency in the size of the character or not (since a Char is 32 bits). Can't have it both ways. ;)
        
        Also, isn't it a bit misleading to suggest using [Word8] and [Word16] for UTF-8 and UTF-16? It's not as simple as using the list operations, since one would still have to write an entire library to deal with the variable-length character encoding.
    - id: 803
      author: Sean Leather
      date: "2010-08-02 11:27:00"
      content: "BTW, I don't mean to be too negative. Thanks for writing this up. It looks good (except for a few nitpicks)! :)"
    - id: 807
      author: Edward Z. Yang
      date: "2010-08-02 13:12:26"
      content: |
        Kevin, I'm not sure what you mean by convergence. Text is all about Unicode support. Whether or not this is enough support for an international application is another question (one that, in my opinion, can only be answered by trying to build something like MediaWiki in Haskell.)
        
        Sean, I was planning to go into these (strange) choices in more detail on my next post. The short answer is that Char is about as pure Unicode you can get, since we're devoting a whole 32-bits to representing the character. UTF-8, however, is an encoding and thus if we want to deal with this representation, we need to deal with a binary representation; it turns out the most convenient grouping is 8-bit words. I'm not sure if GHC inflates unpacked 8-bit words into 32-bits, so I should check that.
        
        It is a bit misleading to recommend Word8 and Word16 for unpacked lazy encoded strings in UTF-8 and UTF-16. But it works and it has the properties written on the tin (one corollary to this is, if you care about performance, you never want unpacked strings!) As for not being able to use list operations, you'd be surprised. I wrote a Unicode aware library in PHP that used UTF-8 has its internal string representation (since PHP only gives you strict binary strings.) I didn't need to deal with the variable length: the operation I cared about, strpos, is guaranteed to work on well-formed UTF-8. More on this later.
    - id: 810
      author: Johan Tibell
      date: "2010-08-02 14:10:39"
      content: |
        I think we will converge on Text for (Unicode) text processing. Bryan O'Sullivan and I will look into getting rid of some performance problems as soon as we find some time.
        
        For binary data I'd go with ByteString.
    - id: 811
      author: Edward Z. Yang
      date: "2010-08-02 14:18:14"
      content: "Johan, I agree with your assessment. ByteString's dominance seems assured, given how many derivative libraries have been built on top of it."
    - id: 813
      author: Ivan Lazar Miljenovic
      date: "2010-08-02 21:36:41"
      content: |
        What about using Text for UTF-8, etc.? utf8-string? Rope libraries?
        
        Come on, be exhaustive! ;-)
    - id: 814
      author: Edward Z. Yang
      date: "2010-08-02 21:50:54"
      content: "To seriously answer your question, I don’t think Text supports UTF-8 has its internal representation: it’s a one-pony trick (UTF-16). I managed to miss utf8-string when doing my survey; it looks like it would be handy when managing [Word8] and UTF-8 ByteStrings (I'll add it in this evening.) Rope... is another matter all together, but worth a mention. :-)"
    - id: 816
      author: Ivan Lazar Miljenovic
      date: "2010-08-03 00:50:11"
      content: "Text does use UTF-16 internally, but does have conversion functions if you need/want UTF-8."
    - id: 817
      author: Edward Z. Yang
      date: "2010-08-03 00:55:22"
      content: "Yeah, the chart was intended for in-memory representation only."
    - id: 820
      author: Chris Eidhof
      date: "2010-08-03 14:21:20"
      content: |
        Edward,
        
        Thanks very much for writing this up. It certainly clears up a lot of things for me.
    - id: 823
      author: "Bryan O'Sullivan"
      date: "2010-08-04 02:32:10"
      content: |
        Edward,
        
        For an article that purports to give advice, I am taken aback at the unsoundness of the criteria and the eccentricity of the guidance that you give.
        
        For instance, your opener "You should default to using [Char] for text strings and [Word8] for binary strings" begins with a clanger, because (a) those types give simply atrocious performance and memory usage and (b) some of the standard list manipulation functions give incorrect results when used on [Char]. All of your subsequent advice to use [Char] or some kind of [Word] for text data can be disregarded on both bases, a point to which I shall return.
        
        On a related note, your admonition to care about the internal representation is also in poor form, except for the very basic binary/text distinction. As an example, advising someone to (a) care about UTF-16 is marginal, but (b) then to use [Word16] is simply weird. If someone was to follow that [Word16] notion, they could easily end up accepting or generating garbage, and in fact would be very likely to do so. This is not to mention the substantial amount of time needed to cook up a library that could even do a sub-par job handling [Word16] as text.
        
        I could go on, but I'm sure you get my point. It is not the best of form to try to strike an authoritative pose, and then produce something quite so bafflingly misguided.
        
        I appreciate your enthusiasm for Haskell, but ardour is a dish best flavoured with good sense.
    - id: 824
      author: Edward Z. Yang
      date: "2010-08-04 02:59:05"
      content: |
        Hello Bryan,
        
        I guess I did get carried away combining the different possibilities of binary/text, lazy/strict and packed/unpacked, and so thus thought “If for some very odd reason I wanted an unpacked lazy UTF-16 representation, what would I use?” and didn’t say, “You don’t actually want that.” Perhaps it is better not to mention the possibility at all.
        
        You’re right that the tenor of the article is all wrong. The list is factually correct but interesting mostly for only academic purposes. For most people, better advice would be simply: “text for Unicode, bytestring for Binary.”
        
        I will update the text accordingly.
        
        Yours humbly,
        Edward
        
        P.S. I’m curious to know what standard list manipulation functions mess up with [Char]. Since each Char is essentially a 32-bit integer, I imagine it can’t be anything involving codepoint divisions.
    - id: 825
      author: Edward Z. Yang
      date: "2010-08-04 03:18:59"
      content: Updated.
    - id: 826
      author: Johan Tibell
      date: "2010-08-04 04:00:26"
      content: |
        Edward, an example would be
        
            map toLower
        
        as lowercasing a character sometimes produces two characters (same applied for uppercasing). Similar problems crop up when you want to compare two strings as they generally need to be converted to some canonical form before comparison (as the same string can be created from different sequences of code points).
    - id: 827
      author: Sean Leather
      date: "2010-08-04 10:34:15"
      content: "It's looking better. One more nit to pick: What does \"can specialize\" mean in the position between [Word8] and Codec.Binary.UTF8.Generic? Can you rephrase?"
    - id: 830
      author: Edward Z. Yang
      date: "2010-08-04 13:30:19"
      content: |
        Sean, I rephrased it; hopefully it's better now.
        
        Johann, that’s a good catch. Equality and canonicalization is definitely an important problem (http://www.mediawiki.org/wiki/Unicode_normalization_considerations) although as I understand it 'text' will not perform those operations out of the tin either; you need to use 'text-icu' explicitly.
    - id: 832
      author: Eyal Lotem
      date: "2010-08-05 02:40:04"
      content: |
        Need more type-classes!
        
        Having different types and different (qualified) function names for lazy/strict, or even for different UTF encodings of unicode is exactly the opposite of what I'd expect of Haskell.
        
        In Haskell, instead of encoding-agnostic/laziness-agnostic code, because of the lack of type-classes, most code is horribly specific.
    - id: 2628
      author: cmeclax
      date: "2011-06-13 19:53:37"
      content: "http://hackage.haskell.org/package/string-class-0.1.5.1 is a leap in the right direction."
    - id: 3308
      author: bairyn
      date: "2012-01-08 13:06:16"
      content: "A good way, perhaps, to polymorphically support string types is to use bos's ListLike's standard StringLike class to inefficiently convert to and from Strings as a default, and to use rewrite rules, which require only optimizations to be enabled and trust, to provide efficient implementations for individual types.  Often, the optimal implementations for individual string types can differ substantially from the inefficient general one; such implementations often require or use functionality that is specific to the type and that is not available generally for any string type.  That way, authors can optimize functions for a specific string type or types; and their code will, albeit potentially inefficiently (for which I can't think of a better alternative, currently), still support, in addition to any of the existing standard string types, any new string types that may come along and is used."
    - id: 22303
      author: chris
      date: "2018-03-21 18:31:13"
      content: |
        FWIW FYI (2018)
        
        Data.CompactString is DEPRECIATED in favor of Data.Text
        
        http://hackage.haskell.org/package/compact-string
---

*Notice.* Following a critique from Bryan O’Sullivan, I’ve restructured the page.

“How do the different text handling libraries compare, and when should we use which package?” [asks Chris Eidhof](http://blog.ezyang.com/2010/07/suggestion-box/#comment-787). The latter question is easier to answer. Use [bytestring](http://hackage.haskell.org/package/bytestring) for binary data—raw bits and bytes with no explicit information as to semantic meaning. Use [text](http://hackage.haskell.org/package/text) for Unicode data representing human written languages, usually represented as binary data equipped with a character encoding. Both (especially bytestring) are widely used and are likely to become—if they are not already—standards.

There are, however, a lot more niche string handling libraries on Hackage. Having not used all of them in substantial projects, I will refrain on judging them on stability or implementation; instead, we’ll categorize them on the niche they fill. There are several axes that a string library or module may be categorized on:

- *Binary or text?* Binary is raw bits and bytes: it carries no explicit information about what a `0` or `0x0A` means. Text is meant to represent human language and is usually binary data equipped with a character encoding. This is [the most important distinction](http://www.joelonsoftware.com/articles/Unicode.html) for a programmer to know about.
- If text, *ASCII, 8-bit or Unicode?* ASCII is simple but English-only; 8-bit (e.g. Latin-1) is ubiquitous and frequently necessary for backwards compatibility; Unicode is the “Right Way” but somewhat complicated. Unicode further asks, *What in-memory encoding?* UTF-16 is easy to process while UTF-8 can be twice as memory efficient for English text. Most languages pick Unicode and UTF-16 for the programmer.
- *Unpacked or packed?* Unpacked strings, the native choice, are just linked lists of characters. Packed strings are classic C arrays, allowing efficient processing and memory use. Most languages use packed strings: Haskell is notable (or perhaps notorious) in its usage of linked lists.
- *Lazy or strict?* Laziness is more flexible, allowing for things like streaming. Strict strings must be held in memory in their entirety, but can be faster when the whole string would have needed to be computed anyway. Packed lazy representations tend to use chunking to reduce the number of generated thunks. Needless to say, strict strings are the classic interpretation, although lazy strings have useful applications for streaming.

Based on these questions, here are where the string libraries of Hackage fall:

<div class="container left">

- Binary:
  - Packed:
    - Lazy: [Data.ByteString.Lazy](http://hackage.haskell.org/packages/archive/bytestring/0.9.1.7/doc/html/Data-ByteString-Lazy.html)
    - Strict: [Data.ByteString](http://hackage.haskell.org/packages/archive/bytestring/0.9.1.7/doc/html/Data-ByteString.html)
- Text:
  - ASCII or 8-bit:
    - Packed and lazy: [Data.ByteString.Lazy.Char8](http://hackage.haskell.org/packages/archive/bytestring/0.9.1.7/doc/html/Data-ByteString-Lazy-Char8.html)
    - Packed and strict: [Data.ByteString.Char8](http://hackage.haskell.org/packages/archive/bytestring/0.9.1.7/doc/html/Data-ByteString-Char8.html), [Data.CompactString.ASCII](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString-ASCII.html) or [Data.CompactString](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString.html) with [Latin1](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString-Encodings.html)
  - Unicode:
    - [UTF-32](http://en.wikipedia.org/wiki/UTF-32/UCS-4), unpacked and lazy: `[Char]` (not a library, but if it was, this is where it would fall)
    - [UTF-16](http://en.wikipedia.org/wiki/UTF-16/UCS-2):
      - Packed and lazy: [Data.Text.Lazy](http://hackage.haskell.org/packages/archive/text/0.7.2.1/doc/html/Data-Text-Lazy.html)
      - Packed and strict: [Data.Text](http://hackage.haskell.org/packages/archive/text/0.7.2.1/doc/html/Data-Text.html) or [Data.CompactString.UTF16](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString-UTF16.html)
    - [UTF-8](http://en.wikipedia.org/wiki/UTF-8):
      - Unpacked and lazy: [Codec.Binary.UTF8.Generic](http://hackage.haskell.org/packages/archive/utf8-string/0.3.6/doc/html/Data-String-UTF8.html) contains generic operations that can be used to process `[Word8]`.
      - Packed and lazy: [Data.ByteString.Lazy.UTF8](http://hackage.haskell.org/packages/archive/utf8-string/0.3.6/doc/html/Data-ByteString-Lazy-UTF8.html)
      - Packed and strict: [Data.CompactString.UTF8](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString-UTF8.html) or [Data.ByteString.UTF8](http://hackage.haskell.org/packages/archive/utf8-string/0.3.6/doc/html/Data-ByteString-UTF8.html)
    - [Compact (UTF-8-like)](http://twan.home.fmf.nl/compact-string/details.html), packed and strict: [Data.CompactString](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString.html) with [Compact](http://hackage.haskell.org/packages/archive/compact-string/0.3.1/doc/html/Data-CompactString-Encodings.html)

</div>

Beyond in-memory encoding, there is also a question of source and target encodings: hopefully something normal, but occasionally you get Shift_JIS text and you need to do something to it. You can convert it to Unicode with [encoding](http://hackage.haskell.org/package/encoding) (handles `String` or strict/lazy `ByteString` with possibility for extension with `ByteSource` and `ByteSink`) or [iconv](http://hackage.haskell.org/package/iconv) (handles strict/lazy `ByteString`).

*Unicode joke.*

    Well done, mortal!  But now thou must face the final Test...--More--

    Wizard the Evoker         St:10 Dx:14 Co:12 In:16 Wi:11 Ch:12  Chaotic
    Dlvl:BMP  $:0  HP:11(11) Pw:7(7) AC:9  Xp:1/0 T:1

*Alt text.* Yeah, I got to the Supplementary Special-purpose Plane, but then I got killed by TAG LATIN CAPITAL LETTER A. It looked like a normal A so I assumed it was just an Archon...
