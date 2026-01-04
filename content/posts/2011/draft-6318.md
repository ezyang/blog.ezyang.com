---
title: "How many bugs do types catch anyway?"
date: 2011-12-24 22:16:54
slug: 
draft: true
categories: [Miscellaneous]
---

In the postscript for my [previous post](XXX), I drew the following picture describing the effect that types have on the testing space:

![image](/img/XXX)

A commenter was quick to complain that this picture overrepresented the benefits of types for catching bugs—that, in reality, the situation looked a bit more like this:

![image](/img/XXX)

I can sense the underlying question, which is: “How many bugs do types catch anyway?” This is a perfectly legitimate question, which really should be tested empirically—we’ll return to this point later. However, this is not at all the statement that this image is making! The confusion here stems from the fact that we *automatically* ignore (or, more uncharitably, forget about) certain classes of inputs when we consider what to test. Hopefully an example will make this clear.

Consider a function which takes a single input (let’s assume that it’s not affected by any other environmental considerations.) Suppose that it’s written in a C-like dynamic language which has booleans, fixed-width integers, floats and characters (no variable size data for now). If we don’t know what the type of the function is, what are all the possible inputs to this function? All booleans (`true`, `false`), all fixed-width integers (all 2\*\*64 of them!), all floats (all 2\*\*128 of them!) and all characters (256, if we charitably assume 8-bits with no Unicode.)

Now, suppose that this function’s input has a type, and it’s boolean. Well, we only need to test two things now.

![image](/img/XXX)

“Wait!” you may exclaim, “That’s missing the point! You’ve ignored some critical simplifying factors in the dynamic case: maybe the language dynamically casts values to booleans, or maybe the function errors if you don't pass it a boolean, or maybe the behavior is just undefined and thus by definition can’t be wrong. But whatever the answer, the dynamic case is not as complicated as you make it out to be.”

Sure. You’ve used your understanding of the program to rule out inputs as irrelevant or so rarely subject to bugs that you'd be more likely to have a memory bit flip due to cosmic radiation. You'd probably be right, too! But it is *critical* to understand that this mental leap doesn't truly give you any assurance about the regions you've decided not to test. And I assert, without justification, that in certain situations, this mental leap will be wrong. *To use types is a position of humility.*

A final word on the question of testing the effects of types on software development: a commonly cited study on the effects of types on program development time is \_\_\_\_, which answered negatively on the question of whether or not types made the development proceed more quickly. XXX I think the first follow-up study I would conduct would be the following experiment: given an existing program written in a typed/untyped language, please make some feature change to it. As a proponent and user of strongly typed languages, I think this is one place where types will clearly shine: if I change the type of some parameter, my type checker will tell me all of the places I need to fix. So I might imagine the same should hold in the lab: users of typed languages should successfully update their programs and catch all of the cases; users of untyped languages will manage to update most but not all instances, and some minor bugs will persist (usually areas of code that are not covered by tests, e.g. exception handling code.) But there is a long gap between the *plausible* and the *true*, and that is why we have experiment.
