---
title: "So you want to control flow in PT2"
date: 2025-09-05 10:01:23
slug: so-you-want-to-control-flow-in-pt2
categories: [PyTorch]
---

*With contributions from Richard Zou.*

PT2’s dominant internal representation, FX graphs, do not directly support control flow (if statements, while loops): they only represent straight-line basic blocks. Most of our graph capture mechanisms are tracing based (`fx.symbolic_trace`, `make_fx`, Dynamo), which means that we expect to be able to linearize all conditionals we encounter into a straight line program. Sometimes, you want to work with code that has control flow while working the compiler stack. There is no silver bullet, instead there are a lot of different options with different tradeoffs.

# Regional compilation

We have a perfectly good general purpose language that supports control flow: Python. To handle control flow, compile only regions/submodules of your program that have no internal control flow, and then string them together with a standard Python control flow constructs. PT2 compiled regions are compositional with non-compiled regions, “it works.”

**Pro:**

- Simple: requires no major model changes
- Universal: it always works (including data dependent flow, calling into third-party libraries, making an HTTP request, anything!)

**Cons:**

- You will not get a full graph this way; you will only get graphs for each region. In particular, you will not be able to do truly global optimizations, nor will you be able to serialize a self-contained Python-less representation of the entire model
- It can sometimes be inconvenient to structure your program so all the regions you want are compilable. Suppose you have this call graph between modules: A -\> B -\> C. C is compileable; A is compileable *except* for its call to B, which is what does the control flow. It’s easy to compile C, but you can’t directly compile A, as it has a B-shaped bit that can’t be compiled. What to do? If you split A so it is pipelined as A1, B, A2, you can then compile A1 and A2, but not B. Dynamo also supports “graph breaks” to automatically perform this split for you, in which case you just disable compilation on B, but graph break generated graphs can be difficult to reason about as the inputs to A2 are implicitly inferred.

Link: [Reducing torch.compile cold start compilation time with regional compilation](https://docs.pytorch.org/tutorials/recipes/regional_compilation.html)

# Multiple graphs dispatched with guards

When the control flow is controlled by arguments that are known ahead of time (no data-dependent), you can also compile at the top level and get the flattened straight-line program for the *particular* branching you had in this case. Because Dynamo is a symbolic bytecode interpreter, it can automatically determine what inputs were used as part of control flow, and generate *guards* to validate that we would take the same paths again. If those values change, we will recompile the program at the new values. We dispatch between all the different unrollings of the program we have generated.

**Pros:**

- Simple: requires no major model changes
- You get a full graph for a particular unrolling of loops / conditionals, so global optimizations are possible

**Cons:**

- Doesn’t work with data-dependent shapes.
- You will end up with a graph for every unrolling; for example, if you have a loop that ranges from 1 to 32, you will end up with 32 different graphs. This will increase compile time.

# Black box via custom operator

An FX graph just calls operators. The operator internally can have whatever control flow in them they want. So you can always black box a problematic region of your model into an operator and preserve compilation for everything else.

**Pros:**

- You get a single, full graph that works for all possible branches

**Cons:**

- A custom operator only supports inputs/outputs that fall inside our type system, which means you can only pass simple types like Tensor, int, bool (or pytree-able containers containing these things). There is some in progress work to relax this to allow more opaque types.
- You have to explicitly declare all the inputs/outputs for the custom operator. This can be tiresome if the black boxed region represents a Module, since all the parameters also have to be directly passed in as well. The larger the region you black box, the bigger the arguments are.
- You don’t actually get to see the inside of the custom operator from the outside graph, so no optimization over both inside and outside of the custom operator is possible. (Of course, you can always special case this operator in a pass on the outer graph.)
- There are some bugs related to doing another torch.compile region inside of a custom operator, although these are workaroundable: <https://github.com/pytorch/pytorch/issues/151328>

# Conditional operators / Unroll to max iterations

Do you really, really need a conditional? If you’re doing an if-branch, can you instead rewrite it so that you run both branches and torch.where dispatch to the results? If you’re doing a while-loop, can you unroll it to the max number of iterations and rely on dynamic shapes to cause it to no-op when you’re done and running extra iterations. Basically, this option is to rewrite your model so it doesn’t have Python-level control flow anymore (the conditional can either be done host or GPU side).

**Pros:**

- You get a single, full graph that works for all possible branches
- You are able to optimize inside and outside of the control flow

**Cons:**

- You have to rewrite your model
- For unrolling, if you are close to being CPU-dispatch bound, unrolling and running with zero size could push you over the brink (as zero size dispatches are still not free)
- For conditional operators, unconditionally both branches increases the compute you need to do, which can be bad if you are compute-bound.

# Control flow HOP

torch has special structured control flow operators that avoid unrolling large loops or needing to execute both branches of a control flow statement. If you’re familiar with JAX, these are very similar to the JAX equivalents. They have specific constraints that allow them to be directly compilable by torch.compile. For example, torch.cond accepts two functions (a true_fn and a false_fn) for the two branches and requires that outputs of each function must have the same properties (e.g. shape, dtype).

So far, we have the following “higher-order” operators (HOPs):

- [torch.cond](https://docs.pytorch.org/docs/stable/generated/torch.cond.html) (differentiable)
- [torch.while_loop](https://github.com/pytorch/pytorch/blob/a714437093ed196eee28f7de454cf4c41badc098/torch/_higher_order_ops/while_loop.py#L133-L198) (not yet differentiable)
- [torch.\_higher_order_ops.scan](https://github.com/pytorch/pytorch/blob/a714437093ed196eee28f7de454cf4c41badc098/torch/_higher_order_ops/scan.py#L103-L169) (differentiable, ignore the docs)

These are relatively new, have been used in torch.export for inference, but have not been battle tested for training or performance.

The semantics of these control flow operators are as follows:

``` python
def cond(pred, true_branch, false_branch, operands):
    if pred:
        return true_branch(*operands)
    else:
        return false_branch(*operands)

def while_loop(cond_fn, body_fn, carried_inputs):
    val = carried_inputs
    while cond_fn(*val):
        val = body_fn(*val)
    return val

def scan(combine_fn, init, xs, length=None):
    carry = init
    ys = []
    for x in xs:
        carry, y = f(carry, x)
        ys.append(y)
    return carry, np.stack(ys)
```

**Pros:**

- You get a single, full graph that works for all possible branches
- You are able to optimize inside and outside of the control flow

**Cons:**

- You have to rewrite your model.
- The control flow HOPs are structured: they have specific constraints on the functions (true_fn, false_fn (cond) or body_fn (while_loop)) that can be passed to them. One such constraint is that these functions may not mutate any of their inputs. This may make rewrites difficult because you have to think about code in a “functional”, JAX-like way.
- Still WIP and they have some quirks especially for training. For example, the backward pass of torch.scan currently requires re-computing the forward pass (instead of just saving intermediates from each iteration of scan).

# CFG over FX graphs

If FX graphs give you basic blocks, you can use them as building blocks for a language that does support conditionals, stringing them together with basic blocks. In fact, Helion, a kernel DSL language, does exactly this, as it is common to need to directly write data-dependent conditionals and loops when writing kernels (it otherwise uses all PyTorch API functions, similar to conventional FX graphs). To do this, you would need to write your own Python frontend that parses Python directly to generate the CFG. TorchScript also does this, but TorchScript frontend is unmaintained and we don’t recommend using it (and it also doesn’t generate FX graphs by default.)

**Pros:**

- You get a single graph that works for all possible branches
- You are able to optimize inside and outside of control flow
- In principle, you can write exactly the control flow you want

**Cons:**

- You have to write the frontend, we don’t have one ready for you (TorchScript is not it, you’re princess is in another castle)
- If your language looks too much like Python and too general purpose, prepare to get on the endless treadmill of feature requests for adding “just one more Python feature” (can we have lists? dataclasses? etc etc) in the frontend (it is more tractable for Helion, as it’s not a general purpose language.)
