---
title: "For simplicity, assume that n is a power of 2"
date: 2012-11-26 15:36:38
slug: for-simplicity-assume-that-n-is-a-power-of-2
draft: true
categories: [Computer Science]
---

A common assumption that is made in divide-and-conquer proofs is that the size of the input *n* is a power of two. The benefit of this assumption is that we can divide *n* by two every step of the proof without bothering with any pesky remainders. Of course, real data is rarely a power of two; in such cases, we’d like to extend the algorithm to work in such cases too. Usually, this extension is considered “obvious”; but there is actually some choice here. For example, you could...

- *Pad the data to a power of two.* Simple and very easy to show correct, For example, in the [Karatsuba multiplication algorithm](http://en.wikipedia.org/wiki/Karatsuba_algorithm), *n* is the number of digits in a number, so to get to a power of two all we need to do is pad the number with zeros. This method does add a little bit of waste, though.
- *Have different sized recursive calls.* In many cases, the correctness proof is insensitive to whether or not the two recursive calls actually have the same size; in this case, you can be sloppy and punt the extra element to the next recursive call. For example, in [mergesort](http://en.wikipedia.org/wiki/Merge_sort), the merge step can easily handle lists that are of different sizes, and only depends on the sortedness of lists.
- *Decompose the problem into power of two size subproblems.* Every number has a binary representation, in which the ones represent size of two subproblems. If you can easily join these subproblems together, then you don’t need to modify your original algorithm at all.

I recently found a divide-and-conquer algorithm which is amenable to the third technique, but not the second. Furthermore, using the third technique makes it easy to incrementalize the algorithm: that is, begin computing the result even when it is not clear what the final result of *n*.
