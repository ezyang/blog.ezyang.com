---
title: "Polymorphic accumulators"
date: 2012-07-26 11:28:52
slug: 
draft: true
categories: [Miscellaneous]
---

A classic exercise for beginning functional programmers is this: how do you reverse a list using a (right) fold in linear time? The solution is quite clever: the accumulator must be a function, not a bare value:

    Prelude> foldr (\x k -> k . (x:)) id [1,2,3] []
    [3,2,1]

This fold builds up a function `[Int] -> [Int]`, and in order to get out our final reversed list we have to pass the function the empty list `[]`, to serve as the tail. (Another way of looking at this solution is that we are using difference lists which support efficient snoc).

Polymorphic accumulators
