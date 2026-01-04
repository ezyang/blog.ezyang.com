---
title: "Monad Medley"
date: 2011-02-01 12:33:45
slug: 
draft: true
categories: [Miscellaneous]
---

> One year ago: [Adventures in Three Monads](/2010/01/adventures-in-three-monads/)

Here are a few quirky, application specific monads.

# The Fuel Monad

Imagine that your monad had a fuel supply, and when the fuel ran out, the monad stopped running. That’s what the Fuel monad from [Hoopl](http://hackage.haskell.org/package/hoopl) does:

    class Monad m => FuelMonad m where
      getFuel :: m Fuel
      setFuel :: Fuel -> m ()

    type Fuel = Int

    fuelRemaining :: FuelMonad m => m Fuel
    withFuel :: FuelMonad m => Maybe a -> m (Maybe a)

It’s actually an ordinary `State Int` monad under the hood, but Hoopl uses it in a very interesting way: by checking if there's enough fuel before performing operations, we can then vary the amount of fuel we give to the monad to make it run longer or shorter. In the case of optimization passes, that means we can figure out which optimization pass is buggy by finding out how much fuel we can give to the monad before the program breaks.

# The Orc Monad

Orc, short for orchestration, is a mash up of nondeterminism and IO.
