---
title: "The treachery of test methods"
date: 2017-04-30 21:21:56
slug: oo-applied-inappropriately-xunit-test-classes
draft: true
categories: [Miscellaneous]
---

**tl;dr**

xUnit-style test classes are a bad application of object-oriented programming, since the class never represents an encapsulated bundle of state that should be shared across test methods. (When this state is shared, that is a [test anti-pattern.](http://stackoverflow.com/a/333814/23845)) A more logical approach would be to define a class (or function, even!) per test. Unfortunately, in most languages, such an approach would be too verbose to be practical.

**What is an xUnit-style test class?** When I first started programming, someone told me about a little testing framework called [SimpleTest](http://www.simpletest.org/en/first_test_tutorial.html), which introduced me to the concept of test-driven development for the very first time. The story was simple: make a test class, write a test as a method in the class and then go write the actual code you were planning to write:

    class MyTest extends TestCase {
      public function testFoo() { ... }
      public function testBar() { ... }
      ...
    }

This idea of organizing your tests as methods
