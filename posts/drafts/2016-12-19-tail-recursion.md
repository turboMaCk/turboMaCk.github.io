---
title: Deeper Dive To (Tail) Recursion - 2 Tail Calls
subtitle: How Your Head Calls Tail... Recursively
description: Optimizing recursive functions using tail calls.
---

Time to time someone raises concerns about performance of recursive algorithms in some of many conversations I have with my colleagues
friends or folks I meet on some local meetup. Even though this might seems like all clear field for some of you
I've found that there is many confusion going on and all that terminology people more familiar with topic are using doesn't
help to explain all the stuff clearly and therefor I still think it's worth spending some time exploring what recursion and tail recursion
really means and how to use them in practice.

***Note:** One occasion I've been explaining how tail recursion works was in hotel room with one of my friends and colleagues.
Original conversation was about [Prolog](https://en.wikipedia.org/wiki/Prolog) therefor I've wrote original examples in Erlang (which has similar syntax). Anyway for purpose of this
article I'll also try to add few examples in different languages to make it easier to follow for people with no Erlang/Prolog experience.*
