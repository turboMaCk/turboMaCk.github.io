---
title: Deep Dive To (Tail) Recursion - 1 Intro
subtitle: Recursion Is The Easiest Way
# subtitle: How Your Head Calls Tail... Recursively
description: Let's talk about what you should know before we write first tail recursive function.
---

Time to time someone raises concerns about performance of recursive algorithms in some of many conversations I have with my colleagues
friends or folks I meet on some local meetup. Even though this might seems like all clear field for some of you
I've found that there is many confusion going on and all that terminology people more familiar with topic are using doesn't
help to explain all the stuff clearly and therefor I still think it's worth spending some time exploring what recursion and tail recursion
really means and how to use them in practice.

Before we start talking about tail recursion I think it's fair to just recall some basics of recursive functions.

## Recursion

Recursion can be found in many areas of mathematics and computer since. I'm not going to exactly explain what recursion in this article
but to make this article complete there is a [link to wikipedia page](https://en.wikipedia.org/wiki/Recursion) where you can
find more in case you're interested.

> If I have to rate different mathematical concept according to its simplicity & practical applicability,
> then I will give '9 out of 10 rating' to 'Recursion / recurrence relation'.
> 90 % of my mathematical work involve this single principle.
> ~ Vitthal Jadhav (Mathematician)

First lets recall few places when you can come across recursion like:

#### Shell

```shell
rm -r folder_name
```

where `-r` means recursive - apply `rm` command to all sub files and sub folders.

#### Algebraic Data Types

```haskell
data RoseTree a = Node a [RoseTree a]
```

where type definition might be recursive (type containing itself)

#### Algorithms

```javascript
function length(arr) {
    if (!arr[0]) {
        return 0;
    }
    arr.pop();
    return 1 + length(arr);
}
```

where function contains call to itself.

***Note:** `length()` implementation is just used for demonstration purposes. Please don't try to use it in real world!
There are many problems with the code above. Performance will be really bad and also function will cause mutations
in original array which becomes empty after being passed to the function due to `pop()` call! If you thinking about
using something similar in your code base **`fuckUpLength`** would be probably better name for that function:D*

**Let's just say recursion is quite useful and popular technique you can find on many places. So we can move on.**

## Looooooooops

Loops are fairly popular technique which is used in place of recursion in many programs.
The idea is simple - instead of dealing with function calling itself we loop "instructions" we want to apply.
This completely hides original recursive definition, solves problem with stack overflow (recursive function can't return
until all it's sub-calls returns) and is generally much closer to what we want our hardware to do.
Let's have a look at `length()` implementation using loop.

```javascript
function length(arr) {
    var len = 0;
    while(arr[0]) {
      len++;
      arr.pop()
    }
    return len;
}
```

***Note:** Even this implementation of length is bad. Function still performs mutation on array using `pop()`.
Please do not use such a code in real world.*

Loops are so widely used technique that many programmers kind of defaults to it. This is maybe because most generally used
languages derives so much from C (think about C++, Java, C#, JavaScript...). But there is a downside to it though.
Even it might seems easier to use loops over recursion to many of us it's definitely not easier at all!

There are so many things you can fucked up in `for` loop that it even doesn't make sense to try name them.
Concept of loops also heavily relies on mutations which is hard to deal with especially when concurrency (in multi threaded systems)
is involved. This is nothing new. There are many concepts even in imperative languages to solve these issues
like for example [iterators](https://en.wikipedia.org/wiki/Iterator) which I'm not really going to cover in this article but
you can find tons of material about them on [the internet](https://duckduckgo.com/?q=iterator).

Nevertheless I still think that recursion over power any other concept and should be the thing we defaults to instinctively.
It's maybe true that most people with some programming experience thinks in terms of looping rather than recursion but
the fact is that this concept do not scale much for human brains. Even in [dynamic programming](https://en.wikipedia.org/wiki/Dynamic_programming) you so often start
by recursive definition and then later start translating implementation to loop just to gain better performance characteristics.
Why is that? Simply because it's much easier to solve complex problems using recursion than it is by using looping.

***Note:** In case you're interested learning more about dynamic programming I can recommend to look at
[public MIT's Introduction to Algorithms course](https://www.youtube.com/watch?v=OQ5jsbhAv_M&list=PLfMspJ0TLR5HRFu2kLh3U4mvStMO8QURm).*

There is still one issue using technique like dynamic programming in my opinion. Once you optimize code to loops
it's stay that way. This means every time you or anyone else will need to tweak something in you're code I'll need to walk through
code you was no able to put down from head in first place. You can imagine this won't be pleasing experience.

Wouldn't it be nice to have recursive implementation and hand optimization to compiler instead?
Good news folks! There are compilers capable of such optimization out there! The only requirement on your side
is to keep your implementation [tail recursive](https://en.wikipedia.org/wiki/Tail_call)!

I think now we have good background to problem of tail recursion. In next part we're going to look what exactly tail recursion is
and how it works.
