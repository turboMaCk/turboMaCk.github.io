---
title: Hybrid Types in TypeScript
subtitle: Being Dynamic With Type Checker
description: Using functions with custom interfaces in TypeScript
reddit: r/typescript/comments/5h15an/hybrid_types_in_typescript
tags: typescript, javascript, d3.js
---

Yesterday I've been refactoring one of our internal library in [GWI](globalwebindex.net) from JavaScript to TypeScript.
As a JavaScript veteran I like to use some edgy non-conventional & pre-ES 2015 coding styles.
I'm not using `prototype`, `class` or `this` much often.
I'm rather using [Factory Constructor Pattern](http://javascript.info/tutorial/factory-constructor-pattern)
quite often as well as [Higher Order Functions](https://en.wikipedia.org/wiki/Higher-order_function) for simulating [currying](https://en.wikipedia.org/wiki/Currying)
and like to with objects and play with scopes. Now you might think that this is silly. No one will understand my code and using truly
private functions makes it harder to extend functionality. And you're right:D Anyway I know that I can write more reliable
code more quickly and from my experience when code is reliable enough not many people will need to change it.
And when they do they are mostly experienced and are able to understand it.
Also I like private functions. If you miss any functionality or abstraction it's always good idea to add it directly to library
or write your own than hack it. Anyway this article is not about these patterns
and how to use them but rather about how it feels when you put [TypeScript](https://www.typescriptlang.org) in the mix.
If you want to learn more about these patterns [DuckDuckGo](https://duckduckgo.com/) some other article.

First let me explain one thing. I'm not writing TypeScript day to day. But when I do I'm mostly exploring edges of what it can do.
I'm writing a lot of ES 2015 ECMAScript in work and [Elm](elm-lang.org) for fun (what is slowly changing since we already shipped first
feature written in Elm elm as part of our production [Ember.js](ember.js) app).

# Problem

The idea is this. We have got our internal system for charts written in [D3.js](https://d3js.org/). Some parts of this are
[open-sourced and available on GitHub](https://github.com/GlobalWebIndex/d3scription). In GWI we have got whole charts written in pure D3 that
are sharing common interface so on application layer you're basically just dynamically switching factory functions for charts based on
based on chart type and everything works like a magic. However testing visualization layer is hard or even impossible. Wouldn't it be
a good idea to have at least type check for these interfaces? I think it would!

For purpose of this tutorial I've picked one smaller part rather than whole chart.
D3 itself comes with component called [d3.svg.axis](https://github.com/d3/d3/blob/master/API.md#axes-d3-axis).
However sometimes it doesn't fit your needs so you'll need to implement your own solution.
Lets say we want to implement custom axis which will create ticks based on data we have so for each data-point it will create a tick on axis.
This is how we want to use our new component:

```javascript
const axis = exactAxis()
    .scale(scale)
    .data(someData)
    .tickFormat(d => `${d}`);

d3.select('.axis-group').call(axis);
```

As you can see I've chosen `exactAxis` as a name for this component. It's a `function`. However it returns some `Object`
that has at least `scale`, `data` & `tickFormat` methods. We're using chaining so at least `scale` and `data` should return object they are defined on.
Also on the last line we are using [d3.selection.call](https://github.com/d3/d3-selection/blob/master/README.md#selection_call)
which means that `axis` (thing returned by `tickFormat` call) needs to be `function`. This might mean that `tickFormat` returns some `function` instead of `object`
but that is silly idea. Then you will need to always call `tickFormat` as last method.
That said I think we can agree that:

- `exactAxis()` returns `function` (and **functions are Objects in JavaScript**) with all methods defined.
- Each method will return object it's called on (so we can chain method calls).

# Interface

Now we know what we need so lets define interface for out `exactAxis` function.

```javascript
import * as d3 from 'd3';

interface TickFormat extends Function {
    (any) : string;
}

interface ExactAxis extends Function {
    (g : d3.Selection<any>) : void;
    scale(scale : d3.scale.Ordinal<any, any>) : ExactAxis;
    data(data : any[]) : ExactAxis;
    tickFormat(fc : TickFormat) : ExactAxis;
}
```

As you can see there are some `any` used.
Maybe it's good idea to solve this using [Generics](https://www.typescriptlang.org/docs/handbook/generics.html).

```javascript
import * as d3 from 'd3';

interface TickFormat<T> extends Function {
    (data : T) : string;
}

interface ExactAxis<T> extends Function {
    (g : d3.Selection<any>) : void;
    scale(scale : d3.scale.Ordinal<any, any>) : ExactAxis<T>;
    data(data : T[]) : ExactAxis<T>;
    tickFormat(fc : TickFormat<T>) : ExactAxis<T>;
}
```

OK this is better. Now it's obvious that we are passing some data around.

There are still some `any` used for d3 parts but I think we can leave it.


# So Called Hybrid Types in TS

Now we can actually start implementing our axis component. This was the part I was not familiar till yesterday.
I was actually asking some friends who work with typescript daily how an interface like this can be implemented in typescript
but unluckily no one knew. I knew how this can be done in JS but that implementation did not satisfy tsc (compiler).
Than as all SW engineers I've turned my last hope to [documentation](https://www.typescriptlang.org/docs/handbook/interfaces.html)
and found part about [Hybrid Types](https://www.typescriptlang.org/docs/handbook/interfaces.html#hybrid-types).

> As we mentioned earlier, interfaces can describe the rich types present in real world JavaScript.
> Because of JavaScript’s dynamic and flexible nature, you may occasionally encounter an object that works as a combination of some of the types described above.

That's exactly what I was looking for!

Let's have a look at how we minimal "implementation" that satisfy our interface:

```javascript
function exactAxis<T>() : ExactAxis<T> {
    const axis = <ExactAxis<T>>function() {
    }

    axis.scale = function(scale : d3.scale.Ordinal<any, any>) : ExactAxis<T> {
        return axis;
    }

    axis.data = function(d : T[]) {
        return axis;
    }

    axis.tickFormat = function(fc : TickFormat<T>) : ExactAxis<T> {
        return axis;
    }

    return axis;
}
```

That's it! Actually this won't do anything but it's whole boiler plate we need.
Now comes the easy part. We can just simply implement logic (and that's always simpler than designing API, right?)

So as a bonus - This is one possible full implementation:

```javascript
function exactAxis<T>() : ExactAxis<T> {
    // Constants
    const TEXT_DELTA : number = 25;
    const WITHOUT_TEXT_DELTA : number = 12.5;

    // Instance variables
    let data : T[] = [];
    let tickFormat : TickFormat<T> = (d) => `${d}`;
    let scale;

    // Render

    const axis = <ExactAxis<T>>function(g) {

        // D3 always returns array
        // Lets render axis for every given group.
        g.each(function() {
            const $el : d3.Selection<any> = d3.select(this);

            // Prepare data for ticks
            const ticksData = data.sort((a, b) => Number(a) - Number(b))
                .reduce((acc, d) => {
                    const latestWithText = last(acc.withText);

                    // skip duplicates immediately
                    if (latestWithText && latestWithText === d) { return acc; }

                    // for first or not too close we add text one
                    if (!latestWithText || Math.abs(scale(latestWithText) - scale(d)) >= TEXT_DELTA) {
                        acc.withText.push(d);
                    } else {
                        // we add tick without text
                        const latestWithoutText = last(acc.withoutText);

                        // check for delta from latest with text
                        if (Math.abs(scale(latestWithText) - scale(d)) >= WITHOUT_TEXT_DELTA) {

                            // check for delta from latest without text
                            if (!latestWithoutText || Math.abs(scale(latestWithoutText) - scale(d)) >= WITHOUT_TEXT_DELTA) {
                                acc.withoutText.push(d);
                            }
                        }
                    }

                    return acc;
                }, { withText: [], withoutText: [] } );

            // Render

            // With text
            const withText = g.selectAll('.tick.tick--with-text').data(ticksData.withText);

            withText.enter().append('g')
                .attr('class', 'tick tick--with-text')
                .append('text')
                .style('text-anchor', 'middle');

            withText.transition()
                .attr('transform', d => `translate(${scale(d)}, 15)`)
                .select('text')
                .text(tickFormat);

            withText.exit().remove();

            // Without text
            const withoutText = g.selectAll('.tick.tick--without-text').data(ticksData.withoutText);

            withoutText.enter().append('g')
                .attr('class', 'tick tick--without-text')
                .append('line')
                .attr('x1', 0)
                .attr('x2', 0)
                .attr('y1', 0)
                .attr('y2', 5);

            withoutText.transition()
                .attr('transform', d => `trnaslate(${scale(d)}, 0)`);

            withoutText.exit().remove();
        });
    }

    // Public Interface

    axis.scale = function(newScale : d3.scale.Ordinal<any, any>) : ExactAxis<T> {
        scale = newScale;
        return axis;
    }

    axis.data = function(d : T[]) {
        data = d;
        return axis;
    }

    axis.tickFormat = function(fc : TickFormat<T>) : ExactAxis<T> {
        tickFormat = fc;
        return axis;
    }

    return axis;
}
```

*Note: If you read carefully you know that I'm not using `this` often. However for this example has one usage of `this`
to get element in d3's `each` method. It doesn't make sense to go against library API.*

*Note: This is really simplified implementation. Actual thing similar to this we have in our lib uses 3 types of ticks
(long text, short text, no text). This means also different interface for `FormatValue` an we are also always adding
line tick (without text) in middle of ticks with text. However I think this simpler example is better for purpose of this
article.*

# Don't Drink Too Much Kool-Aid

TypeScript maybe lets you express these kind of dynamic APIs but there is down side to it.
If you remove implementation for any method compiler won't complain even though your function
does not return valid `ExactAxis<T>` implementation. However if you make mistake in method implementation (change its types) it will fail
during compile time which seems as an improvement to pure JS version. That said if you want to play with something like this
It's usually good idea to **always start with boilerplate with all methods defined**.
