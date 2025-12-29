# ImpLam

Impure Lambda (as of yet, it's totally pure!)

## How to run

Run ghci Main.hs

Then write "main"  and enter.

Or...

Run ghc Main.hs

Then run Main (./Main, for example)

## Quick note

The symbol \ will be used in place of the symbol λ (lambda symbol).

The symbol β is Beta.

The symbol α is Alpha.

The symbol η is Eta.

The symbol ω is Omega.

The symbol % will be used for describing the relation "is identical to" (modulo symbol).

For example:

M % N

means: "M is identical to N"

Check the section for alpha equivalence, which is the relation being described here, via % (the modulo symbol).
I would suggest shortly reading about what modulo in mathematics means. It has the same purpose for many different things (or types of relation, *winks*)
Also, lowercase refers to variables and uppercase to terms of any kind. The following are possibilities:

M % \x.x

N % \y.y

M % N

a % a


a \%\ b  (is not identical...)

a \%\ M

## Syntax of Lambda Terms for ImpLam

Variables are single character, and may be followed by any number of apostrophes '.
Parenthesis are as free to be used as they may be. Syntatic sugar is a thing, therefore you may write either:

\x.\y.xy

Or:

\xy.xy

Likewise, for applications parentheses are only needed for when you want to apply on the right, for example:

x(yz)

Otherwise, if no parentheses are added as follows:

xyz

The following is inferred:

(xy)z

Another example:

xyzabc % ((xy)z)abc % ((((xy)z)a)b)c

Regarding abstractions, again:

\x.\y.\z.zyx % \xy.\z.zyx % \xyz.zyx

Also, be careful when you apply to an abstraction.
This:

\xyz.xyz

Is not the same as:

(\xyz.xy)z

One last note:

You may add spaces as you like. For example:

(\xyz.abc) is the same as (\x y z.a b c)


## Beta and Eta Reductions

Both Beta and Eta reductions are implemented.
A Beta reduction is when you have a term as follows:

(\x.M)N

In which case, you may do the following conversion:

(\x.M)N ->β M\[x -> N]

(as in, for any x in M, x may be substituted by N, where M and N are both lambda terms of any form)

Thus, the following is a beta reduction:

(\x.x)y ->β y

In this case, x is M and y is N.

(\x.xxz)(yya) ->β (yya)(yya)z

Here, xxz is M and yya is N.

Each abstraction \x. receives a single term, so:

(\x.\y.yx)ab % (\xy.yx)ab ->β (\y.ya)b -> ba

Now, regarding the Eta reduction, it must come as natural that whenever in:

(\x.Mx)

x does not occur in M as a free variable (this will be explained further in this README), we can simply do the following:

(\x.Mx) ->η Mx

The reason this can be accomplished, is that the only purpose of x was to be applied to M, and if we ever apply anything to:

(\x.Mx)

then it will appear in front of the term, which once the substitution is made, it will be exactly the same as having:

Mx

For example, via Beta reduction:

(\x.Mx)y ->β My

And via Eta reduction:

(\x.Mx)y ->η My

(particularly, here the only thing we change is (\x.Mx), where as for the previous case we changed the entire term (\x.Mx)y.

A sidenote:

You may say that you're reducing a term n amount of times via the following notation:

M ->3 M'

N ->3β N'

And if you want to reduce until no more reductions are possible, you use an asterisk *, as such:

M ->* M'

## Free Variables and Bound Variables

Free Variables are variables which do not occur in an abstraction inside the term.
Bound Variables are variables which occur in an abstraction inside the term.

For example:

\x.x

Here, x is a bound variable because it's being abstracted (hint: look at the \) and it occurs inside the body of the abstraction.

Now:

\x.y

Here, x is not longer a bound variable as it does not occur in the body of the abstraction, although now we have y as a free variable, since it occurs in the term but is not tied to any abstraction.

\abc.abdf

In the latter term: d, f are free variables and a, b are bound variables. c is not any of the two.

## Alpha Equivalence and Alpha Conversion

Alpha equivalence is when one term can be converted into another (those two terms will be alpha equivalent in such a case) if and only if you can accomplish that by changing the name of the bound variables (and their respective abstractions).
Achieving it via M...N or N...M is the same. The direction is not important.

For example:

\x.x %α \y.y

\zav.zds(aagd)v %α \xyz.xds(yygd)z

Moreover, there's a name for when we change the name of bound variables: Alpha Conversion.

This is an alpha conversion:

\xy.xy ->α \zy.zy ->α \zb.zb

An alpha conversion is a single step for the whole which connects two terms by an Alpha Equivalence.

## Normal Form of a Lambda Term

Essentially, a Normal Form of a Lambda Term is when you can't reduce a Lambda Term anymore.

For example, terms in Normal Form are:

\x.x

y

\abc.asd

aa

Terms that are NOT in Normal Form (there are reductions which we can apply yet):

(\x.x)y

(\x.xx)(\x.xx)

(\x.xxx)(\x.xxx)

(\xyz.zyx)abc

## Normal and Applicative Orders of Reduction

An order of reduction is how you algorithmically choose how you reduce a term (usually, by this we mean Beta Reduce).
The Normal Order of Reduction is, in practice, when you only reduce the terms after they have been applied, if such a thing is possible.
More concretely, it's when you choose the leftmost, outermost application, before any other, to reduce.

Check it out (Normal Order of Reduction):

(\x.xy)((\ab.ba)lk) ->β ((\ab.ba)lk)y

If you keep reducing it, you get:

(lk)y % lky

For those who know, this is the same as call-by-name evaluation.

Now, when it comes to Applicative Order of Reduction, which is the same as call-by-value evaluation, it's when you reduce the leftmost, innermost application before any other. 

Therefore, Applicative Order of Reduction is as follows:

(\x.xy)((\ab.ba)lk) ->2β (\x.xy)(lk) ->β (lk)y % lky

There are some relevant properties to these two. Normal Order of Reduction always guarantees that we find the Normal Form of a Lambda Term, if there is any. As for Applicative Order of Reduction, it offers no such guarantees other than the fact that we only evaluate the right side of the application once (whereas for Normal Order of Reduction, we may have to evaluate some terms more than once).

For example, in:

(\x.y)((\x.xx)(\x.xx))

If we reduce this in Normal Order of Reduction, the following is achieved:

->β y

However, if we do it in Applicative Order of Reduction, we never get out of this term (I'll leave this one for you to understand by yourself :)):

(\x.y)((\x.xx)(\x.xx))

Here's a better example, via Applicative Order of Reduction (for Normal Order, it would reduce immediatly to y):

(\x.y)((\x.xxx)(\x.xxx)) ->β (\x.y)((\x.xxx)(\x.xxx)(\x.xxx)) ->β (\x.y)((\x.xxx)(\x.xxx)(\x.xxx)(\x.xxx)) ->β to infinity and beyond|

In conclusion, you may want to pick Applicative Order of Reduction if you're looking for getting to the Normal Form Quicker, but beware: you need to make sure you can get there!

## A Suggestion regarding Non-Termination

If you really want to make sure a Lambda Term really does not terminate, try proving it via induction proof! It's the best, and will likely suit the problem at hand!

## A Couple of Somewhat Interesting Lambda Terms 

By the way, the term:

ω % (\x.xx)(\x.xx)

Is generally known as Omega. It never changes it's form no matter how you attempt to reduce it. There's only one possible reduction anyway, so this is easy to confirm.

And as for:

(\x.xxx)(\x.xxx)

I have no idea if it has a known name, but it's forever expanding linearly.

This type of terms is fundamental to Lambda Calculus. They are used for Y Combinators, which I will explain further in this short, compact guide. Also, you can use them to create, for example, a term which expands it's size exponentially.
Here:

(\x.x(xx))(\x.x(xx))

You may try these out in ImpLam ;)

Just be careful not to break anything!

## Equality

I will not delve deep into equality of Lambda Terms. There are some properties to it, such as Reflexivity, Symmetrical and Transitivity relation of equality, such that:

M = M

M = N, then N = M

M = N and N = L, then M = L

There are some rules as well - the rules of compatibility:

M = N, then ML = NL

M = N, then LM = LN

M = N, then \x.M = \x.N

Also, there's an equality if and only if you can reach N starting from M, via any number of Beta Reductions, in any direction, just like with alpha equivalence.

That said:

(\x.x)y ->β y

Therefore:

(\x.x)y = y

Another, similar, example:

(\x.z)y ->β z

(\x.z)y = z

Considering "any number of Beta Reductions" includes 0 Beta Reductions, we naturally have:

(\x.x) = (\x.x)

y = y

...

And so on. It should come as natural that the properties and rules are verified by this definition of equality of Beta Reductions in any direction, in any amount.

# The following are going to be written some other time! Currently, they are at best incomplete.

## Church Numerals (encoding of numbers)

Basically:
0 % \fx.x
1 % \fx.fx
...
n % \fx.(f^n)x % \fx.f...fx

Such that for n, f is applied n times to x, one after another!

Functions for addition, subtraction, successor, exponentiation, etc can be created!

Lambda Calculus is Turing Complete, therefore what can be computed, will be describeable in Lambda Calculus! (as long as it's untyped, in which case it becomes total but no longer is Turing Complete; there are perks to this nonetheless).
I might talk a little about this on a later topic.


## Encoding of various other structures (such as lists, booleans, pairs, etc)

I will not write this extensively (as I have not done so for Church Numerals) as I'm running out of time as of the moment of writing this. Some very short examples (sorry, I need sleep):

true % \xy.x
false % \xy.y
if % \fxy.fxy


## Y Combinator and Recursion

I will explain this one sometime soon. In anycase, here's its definition:

Y % \f.(\x.f(xx))(\x.f(xx))

It's meant to receive a "function" and substitute it there. It will allow for the following equation to be fulfilled, F being any function:

Y F % F (Y F)

Don't forget that the spaces are redundant, this is the same as what's above:

YF % FYF

## Typing, Turing Completeness, Total Lambda Calculus, etc OVERVIEW ONLY

Complicated and takes a while to think through how not to extend myself too far (also in order to avoid spouting nonsense).

# What is to be done (very much generally speaking)
Eta Reductions (should be quick)

The interface should be intuitive enough, although totally unappealing and forcing. In any case, the interface will be grealy improved sometime.

Currently, the only form of reducing is not the most efficient one (it runs the AST for each reduction!). The optimization will only cover cases when you only want to see printed the Normal Form (final result).

Soon, I'll add a chain reducing function (only traverses teh AST once).

I will also add a way of showing whether it was an eta or a beta reduction done for each step.

Many other improvements will be made, for example environment variables and libraries.

More control and flexibility for the interface. And so on...

