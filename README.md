# purescript-ssrs

Stack-safe recursion schemes through
[dissectible](https://github.com/PureFunctor/purescript-dissect) data
structures. This package provides the same API that
[matryoshka](https://github.com/purescript-contrib/purescript-matryoshka)
has minus the `Recursive` / `Corecursive` type classes, distributive
laws, and generalized recursion schemes that make use of said laws.

## Installation

Using `spago`:

    $ spago install ssrs

or if not present within the current package set, add it to
`packages.dhall`:

``` dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall
        sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let overrides = {=}

let additions =
      { dissect =
        { dependencies = [ ... ]  -- copy dependencies from spago.dhall
        , repo = "https://github.com/PureFunctor/purescript-ssrs.git"
        , version = "<insert-desired-revision-here>"
        }
      }

in  upstream // overrides // additions
```

## Rationale

I originally encountered the implementation of a tail-recursive
catamorphism described in the paper [Clowns to the Left of me, Jokers to
the Right (Pearl): Dissecting Data
Structures](https://dl.acm.org/doi/abs/10.1145/1328438.1328474) by Conor
McBride. After a few more days of research, I eventually figured out how
to transpose said algorithm from a catamorphism into an anamorphism, and
subsequently, I've also synthesized a hylomorphism by fusing the two
algorithms together.

For the greater family of recursion schemes however, I had a bit of help
using <https://github.com/willtim/recursion-schemes/> to derive them
from the `cata`, `ana`, and `hylo` schemes. I've decided to compose the
other recursion schemes against these three instead defining them
specifically in order to reduce the overall size of the package.
Likewise, it's also easier to derive schemes these way as they generally
followed the common pattern of: *turn this GAlgebra into an Algebra,
feed it into cata, and unwrap the result afterwards* or *turn this
GCoalgebra into a Coalgebra, wrap some input, then feed it into ana*.

## An Iterative Catamorphism Machine

In the same vein as [the quick primer to
dissect](https://github.com/PureFunctor/purescript-dissect#quick-primer-on-dissect),
I'll be assuming that the reader has some familiarity working with
fixed-point functors, recursion schemes, and dissections. Similarly, I
won't dwell too much on explaining other concepts in-depth.

The original paper implements a tail-recursive catamorphism using four
separate functions that form a tail-recursive loop. An optimizing
compiler can take these four functions and identify the tail-recursion
that forms within them.

``` purescript
cata ∷ ∀ p q v. Dissect p q ⇒ (p v → v) → Mu p → v
cata algebra t = load algebra t Nil

load ∷ ∀ p q v. Dissect p q ⇒ (p v → v) → Mu p → List (q v (Mu p)) → v
load algebra (In pt) stk = next algebra (right (Left pt)) stk

next ∷ ∀ p q v. Dissect p q ⇒ (p v → v) → Either (Tuple (Mu p) (q v (Mu p))) (p v) → List (q v (Mu p)) → v
next algebra (Left (t, pd)) stk = load algebra t (pd : stk)
next algebra (Right pv) stk = unload algebra (algebra pv) stk

unload ∷ ∀ p q v. Dissect p q ⇒ (p v → v) → v → List (q v (Mu p)) → v
unload algebra v (pd : stk) = next algebra (right (Right (pd, v))) stk
unload algebra v Nil = v
```

Unfortunately, PureScript is not one of those compilers (yet, but
hopefully), and so I had to fuse them together to help the compiler with
tail-call optimization. This definition is a bit terse, but I'll try to
explain the intuition behind it in the next few paragraphs.

``` purescript
cata ∷ ∀ p q v. Dissect p q ⇒ Algebra p v → Mu p → v
cata algebra (In pt) = go (pluck pt) Nil
  where
  go :: Either (Tuple (Mu p) (q v (Mu p))) (p v) → List (q v (Mu p)) → v
  go index stack =
    case index of
      Left (Tuple (In pt') pd) →
        go (pluck pt') (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (plant pd (algebra pv)) stk
          Nil →
            algebra pv
```

`Dissect` allows us to build iterative machines that model the traversal
of a recursive structure. What we've essentially done at this point is
model a catamorphism as an iterative machine that uses a stack for its
state. Before we proceed further, let's take a look at our toolkit and
how we can use it to build an iterative catamorphism machine from
scratch.

1.  We have some recursive data type `Mu p ~ p (Mu p)`, and an algebra
    `p v → v`.

2.  We want to take `Mu p ~ p (Mu p)` and pluck all seats of `Mu p` and
    plant the resulting holes with `v`. This allows us to then call our
    algebra on the resulting `p v`.

3.  When plucking a `Mu p ~ p (Mu p)` we end up with two choices:

    1.  We receive a fruit `Mu p ~ p (Mu p)` and a dissection
        `q v (Mu p)`. We have to process this fruit as we did in in step
        2, and store the dissection somewhere until we can fill it with
        the processed fruit.

    2.  We receive a flower `p v` that we can call our algebra on,
        giving us a `v`. We can then use this value to fill in the
        dissections that we've stored in step 3.1 or if we have none, we
        can simply return the value.

If this sounds like an iterative traversal of a tree to you, then you're
gaining the right intuition, otherwise, it's good to know at this point
in time. Iterative traversals make use of a stack in order to keep track
of items being visited at each level. What we're interested in however
is storing dissections in the stack and popping them out once we have
the appropriate values to fill them in with later.

Let's contextualize this into a specific type:

``` purescript
data TreeF n = Leaf | Fork n n

type Tree = Mu TreeF
```

Suppose that we have the following structure and an empty stack:

``` purescript
Fork [ Fork [ Leaf - Leaf ] - Leaf ]

Stack []
```

By calling `pluck` on this structure, we dissect it into two parts. For
now, we push the dissection onto the stack.

``` purescript
> pluck $ Fork [ Fork [ Leaf - Leaf ] - Leaf ]
Fork [ Leaf - Leaf ], Fork [ () - Leaf ]

> push $ Fork [ () - Leaf ]
Stack [ Fork [ () - Leaf ] ]
```

We then call `pluck` on the result, and we end up with another
dissection that we have to push.

``` purescript
> pluck $ Fork [ Leaf - Leaf ]
Leaf, Fork [ () - Leaf ]

> push $ Fork [ () - Leaf ]
Stack [ Fork [ () - Leaf ]
      , Fork [ () - Leaf ]
      ]
```

We call `pluck` again on the result, but this time, we reach a base case
that our `algebra` gladly accepts. Furthermore, we can `plant` this
value in the top-most dissection in our stack.

``` purescript
> pluck Leaf
Pv

> algebra Pv
V

> pop Stack
Fork [ () - Leaf ]

> plant $ Fork [ () - Leaf ] $ V
Leaf, Fork [ V - () ]
```

By planting a value, we get the next element to pluck and the next
dissection to push. Since we receive yet another base case, we're able
to plant it immediately to the top-most dissection. Finally, we've
managed to replace all recursive seats and turn them into collapsed
values. Likewise, we can call our algebra on this structure to collapse
it further.

``` purescript
> push $ Fork [ V - () ]
Stack [ Fork [ () - Leaf ]
      , Fork [ V - () ]
      ]

> pluck Leaf
Pv

> algebra Pv
V

> pop $ Stack
Fork [ V - () ]

> plant $ Fork [ V - () ] $ V
Fork [ V - V ]

> algebra $ Fork [ V - V ]
V
```

We're not quite done yet however, as we still have items in the stack.
I'll let the pseudo-REPL do the talking from here on, but in the end of
this session, we should have our final result.

``` purescript
> pop $ Stack
Fork [ () - Leaf ]

> plant $ Fork [ () - Leaf ] $ V
Leaf, Fork [ V - () ]

> push $ Fork [ V - () ]
Stack [ Fork [ V - () ]
      ]

> pluck Leaf
Pv

> algebra Pv
V

> pop $ Stack
Fork [ V - () ]

> plant $ Fork [ V - () ] $ V
Fork [ V - V ]

> algebra $ Fork [ V - V ]
V
```

We can express this imperative algorithm in pseudocode like so.

``` purescript
LET Index = Pluck(Start)
LET Stack = []

LOOP
  IF Index IS [Next, Hole]
    Push(Hole, Stack)
    Index = Pluck(Next)
  ELSE IF Index IS Base
    IF Pop(Stack) IS Hole
      Index = Plant(Hole, Algebra(Base))
    ELSE
      DONE Algebra(Base)
    END
  END
END
```

## From Catamorphisms to Anamorphisms and Hylomorphisms

Traditionally, catamorphisms can be implemented as a series of function
compositions that go from `Mu
p` into a `v`. The code block below adopts the Haskell definition listed
in [Recursion Schemes, Part II: A Mob of
Morphisms](https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html).
Note that in order to actually work with this definition, we'd have to
perform some indirection as to not implicit perform left-recursion in
`cata`; see the implementation in
[matryoshka](https://github.com/purescript-contrib/purescript-matryoshka)
for more details.

``` purescript
cata :: forall f a. Functor f => (f a -> a) -> (Mu f -> a)
cata f = unwrap >>> fmap (cata f) >>> f
```

Anamorphisms are the dual of catamorphisms; likewise, their coalgebras
and algebras are also duals. If we flip all relevant arrows in this
definition, we end up with:

``` purescript
ana :: forall f a. Functor a => (a -> f a) -> (a -> Mu f)
ana f = wrap <<< fmap (ana f) <<< f
```

We can apply the same principle with our iterative catamorphic machine.
If we contextualize *flipping the arrows* in our implementation, we find
out that replacing all instances of `Mu p` unwrapping is replaced with a
call to `coalgebra`, while all calls to `algebra` are replaced with
`Mu p` wrapping.

``` purescript
ana ∷ ∀ p q v. Dissect p q ⇒ Coalgebra p v → v → Mu p
ana coalgebra seed = go (pluck (coalgebra seed)) Nil
  where
  go :: Either (Tuple v (q (Mu p) v)) (p (Mu p)) → List (q (Mu p) v) → Mu p
  go index stack =
    case index of
      Left (Tuple pt pd) →
        go (pluck (coalgebra pt)) (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (plant pd (In pv)) stk
          Nil →
            In pv
```

For the pseudocode:

``` purescript
LET Index = Pluck(Coalgebra(Seed))
LET Stack = []

LOOP
  IF Index IS [Next, Hole]
    Push(Hole, Stack)
    Index = Pluck(Coalgebra(Next))
  ELSE IF Index IS Recr
    IF Pop(Stack) IS Hole
      Index = Plant(Hole, Mu(Recr))
    ELSE
      DONE Mu(Recr)
    END
  END
END
```

Hylomorphisms can be defined as the composition of a catamorphism and an
anamorphism. While convenient to define, we unfortunately have to pay
the cost of keeping the entire intermediate structure built by the
anamorphism before it can be folded by the catamorphism. We can
alleviate this by "fusing" these two loops together to form a single
tight loop. Our definition for an iterative hylomorphism machine looks
like:

``` purescript
hylo ∷ ∀ p q v w. Dissect p q ⇒ Algebra p v → Coalgebra p w → w → v
hylo algebra coalgebra seed = go (pluck (coalgebra seed)) Nil
  where
  go :: Either (Tuple w (q v w)) (p v) → List (q v w) → v
  go index stack =
    case index of
      Left (Tuple pt pd) →
        go (pluck (coalgebra pt)) (pd : stack)
      Right pv →
        case stack of
          (pd : stk) →
            go (plant pd (algebra pv)) stk
          Nil →
            algebra pv
```

If we analyze the implementation, what we've done is replace the
"planting" branch in our anamorphism with the branch that the
catamorphism machine uses. In turn, we're able to unfold structures and
fold them at each recursive level, instead of waiting for the entire
recursive structure to unfold.

As for the pseudocode:

``` purescript
LET Index = Pluck(Coalgebra(Seed))
LET Stack = []

LOOP
  IF Index IS [Next, Hole]
    Push(Hole, Stack)
    Index = Pluck(Coalgebra(Next))
  ELSE IF Index IS Base
    IF Pop(Stack) IS Hole
      Index = Plant(Hole, Algebra(Base))
    ELSE
      DONE Algebra(Base)
    END
  END
END
```
