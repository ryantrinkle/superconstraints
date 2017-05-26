## superconstraints

A way of inferring instance constraints given an instance.

Suppose you have a class like this:

```haskell
class C a

instance C a => C [a]
```

Normally, given `C [a]`, you cannot obtain `C a`; however, superconstraints allows you to add that capability:

```haskell
class HasSuper (C a) => C a

instance C a => C [a]
makeSuper "C [a]"
```

Then, you can retrieve the superconstraint by doing:

```haskell
case super (Proxy :: Proxy (C [a])) of
  Dict -> ...
```

The superconstraint dictionary will include all of the constraints required by the instance.

### Future Improvements

* Replace the crazy string argument to `makeSuper` with something more sensible
