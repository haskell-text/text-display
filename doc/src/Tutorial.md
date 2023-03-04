# Tutorial

This tutorial will teach you how to use the `Display` typeclass for your own data types.

## The Display typeclass

The `Display` typeclass is an interface to create user-facing output.
As opposed to the `Show` typeclass, it does not have to output Haskell syntax at all
time. This enables the programmer to separate concerns between textual output 
that needs to be parsable by derived instances of the `Read` typeclass, and 
output whose prime goal is to convey information to humans.

The main way to get started with it is to call the `display` function on any data type
that implements the `Display` typeclass. We are going to see how to implement it in
the next sections.

## Implementing the typeclass

The easiest way to implement the typeclass is to provide an implementation for its
`displayBuilder` method:

```haskell
import Data.Text.Display


data MyType = MyType Int

-- >>> display (MyType 32)
-- "MyType 32"
instance Display MyType where
   displayBuilder (MyType i) = "MyType " <> display i
```

But this can be quite time-consuming, especially if your datatype already has
an existing `Show` that you wish to reuse. In which case, you can piggy-back
on this instance like this:

```haskell
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
data AutomaticallyDerived = AD
  deriving stock Show 
  deriving Display
    via (ShowInstance AutomaticallyDerived) 
```

This derivation takes advantage of the [ShowInstance][ShowInstance] helper to reuse 
the `Show` instance of `AutomaticallyDerived`.
In this case, `show` and `display` will give the same result on the
`AutomaticallyDerived` datatype.

But let's say you want to redact an instance of `Display`? You can do it locally, through
the `OpaqueInstance` helper. It is most useful to hide tokens or passwords:

```haskell
data UserToken = UserToken UUID                           
 deriving Display                                         
   via (OpaqueInstance "[REDACTED]" UserToken)            
                                                          
display $ UserToken "7a01d2ce-31ff-11ec-8c10-5405db82c3cd"
-- => "[REDACTED]"                                              
```

By now you should be comfortable with the base concepts of `Display`. Have fun. :) 

[ShowInstance]: https://hackage.haskell.org/package/text-display/docs/Data-Text-Display.html#t:ShowInstance
