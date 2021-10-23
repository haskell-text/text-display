<p align="center">

<img src="./images/logo.svg" height=100 width=100 />
</p>

<p align="center">
<a href="https://github.com/Kleidukos/text-display/actions"></a>
  <img src="https://img.shields.io/github/workflow/status/Kleidukos/text-display/CI?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
</p>

<p align="center">
<a href='https://ko-fi.com/Q5Q327ZHW' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi1.png?v=3' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
</p>

> *A Typeclass for user-facing output*


## Description

The `text-display` library offers a way for developers to print a textual representation of datatypes that does not
have to abide by the rules of the [Show typeclass][Show].

If you wish to learn more about how things are done and why, please read the [DESIGN.md][./DESIGN.md] file.

## Examples

There are two methods to implement `Display` for your type:

The first one is a manual implementation:

```haskell
data ManualType = MT Int

-- >>> display (MT 32)
-- "MT 32"
instance Display ManualType where
  displayPrec prec (MT i) = displayParen (prec > 10) $ "MT " <> displayPrec 11 i
```

But this can be quite time-consuming, especially if your datatype already has
an existing `Show` that you wish to reuse. In which case, you can piggy-back
on this instance like this:

```haskell
{-# LANGUAGE DerivingVia #-}
data AutomaticallyDerived = AD
  -- We derive 'Show'
  deriving Show 
  -- We take advantage of the 'Show' instance to derive 'Display' from it
  deriving Display
    via (ShowInstance AutomaticallyDerived) 
```

But let's say you want to redact an instance of `Display`? You can do it locally, through
the `OpaqueInstance` helper. It is most useful to hide tokens or passwords:

```haskell
data UserToken = UserToken UUID                           
 deriving Display                                         
   via (OpaqueInstance "[REDACTED]" UserToken)            
                                                          
display $ UserToken "7a01d2ce-31ff-11ec-8c10-5405db82c3cd"
-- => "[REDACTED]"                                              
```
[Show]: https://hackage.haskell.org/package/base/docs/Text-Show.html#v:Show
