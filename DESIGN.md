# Library design 

## A “Lawless Typeclass”[^1]

The `Display` typeclass does not contain any law. This is a controversial choice for some people,
but the truth is that there are not any laws to ask of the consumer that are not already enforced
by the type system and the internals of the `Text` type.

## The `Builder` type

As opposed to `Show`, `Display` takes the opportunity of pionneering better techniques when it comes
to the messy business of handling the textual representation of data structure, faithfully and efficiently.

The first of these choices is that it does not use the `Text` type up until the text is requested by the
user. Internally, everything is built with the `Builder` type, a buffer used for efficiently building `Text`
values, with an `append` operation running in `𝛰(1)`. In comparison, the `append` operation for `Text` has a 
complexity of `𝛰(1)`.  
This is especially useful for types that are defined recursively, like lists or trees for example. 

For example: 
```haskell
data Tree a = Node a [Tree a]

instance Display a => Display (Tree a) where
  -- displayBuilder for the instance
  displayBuilder (Node a xs) = displayBuilder a <> displayBuilderList xs

-- display for the application code
display $ Node 1 [Node 2 [], Node 3 [], Node 4 []]
-- => "1[2[],3[],4[]]"
```


## The four siblings: `display`, `displayPrec`, `displayList`, and `displayBuilder`

If you take a close look at the `Display` typeclass, you will see the following information:

```haskell
ghci> :i Display
type Display :: * -> Constraint
    ┌──
    │  class Display a where
    │    displayBuilder :: a -> Builder
 1. │    displayList :: [a] -> Builder
    │    displayPrec :: Int -> a -> Builder
    └──

    ┌──
 2. │  {-# MINIMAL displayBuilder | displayPrec #-}
    └─
```

1. This is the class definition, with its methods.
  * `displayBuilder` produces a `Builder` value out of an `a`.  
     This is the most common way to implement `Display` when you just need to render something for an end-user.

  * `displayList` produces a `Builder` value out of a list of `a`.
     This method has a default implementation provided by the library, but it may be overloaded in case you
     wish to render the list of a particular type in a certain way, like how `[Char]` is rendered to `"this nice user-readable string"` instead of something `['l', 'i', 'k', 'e', ' ', 't', 'h', 'i', 's']`.

  * `displayPrec` takes a precedence value with the value to be transformed, and produces a `Builder`.  
     It has a more advanced target audience in mind, and is used when printing nested datatypes calls for a clarification of operator and constructor precedence.
     If the precedence is not set, we cannot say that we want parentheses surrounding an inner value, like `"Just (Just 5)"`, and indeed, may find ourselves
     with a result like `"Just Just 5"`, which is unacceptable.

2. This is the minimal implementation a user of the library must provide in order to implement `Display` for
their datatype.  
You will notice that `display` itself is not part of the Typeclass. And indeed, we wanted the typeclass to:

  1. Internally use a `Builder` type for efficient production of textual data;
  2. Be able to handle precedence;
  3. Be user-friendly and return a `Text`, that is understood in most textual APIs.

  While all three goals have been achieved, we noticed that only the first two points had to be implemented
  *inside* the typeclass definition, and `display :: Display a => a -> Text` could live outside of the typeclass
  and still serve its purpose. As such, as not to clutter the typeclass methods, it was decided to keep `display`
  out of it.

## Usage restrictions

### "🚫 You should not derive Display for function types!"

Sometimes, when using the library, you may encounter this message:

```
• 🚫 You should not derive Display for function types!                     
  💡 Write a 'newtype' wrapper that represents your domain more accurately.
     If you are not consciously trying to use `display` on a function,     
     make sure that you are not missing an argument somewhere.
```

The `display` library does not allow the definition and usage of `Display` on
bare function types (`(a -> b)`).  
Experience and time have shown that due to partial application being baked in the language,
many users encounter a partial application-related error message when a simple missing
argument to a function is the root cause.

There may be legitimate uses of a `Display` instance on a function type.
But these usages are extremely dependent on their domain of application.
That is why it is best to wrap them in a newtype that can better
express and enforce the domain.

### "🚫 You should not derive Display for ByteStrings!"

An arbitrary ByteStrings cannot be safely converted to text without prior knowledge of its encoding.
As such, in order to avoid dangerously blind conversions, it is recommended to use a specialised
function such as `decodeUtf8'` or `decodeUtf8Strict` if you wish to turn a UTF8-encoded ByteString
to Text.

[^1]: _"mort aux lois, vive l'anarchie"_ - Georges Brassens
