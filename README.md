api-builder
===

Simple library for building JSON API wrappers in Haskell – define a `Builder`, add some types with `FromJSON` instances, an error type, and some routes, and you can easily use your API from Haskell code. Based on a `EitherT StateT StateT` monad transformer stack.