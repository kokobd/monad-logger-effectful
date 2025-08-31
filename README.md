# effectful support for monad-logger

This package provides integration between the [`effectful`](https://hackage.haskell.org/package/effectful) library and
[`monad-logger`](https://hackage.haskell.org/package/monad-logger), enabling structured logging within the effectful effects
system.

The package defines a `Logger` effect that can be used with the effectful
library, along with orphan instances for `MonadLogger` and `MonadLoggerIO`
that allow existing monad-logger code to work seamlessly with effectful
computations.
