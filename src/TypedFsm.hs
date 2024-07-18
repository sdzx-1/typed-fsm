{- | This package defines the typed protocol framework. This module re-exports
the public API.

FSM stands for [Finite State Machine](https://en.wikipedia.org/wiki/Finite-state_machine).

The typed-fsm is used to define and execute FSM.

Advantages of type-fsm:

* Focus on the right message.
* Top-to-bottom design for easy refactoring.
* Conducive to building complex state machine systems:
** Type guarantees will not produce incorrect function calls when written.
** With the help of the type system, we can define many state processing functions and then call each other recursively with confidence.
* There is a sanity check. If you miss some items for pattern matching, the compiler will issue a warning, and there will also be a warning for invalid items.


In order to use do syntax, the `QualifiedDo` extension is required.

The QualifiedDo extension allows us to overload only the two operators (>>=) and (>>).

Prior to ghc 9.10.1, this extension had [serious issues](https://gitlab.haskell.org/ghc/ghc/-/issues/21206),
But this [MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10140) fixes these problems! !

The latest ghc 9.10.1 contains this MR. So this library requires you to update the ghc version to 9.10.1.

Using typed-fsm will go through the following five steps:

1. Define status
2. Define state transfer messages
3. Build status processing function
4. Construct functions from events to messages in different states
5. Running status processing function

Here are some examples:

1. [turnstile](https://github.com/sdzx-1/typed-fsm/tree/main/examples/turnstile)
2. [mouse motion](https://github.com/sdzx-1/typed-fsm/tree/main/examples/motion)
3. [atm](https://github.com/sdzx-1/typed-fsm/tree/main/examples/ATM)
-}
module TypedFsm (
  -- * Defining and implementing FSM
  module TypedFsm.Core,

  -- * Running FSM
  module TypedFsm.Driver,
) where

import TypedFsm.Core
import TypedFsm.Driver
