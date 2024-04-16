typed-fsm
==========

typed finite-state machine

Advantages of type-fsm
1. Focus on the right message

2. Top-to-bottom design for easy refactoring

3. Conducive to building complex state machine systems

  + Type guarantees will not produce incorrect function calls when written
  + With the help of the type system, we can define many state processing functions and then call each other recursively with confidence.

4. There is a sanity check. If you miss some items for pattern matching, the compiler will issue a warning, and there will also be a warning for invalid items.

[Detailed design explanation](./more-en.md)

[详细设计解释](./more.md)

## Run example
1. Install ghc 9.10.1 using ghcup [Enough help here](https://discourse.haskell.org/t/ghc-9-10-1-alpha1-is-now-available/9048)
2. cabal run motion  --flags="BuildExample"
