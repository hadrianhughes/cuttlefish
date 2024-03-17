A project with the aim of building a programming language that is as 'correct' and type-safe as Haskell or Rust, while being as convenient and intuitive as Go.

The language will not necessarily be called Cuttlefish--I just needed a repo name.

## Next

- We basically just want to restrict the function call parser so it all has to be on one line. To do this, let's make a separate version of `atomicExprP` that uses the horizontal space consumer.
- Also parse access for structs and lists
- We need to think about destructuring things like tuples
