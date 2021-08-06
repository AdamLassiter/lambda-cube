# λ-Cube
Some thoughts on Calculus of Constructions

[![gh-actions](https://github.com/AdamLassiter/lambda-cube/actions/workflows/haskell.yml/badge.svg)](https://github.com/AdamLassiter/lambda-cube/actions/workflows/haskell.yml)
[![gh-pages](https://img.shields.io/badge/dynamic/json?color=brightgreen&label=Haddock&query=statuses%5B0%5D.state&url=https%3A%2F%2Fapi.github.com%2Frepos%2FAdamLassiter%2Flambda-cube%2Fcommits%2Fmaster%2Fstatus)](https://adamlassiter.github.io/lambda-cube/)
[![codecov](https://codecov.io/gh/AdamLassiter/lambda-cube/branch/master/graph/badge.svg?token=AOX2G89AL9)](https://codecov.io/gh/AdamLassiter/lambda-cube)
[![license](https://img.shields.io/github/license/AdamLassiter/lambda-cube?label=License)](/LICENSE)


![lambda-cube](resources/lambda-cube-img.svg)


## Compile, Test and Install

```sh
stack build
stack test
stack run lambda-cube-exe
```


## Performance

`stack test && prof-flamegraph`
![flamegraph](resources/lambda-cube-test.prof.svg)

`stack test && hp-to-pretty`
![heapgraph](resources/lambda-cube-test.hp.svg)


## References

* Loosely based on [ChristopherKing42/CalculusOfConstructions.hs](https://gist.github.com/ChristopherKing42/d8c9fde0869ec5c8feae71714e069214)
* Tested against the [Gabriel439/Haskell-Morte-Library](https://github.com/Gabriel439/Haskell-Morte-Library) [Prelude](https://github.com/Gabriel439/Haskell-Morte-Library/tree/master/Prelude) and [Examples](https://github.com/Gabriel439/Haskell-Morte-Library/tree/master/test/src)
