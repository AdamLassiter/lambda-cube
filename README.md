# λ-Cube

[![gh-actions](https://github.com/AdamLassiter/lambda-cube/actions/workflows/haskell.yml/badge.svg)](https://github.com/AdamLassiter/lambda-cube/actions/workflows/haskell.yml)
[![gh-pages](https://img.shields.io/badge/dynamic/json?color=brightgreen&label=Haddock&query=statuses%5B0%5D.state&url=https%3A%2F%2Fapi.github.com%2Frepos%2FAdamLassiter%2Flambda-cube%2Fcommits%2Fmaster%2Fstatus)](https://adamlassiter.github.io/lambda-cube/)
[![codecov](https://codecov.io/gh/AdamLassiter/lambda-cube/branch/master/graph/badge.svg?token=AOX2G89AL9)](https://codecov.io/gh/AdamLassiter/lambda-cube)
[![license](https://img.shields.io/github/license/AdamLassiter/lambda-cube?label=License)](/LICENSE)

Some thoughts on Calculus of Constructions...

![lambda-cube](resources/lambda-cube-img.svg)

```
λ>> (λ [x : Bool] -> not x) Bool@True
π [Bool : *] -> π [True : Bool] -> π [False : Bool] -> Bool
λ [Bool : *] -> λ [True : Bool] -> λ [False : Bool] -> False
```


## Compile, Test and Install

```
>> stack build
>> stack test
>> stack run

λ>> ...
```


## Performance

``` 
>> stack bench

benchmarking λ(T:*) . λ(x:T) . x
time                 859.1 μs   (853.5 μs .. 864.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 874.8 μs   (865.7 μs .. 906.9 μs)
std dev              52.61 μs   (11.42 μs .. 109.3 μs)
variance introduced by outliers: 49% (moderately inflated)
                         
benchmarking λ(x:Nat) . Bool@eq (even x) (odd (Nat@Succ x))
time                 1.008 ms   (1.002 ms .. 1.013 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.023 ms   (1.013 ms .. 1.069 ms)
std dev              60.27 μs   (12.37 μs .. 136.0 μs)
variance introduced by outliers: 47% (moderately inflated)
```


## References

* [ChristopherKing42/CalculusOfConstructions.hs](https://gist.github.com/ChristopherKing42/d8c9fde0869ec5c8feae71714e069214)
* [Gabriel439/Haskell-Morte-Library](https://github.com/Gabriel439/Haskell-Morte-Library)
  * [Prelude](https://github.com/Gabriel439/Haskell-Morte-Library/tree/master/Prelude)
  * [Examples](https://github.com/Gabriel439/Haskell-Morte-Library/tree/master/test/src)
