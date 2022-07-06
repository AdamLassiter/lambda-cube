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

## Feature Flags

### Syntax

The following flags can be used to disable certain language features:

| Flag        | Description                                        | Default              | With Flag                                                          |
|-------------|----------------------------------------------------|----------------------|--------------------------------------------------------------------|
| NOANONPI    | Require explicit binds for Pi expressions          | `O.τ.I`              | `O.π(_:τ).I`                                                       |
| NOTAUSUB    | Disable stdlib types Tau type substitution         | `λ(x:Null).isNull x` | `λ(x:λ(Null:*).λ(null:Null).Null).λ(Null:*).λ(null:Null).isNull x` |
| BRACKETTYPE | Enforce brackets uniquely in type-signatures *     | `λ[x:*].x`           | `λ(x:*).x`                                                         |
| SETNOTATION | Enforce set-theory notation *                      | `π(τ:#).λ[x:*].x`    | `∀(τ∈⊥)->∃(x∈T)->x`                                                |
| HSNOTATION  | Enforce haskell-like notation, implies SETNOTATION | `π(τ:#).λ[x:*].x`    | `forall(τ:#)->lambda(x:*)->x`                                      |
| NOINFER     | Require explicit types for binds *                 | `(λ(x:?).E x) a`     | `(λ(x:A).E x) a`                                                   |

_* Not implemented_

### Logging

The following flags can be used to remove logging calls at compile-time:

| Flag     | Description                                     |
|----------|-------------------------------------------------|
| LOGTRACE | Enable logging at TRACE level, implies LOGDEBUG |
| LOGDEBUG | Enable logging at DEBUG level, implies LOGINFO  |
| LOGINFO  | Enable logging at INFO level, implies LOGWARN   |
| LOGWARN  | Enable logging at WARN level                    |

In general, it is preferred to use `setLogLevel` or `set[Trace|Debug|Info|Warn|Error]SourceRegex`.
Nevertheless, there may be certain scenarios where either logs will never be desired, or there is some performance impact.


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
