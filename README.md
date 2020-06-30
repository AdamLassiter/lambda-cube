# λ-cube
An implementation of Andres Loeh's LambdaPi.

```
LambdaPi Test Suite
-------------------
term: (λ 1 . λ 0 . 0) :: (Π 0::* . Π 1::0 . 1)
eval: λ 1 . λ 0 . 0
type: Π 0::* . Π 1::0 . 1

term: ((λ 1 . λ 0 . 0) :: (Π 0::* . Π 1::0 . 1)) (Bool)
eval: λ 0 . 0
type: Π 0::Bool . Bool

term: (((λ 1 . λ 0 . 0) :: (Π 0::* . Π 1::0 . 1)) (Bool)) (False)
eval: False
type: Bool
```