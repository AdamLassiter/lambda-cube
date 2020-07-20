# λ Cube
Some implementations of vertices of the λ cube, with varying levels of quality.

![lambda-cube](resources/Lambda_Cube_img.svg)

- [x] [(λ→) Simply Typed Lambda Calculus](src/SimplyTyped.hs)
- [ ] (λ2) System F
- [ ] (Fω_) System Fω_
- [x] [(λP) Lambda-Pi](src/LambdaPi.hs)
- [ ] (Fω) System Fω
- [x] [(λC) Calculus of Constructions](src/Constructions.hs)

---

```
===================
Running Test Suites
===================

SimplyTyped Test Suite
----------------------
term: (λ 0 . 0 : (a -> a)) (y)
ctx:  y : a, a : "*"
eval: y
type: a

term: ((λ 1 . λ 0 . 1 : ((b -> b) -> (a -> (b -> b)))) (λ 0 . 0)) (y)
ctx:  b : "*", y : a, a : "*"
eval: λ 0 . 0
type: (b -> b)


LambdaPi Test Suite
-------------------
term: (λ 1 . λ 0 . 0) : (π 1 : * . π 0 : 1 . 1)
env:  
eval: λ 1 . λ 0 . 0
type: π 1 : * . π 0 : 1 . 1

term: ((λ 1 . λ 0 . 0) : (π 1 : * . π 0 : 1 . 1)) (Bool)
env:  False : Bool, Bool : *
eval: λ 0 . 0
type: π 0 : Bool . Bool

term: (((λ 1 . λ 0 . 0) : (π 1 : * . π 0 : 1 . 1)) (Bool)) (False)
env:  False : Bool, Bool : *
eval: False
type: Bool


Constructions Test Suite
------------------------
term: λ ("x" : *) . λ ("y" : "x") . "y"
ctx:  "Bool" : *, "False" : "Bool"
eval: λ (0 : *) . λ (1 : 0) . 1
type: π (0 : *) . π (1 : 0) . 0

term: (λ ("x" : *) . λ ("y" : "x") . "y") ("Bool")
ctx:  "Bool" : *, "False" : "Bool"
eval: λ (1 : -1) . 1
type: π (1 : -1) . -1

term: ((λ ("x" : *) . λ ("y" : "x") . "y") ("Bool")) ("False")
ctx:  "Bool" : *, "False" : "Bool"
eval: -2
type: -1
```

---

| (s_1,s_2) | ( * , * ) | ( * , ◻ ) | ( ◻ , * ) | ( ◻ , ◻ )
|-----------|-----------|-----------|-----------|-----------
| λ→        | Yes       | No        | No        | No
| λP        | Yes       | Yes       | No        | No
| λ2        | Yes       | No        | Yes       | No
| λω        | Yes       | No        | No        | Yes
| λP2       | Yes       | Yes       | Yes       | No
| λPω       | Yes       | Yes       | No        | Yes
| λω        | Yes       | No        | Yes       | Yes
| λC        | Yes       | Yes       | Yes       | Yes

---

| System of the cube | Logical System
|--------------------|---------------
| λ→                 | (First Order) Propositional Calculus
| λ2                 | Second Order Propositional Calculus
| λω                 | Weakly Higher Order Propositional Calculus
| λω                 | Higher Order Propositional Calculus
| λP                 | (First Order) Predicate Logic
| λP2                | Second Order Predicate Calculus
| λPω                | Weak Higher Order Predicate Calculus
| λC                 | Calculus of Constructions
