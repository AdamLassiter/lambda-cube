# λ-Cube
Some thoughts on Calculus of Constructions

![lambda-cube](resources/Lambda_Cube_img.svg)

---

```
==================
Running Test Suite
==================
term: λ (x : *) . λ (y : x) . y
ctx:  Bool : *, False : Bool
eval: λ (0 : *) . λ (1 : 0) . 1
type: π (0 : *) . π (1 : 0) . 0

term: (λ (x : *) . λ (y : x) . y) (Bool)
ctx:  Bool : *, False : Bool
eval: λ (1 : Bool) . 1
type: π (1 : Bool) . Bool

term: ((λ (x : *) . λ (y : x) . y) (Bool)) (False)
ctx:  Bool : *, False : Bool
eval: False
type: Bool

==================
Constructions REPL
==================
>> let Bool : *
>> let False : Bool
>> eval (λ (x : *) . λ (y : x) . y) (Bool) (False)
False
>> type (λ (x : *) . λ (y : x) . y) (Bool) (False)
Bool
>> 
```

---

| (s_1,s_2) | ( * , * ) | ( * , # ) | ( # , * ) | ( # , # )
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
