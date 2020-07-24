# λ-Cube
Some thoughts on Calculus of Constructions

![lambda-cube](resources/Lambda_Cube_img.svg)

### stlc.cube testfile
```
let Bool : *
let False : Bool
let True : Bool

let id = λ x : Bool . x
assert (id) (True) = True

let false = λ x : Bool . False
assert (false) (True) = False

let first = λ x : Bool . λ y : Bool . x
assert (first) (False) (True) = False

let applyFalse = λ f : (π x : Bool . Bool) . (f) (False)
assert (applyFalse) (id) = False
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
