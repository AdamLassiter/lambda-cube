-- mapid2.mt

(    lambda (List : * -> *)
->   lambda (map  : forall (a : *) -> forall (b : *) -> (a -> b) -> List a -> List b)
->   lambda (id   : forall (a : *) -> a -> a)
    ->   lambda (a : *) -> id (List a)
)

-- List
(lambda (a : *) -> forall (x : *) -> (a -> x -> x) -> x -> x)

-- map
(   lambda (a : *)
->  lambda (b : *)
->  lambda (f : a -> b)
->  lambda (l : forall (x : *) -> (a -> x -> x) -> x -> x)
->  lambda (x : *)
->  lambda (Cons : b -> x -> x)
->  lambda (Nil: x)
->  l x (lambda (va : a) -> lambda (vx : x) -> Cons (f va) vx) Nil
)

-- id
(lambda (a : *) -> lambda (va : a) -> va)
