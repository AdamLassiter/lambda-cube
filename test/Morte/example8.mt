-- mapcomp1.mt

-- map (f . g)

(   lambda (List : * -> *)
->  lambda (map  : forall (a : *) -> forall (b : *) -> (a -> b) -> List a -> List b)
->  lambda (  app
    :   forall (a : *)
    ->  forall (b : *)
    ->  forall (c : *)
    ->  (b -> c)
    ->  (a -> b)
    ->  (a -> c)
    )
    ->  lambda (a : *)
    ->  lambda (b : *)
    ->  lambda (c : *)
    ->  lambda (f : b -> c)
    ->  lambda (g : a -> b)
    ->  map a c (app a b c f g)
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

-- app
(   lambda (a : *)
->  lambda (b : *)
->  lambda (c : *)
->  lambda (f : b -> c)
->  lambda (g : a -> b)
->  lambda (va : a)
->  f (g va)
)
