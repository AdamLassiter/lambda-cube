-- mapcomp1.mt

-- map (f . g)

(   \(List : * -> *)
->  \(map  : forall (a : *) -> forall (b : *) -> (a -> b) -> List a -> List b)
->  \(  (.)
    :   forall (a : *)
    ->  forall (b : *)
    ->  forall (c : *)
    ->  (b -> c)
    ->  (a -> b)
    ->  (a -> c)
    )
    ->  \(a : *)
    ->  \(b : *)
    ->  \(c : *)
    ->  \(f : b -> c)
    ->  \(g : a -> b)
    ->  map a c ((.) a b c f g)
)

-- List
(\(a : *) -> forall (x : *) -> (a -> x -> x) -> x -> x)

-- map
(   \(a : *)
->  \(b : *)
->  \(f : a -> b)
->  \(l : forall (x : *) -> (a -> x -> x) -> x -> x)
->  \(x : *)
->  \(Cons : b -> x -> x)
->  \(Nil: x)
->  l x (\(va : a) -> \(vx : x) -> Cons (f va) vx) Nil
)

-- (.)
(   \(a : *)
->  \(b : *)
->  \(c : *)
->  \(f : b -> c)
->  \(g : a -> b)
->  \(va : a)
->  f (g va)
)
