-- pair.mt

-- let Pair a b = P a b
--
-- in  \x y -> snd (P x y)

(   lambda (Pair : * -> * -> *)
->  lambda (P    : forall (a : *) -> forall (b : *) -> a -> b -> Pair a b)
->  lambda (fst  : forall (a : *) -> forall (b : *) -> Pair a b -> a)
->  lambda (snd  : forall (a : *) -> forall (b : *) -> Pair a b -> b)
->  lambda (a : *) -> lambda (x : a) -> lambda (y : a) -> snd a a (P a a x y)
)

-- Pair
(lambda (a : *) -> lambda (b : *) -> forall (r : *) -> (a -> b -> r) -> r)

-- P
(   lambda (a : *)
->  lambda (b : *)
->  lambda (va : a)
->  lambda (vb : b)
->  lambda (r : *)
->  lambda (Pair : a -> b -> r)
->  Pair va vb
)

-- fst
(   lambda (a : *)
->  lambda (b : *)
->  lambda (p : forall (r : *) -> (a -> b -> r) -> r)
->  p a (lambda (x : a) -> lambda (_ : b) -> x)
)

-- snd
(   lambda (a : *)
->  lambda (b : *)
->  lambda (p : forall (r : *) -> (a -> b -> r) -> r)
->  p b (lambda (_ : a) -> lambda (x : b) -> x)
)
