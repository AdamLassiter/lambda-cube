-- corecursive.mt

-- first :: (a -> b) -> (a, c) -> (b, c)
-- first f (va, vb) = (f va, vb) 
-- 
-- data Stream a = Cons (a, Stream a)
-- 
-- map :: (a -> b) -> Stream a -> Stream b
-- map f (Cons (va, s)) = Cons (first f (va, map f s))
-- 
-- -- exampleA = exampleB
-- 
-- exampleA :: Stream a -> Stream a
-- exampleA = map id
-- 
-- exampleB :: Stream a -> Stream a
-- exampleB = id
-- 
-- -- exampleC = exampleD
-- 
-- exampleC :: (b -> c) -> (a -> b) -> Stream a -> Stream c
-- exampleC f g = map (f . g)
-- 
-- exampleD :: (b -> c) -> (a -> b) -> Stream a -> Stream c
-- exampleD f g = map f . map g

(   lambda (id : forall (a : *) -> a -> a)
->  lambda (  (.)
    :   forall (a : *)
    ->  forall (b : *)
    ->  forall (c : *)
    ->  (b -> c)
    ->  (a -> b)
    ->  (a -> c)
    )
->  lambda (Pair : * -> * -> *)
->  lambda (P : forall (a : *) -> forall (b : *) -> a -> b -> Pair a b)
->  lambda (  first
    :   forall (a : *)
    ->  forall (b : *)
    ->  forall (c : *)
    ->  (a -> b)
    ->  Pair a c
    ->  Pair b c
    )

->  (   lambda (Stream : * -> *)
    ->  lambda (  map
        :   forall (a : *)
        ->  forall (b : *)
        ->  (a -> b)
        ->  Stream a
        ->  Stream b
        )

        -- exampleA = exampleB
    ->  (   lambda (exampleA : forall (a : *) -> Stream a -> Stream a)
        ->  lambda (exampleB : forall (a : *) -> Stream a -> Stream a)

        -- exampleC = exampleD
        ->  lambda (  exampleC
            :   forall (a : *)
            ->  forall (b : *)
            ->  forall (c : *)
            ->  (b -> c)
            ->  (a -> b)
            ->  Stream a
            ->  Stream c
            )

        ->  lambda (  exampleD
            :   forall (a : *)
            ->  forall (b : *)
            ->  forall (c : *)
            ->  (b -> c)
            ->  (a -> b)
            ->  Stream a
            ->  Stream c
            )

        -- Uncomment the example you want to test
--      ->  exampleA
--      ->  exampleB
        ->  exampleC
--      ->  exampleD
        )

        -- exampleA
        (lambda (a : *) -> map a a (id a))
  
        -- exampleB
        (lambda (a : *) -> id (Stream a))

        -- exampleC
        (   lambda (a : *)
        ->  lambda (b : *)
        ->  lambda (c : *)
        ->  lambda (f : b -> c)
        ->  lambda (g : a -> b)
        ->  map a c ((.) a b c f g)
        )

        --  exampleD
        (   lambda (a : *)
        ->  lambda (b : *)
        ->  lambda (c : *)
        ->  lambda (f : b -> c)
        ->  lambda (g : a -> b)
        ->  (.) (Stream a) (Stream b) (Stream c) (map b c f) (map a b g)
        )
    )

    -- Stream
    (   lambda (a : *)
    ->  forall (x : *)
    ->  (forall (s : *) -> s -> (s -> Pair a s) -> x)
    ->  x
    )

    -- map
    (   lambda (a : *)
    ->  lambda (b : *)
    ->  lambda (f : a -> b)
    ->  lambda (  st
        :   forall (x : *) -> (forall (s : *) -> s -> (s -> Pair a s) -> x) -> x
        )
    ->  lambda (x : *)
    ->  lambda (S : forall (s : *) -> s -> (s -> Pair b s) -> x)
    ->  st
        x
        (   lambda (s : *)
        ->  lambda (seed : s)
        ->  lambda (step : s -> Pair a s)
        ->  S
            s
            seed
            (lambda (seed : s) -> first a b s f (step seed))
        )
    )
)

-- id
(lambda (a : *) -> lambda (va : a) -> va)

-- (.)
(   lambda (a : *)
->  lambda (b : *)
->  lambda (c : *)
->  lambda (f : b -> c)
->  lambda (g : a -> b)
->  lambda (va : a)
->  f (g va)
)

-- Pair
(lambda (a : *) -> lambda (b : *) -> forall (x : *) -> (a -> b -> x) -> x)

-- P
(   lambda (a : *)
->  lambda (b : *)
->  lambda (va : a)
->  lambda (vb : b)
->  lambda (x : *)
->  lambda (P : a -> b -> x)
->  P va vb
)

-- first
(   lambda (a : *)
->  lambda (b : *)
->  lambda (c : *)
->  lambda (f : a -> b)
->  lambda (p : forall (x : *) -> (a -> c -> x) -> x)
->  lambda (x : *)
->  lambda (Pair : b -> c -> x)
->  p x (lambda (va : a) -> lambda (vc : c) -> Pair (f va) vc)
)
