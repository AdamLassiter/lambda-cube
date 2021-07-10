-- all.mt

-- let data Bool = True | False
--
--     data List a = Cons a (List a) | Nil
--
-- in  let (&&) :: Bool -> Bool -> Bool
--         (&&) b1 b2 = if b1 then b2 else False
--
--         bools :: List Bool
--         bools = Cons True (Cons True (Cons True Nil))
--
--     in  foldr bools (&&) True

(   lambda (Bool : *)
->  lambda (True  : Bool)
->  lambda (False : Bool)
->  lambda (if : Bool -> forall (r : *) -> r -> r -> r)
->  lambda (List : * -> *)
->  lambda (Cons : forall (a : *) -> a -> List a -> List a)
->  lambda (Nil  : forall (a : *)                -> List a)
->  lambda (  foldr
    :   forall (a : *) -> List a -> forall (r : *) -> (a -> r -> r) -> r -> r
    )
->  (   lambda ((&&) : Bool -> Bool -> Bool)
    ->  lambda (bools : List Bool)
    ->  foldr Bool bools Bool (&&) True
    )

    -- (&&)
    (lambda (x : Bool) -> lambda (y : Bool) -> if x Bool y False)

    -- bools
    (Cons Bool True (Cons Bool True (Cons Bool True (Nil Bool))))
)

-- Bool
(forall (r : *) -> r -> r -> r)

-- True
(lambda (r : *) -> lambda (x : r) -> lambda (_ : r) -> x)

-- False
(lambda (r : *) -> lambda (_ : r) -> lambda (x : r) -> x)

-- if
(lambda (b : forall (r : *) -> r -> r -> r) -> b)

-- List
(   lambda (a : *)
->  forall (list : *)
->  (a -> list -> list)  -- Cons
->  list                 -- Nil
->  list
)

-- Cons
(   lambda (a : *)
->  lambda (va  : a)
->  lambda (vas : forall (list : *) -> (a -> list -> list) -> list -> list)
->  lambda (list : *)
->  lambda (Cons : a -> list -> list)
->  lambda (Nil  : list)
->  Cons va (vas list Cons Nil)
)

-- Nil
(   lambda (a : *)
->  lambda (list : *)
->  lambda (Cons : a -> list -> list)
->  lambda (Nil  : list)
->  Nil
)

-- foldr
(   lambda (a : *)
->  lambda (vas : forall (list : *) -> (a -> list -> list) -> list -> list)
->  vas
)
