-- recursive.mt

-- The Haskell code we will translate to Morte:
--
--     import Prelude hiding (
--         (+), (*), IO, putStrLn, getLine, (>>=), (>>), return )
-- 
--     -- Simple prelude
--
--     data Nat = Succ Nat | Zero
--
--     zero :: Nat
--     zero = Zero
--
--     one :: Nat
--     one = Succ Zero
--
--     (+) :: Nat -> Nat -> Nat
--     Zero   + n = n
--     Succ m + n = m + Succ n
--
--     (*) :: Nat -> Nat -> Nat
--     Zero   * n = Zero
--     Succ m * n = n + (m * n)
--
--     foldNat :: Nat -> (a -> a) -> a -> a
--     foldNat  Zero    f x = x
--     foldNat (Succ m) f x = f (foldNat m f x)
--
--     data IO r = PutStrLn String (IO r) | GetLine (String -> IO r) | Return r
--
--     putStrLn :: String -> IO U
--     putStrLn str = PutStrLn str (Return Unit)
--
--     getLine :: IO String
--     getLine = GetLine Return
--
--     return :: a -> IO a
--     return = Return
--
--     (>>=) :: IO a -> (a -> IO b) -> IO b
--     PutStrLn str io >>= f = PutStrLn str (io >>= f)
--     GetLine k       >>= f = GetLine (\str -> k str >>= f)
--     Return r        >>= f = f r
--
--     -- Derived functions
--
--     (>>) :: IO U -> IO U -> IO U
--     m >> n = m >>= \_ -> n
--
--     two :: Nat
--     two = one + one
--
--     three :: Nat
--     three = one + one + one
--
--     four :: Nat
--     four = one + one + one + one
--
--     five :: Nat
--     five = one + one + one + one + one
--
--     six :: Nat
--     six = one + one + one + one + one + one
--
--     seven :: Nat
--     seven = one + one + one + one + one + one + one
--
--     eight :: Nat
--     eight = one + one + one + one + one + one + one + one
--
--     nine :: Nat
--     nine = one + one + one + one + one + one + one + one + one
--
--     ten :: Nat
--     ten = one + one + one + one + one + one + one + one + one + one
--
--     replicateM_ :: Nat -> IO U -> IO U
--     replicateM_ n io = foldNat n (io >>) (return Unit)
--
--     ninetynine :: Nat
--     ninetynine = nine * ten + nine
--
--     main_ :: IO U
--     main_ = getLine >>= putStrLn

-- "Free" variables
(   lambda (String : *   )
->  lambda (U : *)
->  lambda (Unit : U)

    -- Simple prelude
->  (   lambda (Nat : *)
    ->  lambda (zero : Nat)
    ->  lambda (one : Nat)
    ->  lambda ((+) : Nat -> Nat -> Nat)
    ->  lambda ((*) : Nat -> Nat -> Nat)
    ->  lambda (foldNat : Nat -> forall (a : *) -> (a -> a) -> a -> a)
    ->  lambda (IO : * -> *)
    ->  lambda (return : forall (a : *) -> a -> IO a)
    ->  lambda ((>>=)
        :   forall (a : *)
        ->  forall (b : *)
        ->  IO a
        ->  (a -> IO b)
        ->  IO b
        )
    ->  lambda (putStrLn : String -> IO U)
    ->  lambda (getLine : IO String)

        -- Derived functions
    ->  (   lambda ((>>) : IO U -> IO U -> IO U)
        ->  lambda (two   : Nat)
        ->  lambda (three : Nat)
        ->  lambda (four  : Nat)
        ->  lambda (five  : Nat)
        ->  lambda (six   : Nat)
        ->  lambda (seven : Nat)
        ->  lambda (eight : Nat)
        ->  lambda (nine  : Nat)
        ->  lambda (ten   : Nat)
        ->  (   lambda (replicateM_ : Nat -> IO U -> IO U)
            ->  lambda (ninetynine : Nat)
            ->  replicateM_ ninetynine ((>>=) String U getLine putStrLn)
            )

            -- replicateM_
            (   lambda (n : Nat)
            ->  lambda (io : IO U)
            ->  foldNat n (IO U) ((>>) io) (return U Unit)
            )

            -- ninetynine
            ((+) ((*) nine ten) nine)
        )

        -- (>>)
        (   lambda (m : IO U)
        ->  lambda (n : IO U)
        ->  (>>=) U U m (lambda (_ : U) -> n)
        )

        -- two
        ((+) one one)

        -- three
        ((+) one ((+) one one))

        -- four
        ((+) one ((+) one ((+) one one)))

        -- five
        ((+) one ((+) one ((+) one ((+) one one))))

        -- six
        ((+) one ((+) one ((+) one ((+) one ((+) one one)))))

        -- seven
        ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one one))))))

        -- eight
        ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one one)))))))
        -- nine
        ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one one))))))))

        -- ten
        ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one ((+) one one)))))))))
    )

    -- Nat
    (   forall (a : *)
    ->  (a -> a)
    ->  a
    ->  a
    )

    -- zero
    (   lambda (a : *)
    ->  lambda (Succ : a -> a)
    ->  lambda (Zero : a)
    ->  Zero
    )

    -- one
    (   lambda (a : *)
    ->  lambda (Succ : a -> a)
    ->  lambda (Zero : a)
    ->  Succ Zero
    )

    -- (+)
    (   lambda (m : forall (a : *) -> (a -> a) -> a -> a)
    ->  lambda (n : forall (a : *) -> (a -> a) -> a -> a)
    ->  lambda (a : *)
    ->  lambda (Succ : a -> a)
    ->  lambda (Zero : a)
    ->  m a Succ (n a Succ Zero)
    )

    -- (*)
    (   lambda (m : forall (a : *) -> (a -> a) -> a -> a)
    ->  lambda (n : forall (a : *) -> (a -> a) -> a -> a)
    ->  lambda (a : *)
    ->  lambda (Succ : a -> a)
    ->  lambda (Zero : a)
    ->  m a (n a Succ) Zero
    )

    -- foldNat
    (   lambda (n : forall (a : *) -> (a -> a) -> a -> a)
    ->  n
    )

    -- IO
    (   lambda (r : *)
    ->  forall (x : *)
    ->  (String -> x -> x)
    ->  ((String -> x) -> x)
    ->  (r -> x)
    ->  x
    )

    -- return
    (   lambda (a : *)
    ->  lambda (va : a)
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> x -> x)
    ->  lambda (GetLine : (String -> x) -> x)
    ->  lambda (Return : a -> x)
    ->  Return va
    )

    -- (>>=)
    (   lambda (a : *)
    ->  lambda (b : *)
    ->  lambda (m : forall (x : *)
        ->  (String -> x -> x)
        ->  ((String -> x) -> x)
        ->  (a -> x)
        ->  x
        )
    ->  lambda (f : a
        ->  forall (x : *)
        -> (String -> x -> x)
        -> ((String -> x) -> x)
        -> (b -> x)
        -> x
        )
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> x -> x)
    ->  lambda (GetLine : (String -> x) -> x)
    ->  lambda (Return : b -> x)
    ->  m x PutStrLn GetLine (lambda (va : a) -> f va x PutStrLn GetLine Return)
    )

    -- putStrLn
    (   lambda (str : String)
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> x -> x  )
    ->  lambda (GetLine  : (String -> x) -> x)
    ->  lambda (Return   : U -> x)
    ->  PutStrLn str (Return Unit)
    )

    -- getLine
    (   lambda (x : *)
    ->  lambda (PutStrLn : String -> x -> x  )
    ->  lambda (GetLine  : (String -> x) -> x)
    ->  lambda (Return   : String -> x)
    -> GetLine Return
    )
)