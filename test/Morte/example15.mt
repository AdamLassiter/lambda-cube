-- corecursive.mt

-- data IOF r s = PutStrLn String s | GetLine (String -> s) | Return r
--
-- data IO r = forall s . MkIO s (s -> IOF r s)
--
-- main = MkIO Nothing (maybe (\str -> PutStrLn str Nothing) (GetLine Just))

(   lambda (String : *)
->  (   lambda (Maybe : * -> *)
    ->  lambda (Just : forall (a : *) -> a -> Maybe a)
    ->  lambda (Nothing : forall (a : *) -> Maybe a)
    ->  lambda (  maybe
        :   forall (a : *) -> Maybe a -> forall (x : *) -> (a -> x) -> x -> x
        )
    ->  lambda (IOF : * -> * -> *)
    ->  lambda (  PutStrLn
        :   forall (r : *)
        ->  forall (s : *)
        ->  String
        ->  s
        ->  IOF r s
        )
    ->  lambda (  GetLine
        :   forall (r : *)
        ->  forall (s : *)
        ->  (String -> s)
        ->  IOF r s
        )
    ->  lambda (  Return
        :   forall (r : *)
        ->  forall (s : *)
        ->  r
        ->  IOF r s
        )
    ->  (   lambda (IO : * -> *)
        ->  lambda (  MkIO
            :   forall (r : *) -> forall (s : *) -> s -> (s -> IOF r s) -> IO r
            )
        ->  (   lambda (main : forall (r : *) -> IO r)
            ->  main
            )

            -- main
            (   lambda (r : *)
            ->  MkIO
                r
                (Maybe String)
                (Nothing String)
                (   lambda (m : Maybe String)
                ->  maybe
                        String
                        m
                        (IOF r (Maybe String))
                        (lambda (str : String) ->
                            PutStrLn r (Maybe String) str (Nothing String)
                        )
                        (GetLine r (Maybe String) (Just String))
                )
            )
        )

        -- IO
        (   lambda (r : *)
        ->  forall (x : *)
        ->  (forall (s : *) -> s -> (s -> IOF r s) -> x)
        ->  x
        )

        -- MkIO
        (   lambda (r : *)
        ->  lambda (s : *)
        ->  lambda (seed : s)
        ->  lambda (step : s -> IOF r s)
        ->  lambda (x : *)
        ->  lambda (k : forall (s : *) -> s -> (s -> IOF r s) -> x)
        ->  k s seed step
        )
    )

    -- Maybe
    (lambda (a : *) -> forall (x : *) -> (a -> x) -> x -> x)

    -- Just
    (   lambda (a : *)
    ->  lambda (va : a)
    ->  lambda (x : *)
    ->  lambda (Just : a -> x)
    ->  lambda (Nothing : x)
    ->  Just va
    )

    -- Nothing
    (   lambda (a : *)
    ->  lambda (x : *)
    ->  lambda (Just : a -> x)
    ->  lambda (Nothing : x)
    ->  Nothing
    )

    -- maybe
    (lambda (a : *) -> lambda (m : forall (x : *) -> (a -> x) -> x -> x) -> m)

    -- IOF
    (   lambda (r : *)
    ->  lambda (s : *)
    ->  forall (x : *)
    ->  (String -> s -> x)
    ->  ((String -> s) -> x)
    ->  (r -> x)
    ->  x
    )

    -- PutStrLn
    (   lambda (r : *)
    ->  lambda (s : *)
    ->  lambda (str : String)
    ->  lambda (vs : s)
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> s -> x)
    ->  lambda (GetLine : (String -> s) -> x)
    ->  lambda (Return : r -> x)
    ->  PutStrLn str vs
    )

    -- GetLine
    (   lambda (r : *)
    ->  lambda (s : *)
    ->  lambda (k : String -> s)
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> s -> x)
    ->  lambda (GetLine : (String -> s) -> x)
    ->  lambda (Return : r -> x)
    ->  GetLine k
    )

    -- Return
    (   lambda (r : *)
    ->  lambda (s : *)
    ->  lambda (vr : r)
    ->  lambda (x : *)
    ->  lambda (PutStrLn : String -> s -> x)
    ->  lambda (GetLine : (String -> s) -> x)
    ->  lambda (Return : r -> x)
    ->  Return vr
    )

)
