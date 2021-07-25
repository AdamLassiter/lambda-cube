-- bool.mt

-- let data Bool = True | False
--
-- in  if True then One else Zero

(   lambda (Bool : *)
->  lambda (True  : Bool)
->  lambda (False : Bool)
->  lambda (if : Bool -> forall (r : *) -> r -> r -> r)
->  lambda (Int  : *)
->  lambda (Zero : Int)
->  lambda (One  : Int)
->  if True Int One Zero
)

-- Bool
(forall (r : *) -> r -> r -> r)

-- True
(lambda (r : *) -> lambda (x : r) -> lambda (_ : r) -> x)

-- False
(lambda (r : *) -> lambda (_ : r) -> lambda (x : r) -> x)

-- if
(lambda (b : forall (r : *) -> r -> r -> r) -> b)
