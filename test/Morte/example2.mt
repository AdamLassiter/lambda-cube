-- id2.mt

(   lambda (id : forall (a : *) -> a -> a)
->  id (forall (a : *) -> a -> a) id  -- Apply the identity function to itself
)

-- id
(lambda (a : *) -> lambda (x : a) -> x)
