twice :: (f->f) -> f -> f
twice f x = f (f x)

compose :: (h->i) -> (j->h) -> j -> i
compose f g x = f (g x)

compose12 :: (d->c) -> (a->b->d) -> a -> b -> c
compose12 f g x y = f (g x y)

{-
compose12 twice compose

compose12 twice compose :: (h -> f) -> (f -> h) -> f -> f
compose12 twice compose k l x = k (l x) 


compose12 :: (d->c) -> (a->b->d) -> a -> b -> c
compose12 = \f g x y -> f (g x y)

twice :: (a->a) -> a -> a
twice =\f x -> f (f x)

compose :: (b->c) -> (a->b) -> a -> c
compose = \f g x -> f (g x)




compose12 twice compose
                    (def compose12)
(\f g x y -> f (g x y)) twice compose
                    (reemplazo f con compose)
 (\g x y -> twice (g x y)) compose
                    (reemplazo g con compose)
 \x y -> twice (compose x y)
                    (def twice)
 \x y -> (\h i -> h (h i)) (compose x y)
                    (reemplazo h = compose x y)
 \x y -> (\i -> (compose x y) ((compose x y) i))

 \x y -> (\i -> compose x y (compose x y i))   

 \x y -> (\i -> compose x y (x (y i)))

\x y -> (\i -> x (y (x (y i))))

\x y i -> x (y (x (y i)))

compose12 twice compose :: (h -> f) -> (f -> h) -> f -> f
compose12 twice compose + \x y i -> x (y (x (y i)))
-}