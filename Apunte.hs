first :: (a,b) -> a
first (x,y) = x

doble :: Int -> Int
doble x = x + x

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

twice :: (a -> a) -> (a -> a)
twice f = g
    where g x = f (f x)



apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f = g
    where g p = f (swap p)

appDup :: ((a,a) -> b) -> (a -> b)
appDup f = g
    where g x = f (x, x)

appFork :: (a -> c, a -> b) -> (a -> (c,b))
appFork (f, g) = h
    where h x = (f x, g x)

appPar ::  (c -> d, a -> b ) -> ((c,a) -> (d,b))
appPar (f, g) = h
    where h (x, y) = (f x, g y)

appDist :: (a -> b) -> ((a,b) -> (b,b))
appDist f = g
    where g (x, y) = (f x, f y)

flip :: (c -> d) -> (c -> (a -> d))
flip f = h
    where h x = k
        where k y = (f y) x

subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))
subst f = h
    where h g = k
        where k x = (f x) (g x)




-- SIN WHERE

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

apply :: (a -> b) -> (a -> b)
apply f x = f x

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f p = f (swap p)

appDup :: ((a,a) -> b) -> (a -> b)
appDup f x = f (x, x)

appFork :: (a -> c, a -> b) -> (a -> (c,b))
appFork (f, g) x = (f x, g x)

appPar ::  (c -> d, a -> b ) -> ((c,a) -> (d,b))
appPar (f, g) (x, y) = (f x, g y)

appDist :: (a -> b) -> ((a,b) -> (b,b))
appDist f (x, y) = (f x, f y)

flip :: (c -> d) -> (c -> (a -> d))
flip f x y = (f y) x

subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))
subst f g x = (f x) (g x)


const :: b -> (a -> b)
const x y = x

