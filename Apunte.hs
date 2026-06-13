assoc :: (a,(b,c)) -> ((a,b),c)
assoc (x,(y,z)) = ((x,y),z)
appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
appAssoc f p = f (assoc p)

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)
curry f = g where g x = h where h y = f (x,y)

uncurry :: (a->(b->c)) -> (a,b)-> c 
uncurry f = g where g (x,y) = (f x) y
uncurry f (x,y) = (f x) y

first :: (a,b) -> a
first (x,y) = x

--izq izq izq :: dere --

doble :: Int -> Int
doble x = x + x

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

twice :: (a -> a) -> (a -> a)
twice f = g
    where g x = f (f x)

id :: a -> a
id x = x

apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f = g
    where g p = f (swap p)

appDup :: ((a,a) -> b) -> a -> b
appDup f x = f (x, x)

appFork :: (a -> c, a -> b) -> a -> (c,b)
appFork (f, g) x = (f x, g x)

appPar ::  (c -> d, a -> b ) -> ((c,a) -> (d,b))
appPar (f, g) = h
    where h (x, y) = (f x, g y)

appDist :: (a -> b) -> ((a,b) -> (b,b))
appDist f = g
    where g (x, y) = (f x, f y)

-- foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int
sum' :: [Int] -> Int
sum' = foldr (+) 0
--sum' ws = foldr (\x y -> x + y) 0 ws

-- foldr :: (a -> Int -> Int) -> Int -> [a] -> Int
length' :: [a] -> Int
length' = foldr (const (+1)) 0
-- length' = foldr (\_ n -> 1 + n) 0 

-- foldr :: (a -> b -> b) -> b -> [a] -> b
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
--map' f = foldr (\x ys -> f x : ys) []

-- foldr :: (a -> b -> b) -> b -> [a] -> b
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs-> singularSi x (f x) ++ xs) [] 
--filter' f = foldr (\x xs -> singularSi x (f x) ++ xs) [] 

-- foldr :: (a -> b -> b) -> b -> [a] -> b
find' :: (a -> Bool) -> [a] -> Maybe a
find' f = foldr (\x xs -> if f x then Just x else xs ) Nothing  

-- foldr :: (a -> b -> b) -> b -> [a] -> b
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr ((||) . f) False

-- foldr :: (a -> b -> b) -> b -> [a] -> b
all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ((&&) . f) True

-- foldr :: (a -> b -> b) -> b -> [a] -> b
countBy' :: (a -> Bool) -> [a] -> Int
countBy' f = foldr (\x xs -> if f x then 1 + xs else xs) 0

-- foldr :: (a -> b -> b) -> b -> [a] -> b
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' f = foldr (\x (xs, ys) -> if f x then (x : xs, ys) else (xs, x : ys)) ([], [])

-- foldr    :: (a -> ([b] -> [c]) -> [b] -> [c])
--          -> ([b] -> [c]) 
--          -> [a] 
--          -> ([b] -> [c])
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = foldr (\x g ys -> case ys of 
                            []      -> []
                            (y:ys') -> f x y : g ys') (const [])
                                                   -- (\_ -> []) es lo mismo que ignorar el parametro

-- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f = foldr (\x ys -> let (y:ys') = ys in f x y : ys') . (:[])

-- foldr :: (a -> [a] -> [a]) -> [a] -> [a] -> [a]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x ys -> singularSi x (f x) ++ ys) []

-- foldr :: (a -> (Int -> [a]) -> Int -> [a]) -> (Int -> [a]) -> [a] -> (Int -> [a])

take' :: Int -> [a] -> [a]
-- take' :: [a] -> (Int -> [a])
take' = flip (foldr (\x g n -> if n == 0 then [] else x : g (n-1)) (const []))

-- foldr :: (a -> (Int -> [a]) -> Int -> [a]) -> (Int -> [a]) -> [a] -> (Int -> [a])
drop' :: Int -> [a] -> [a]
-- drop' :: [a] -> (Int -> [a])
drop' = flip (foldr (\x g n -> if n == 0 then x : g 0 else g (n-1)) (const []))

-- foldr :: (a -> (Int -> a) -> Int -> a) -> (Int -> a) -> [a] -> (Int -> a)
elemAt' :: Int -> [a] -> a
-- elemAt' :: [a] -> (Int -> a)
elemAt' = flip (foldr (\x g n -> if n == 0 then x else g (n-1)) (const (error "404 not found") ))


--flip :: (a -> b -> c) -> b -> a -> c
--flip f = h
--    where h x = k
 --       where k y = (f y) x

--subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))
--subst f = h
--    where h g = k
--        where k x = (f x) (g x)

-- SIN WHERE

{-
compose :: (b -> c) -> (a -> b) -> a -> c   
compose f g x = f (g x)

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
subst f g x = f x (g x)

const :: b -> (a -> b)
const x y = x


-}