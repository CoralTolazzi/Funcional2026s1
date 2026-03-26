--Ejercicio 1) Indicar los tipos de las siguientes definiciones:
first :: (a,b) -> a
first (x,y) = x

apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

--apply recibe un tipo que es a en b y retorna c

{-
apply :: (a -> b) -> (a -> b)
f :: a -> b
-------------
apply f :: a -> b

g :: a -> b
x :: a
-------------
g x :: b

f :: a -> b
x :: a
-------------
f x :: b
-}

twice :: (a -> a) -> (a -> a)
twice f = g
    where g x = f (f x)

{-
twice :: (a -> a) -> (a -> a)
f :: a -> a
--------------
twice f :: a -> a

g :: a -> a
x :: a
--------------
g x :: a


f :: a -> a
f x :: a
--------------
f (f x) :: a

f :: a -> a
x :: a
--------------
f x :: a
-}

doble :: Int -> Int
doble x = x + x
{-
doble :: Int -> Int
x :: Int
----------
doble x :: Int

x :: Int
x :: Int
------------
x + x :: Int
-}

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f = g
    where g p = f (swap p)


{-
uflip :: ((b,a) -> c) -> ((a,b) -> c)
f ::  (b,a) -> c
--------------
uflip f :: (a,b) -> c

g :: (a,b) -> c
p :: (a,b)
--------------
g p :: c

f :: (b,a) -> c
swap p :: (b,a)
---------------
f (swap p) :: c

swap :: (a,b) -> (b,a)
p :: (a,b)
------------------
swap p :: (b,a)
-}


--Ejercicio 2) Dadas las definiciones anteriores, indicar el tipo de las siguientes expresiones:

--apply first :: c


{-
apply :: ((d, e) -> d) -> ((d, e) -> d)
first :: (d, e) -> d
------------
apply first :: (d, e) -> d

a -> b = (d, e) -> d
a = (d, e)
b = d
-}

--first (swap, uflip)

{-

first :: ((a,b) -> (b,a) , ((b,a) -> c) -> ((a,b) -> c)) -> (a,b) -> (b,a)
(swap, uflip) :: ((a,b) -> (b,a) , ((b,a) -> c) -> ((a,b) -> c))
----------------
first (swap, uflip) :: (a,b) -> (b,a)

d = (a,b) -> (b,a)
e = ((b,a) -> c) -> ((a,b) -> c)
-}

twice doble

twice doble


-- d. twice twice
-- e. twice uflip
-- f. twice swap
-- g. uflip swap
-- h. (twice twice) swap
