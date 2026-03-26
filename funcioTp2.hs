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

{-
twice :: (Int -> Int) -> (Int -> Int)
doble :: Int -> Int
-----------------------------------
twice doble :: (Int -> Int)
-}


{-
twice :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))
twice :: (a -> a) -> (a -> a)
-------------------------
twice twice :: (a -> a) -> (a -> a)

-}

{-
paso 1:
twice :: (d -> d) -> (d -> d)
uflip :: ((b, a) -> c) -> ((a, b) -> c) 
-------------------------
twice uflip :: ...

d = (b, a) -> c
d = (a, b) -> c

(b, a) -> c = (a, b) -> c
--por transitividad: (b, a) = (a, b) 
                     a = b
                     (a,a) = (a,a)

paso 2: Reemplazo "d" según paso 1
twice :: ((b, a) -> c) -> (b, a) -> c)) -> ((b, a) -> c) -> (b, a) -> c))
uflip :: ((b, a) -> c) -> ((a, b) -> c) 
-------------------------
twice uflip :: ...

paso 3: Reemplazo "b" por la "a"
twice :: ((a, a) -> c) -> (a, a) -> c)) -> ((a, a) -> c) -> (a, a) -> c))
uflip :: ((a, a) -> c) -> ((a, a) -> c) 
-------------------------
twice uflip :: ((a, a) -> c) -> ((a, a) -> c)

-}

{-
paso 1 
twice :: (c -> c) -> (c -> c)
swap :: (a,b) -> (b,a)
-----------------------
twice swap :: 

c = (a,b)
c = (b,a)
b = a

paso 2
twice :: ((a,a) -> (a,a)) -> ((a,a) -> (a,a))
swap :: (a,a) -> (a,a)
-----------------------
twice swap :: (a,a) -> (a,a)
-}

{-
paso 1
uflip :: ((c, e) -> d) -> ((e, c) -> d) 
swap :: (a,b) -> (b,a)
--------------------------------------------
uflip swap ::  

(c, e) -> d = (a,b) -> (b,a)

d = (b,a)
(c, e) = (a,b)
Para que sean igual tiene que coincidir sus componentes
c = a
e = b

paso 2
uflip :: ((a,b) -> (b,a)) -> ((b, a) -> (b,a)) 
swap :: (a,b) -> (b,a)
--------------------------------------------
uflip swap :: (b, a) -> (b,a) 




-}

{-
paso 1
twice twice :: (a -> a) -> (a -> a)
swap :: (c,b) -> (b,c)
--------------------
(twice twice) swap :: 

a = (c,b)
a = (b,c)
c = b
paso 2

twice twice :: ((c,c) -> (c,c)) -> ((c,c) -> (c,c))
swap :: (c,c) -> (c,c)
--------------------
(twice twice) swap :: (c,c) -> (c,c)

-}

{-
const x = g
    where g y = x

const :: b -> (a -> b)
x :: b
-------------
const x :: a -> b

g :: a -> b
y :: a
----------------
g y :: b
-}

{-
appDup f = g
    where g x = f (x, x)

appDup :: ((a,a) -> b) -> (a -> b)
f :: (a,a) -> b
-----------------------------
appDup f :: a -> b


g :: a -> b
x :: a
--------------------
g x :: b

f :: (a,a) -> b
(x,x) :: (a,a)
--------------------
f (x,x) :: b
-}

{-
appFork (f, g) = h
    where h x = (f x, g x)

appFork :: (a -> c, a -> b) -> (a -> (c,b))
(f, g) :: (a -> c, a -> b)
--------------------------
appFork (f, g) :: a -> (c,b)

h :: a -> (c,b)
x :: a
------------------------
h x :: (c,b)

f :: a -> c
x :: a
---------------------
f x :: c

g :: a -> b
x :: a
---------------------
g x :: b
-}


{-
appPar (f, g) = h
    where h (x, y) = (f x, g y)

appPar ::  (c -> d, a -> b ) -> ((c,a) -> (d,b))
(f, g) :: (c -> d, a -> b )
-------------------
appPar (f, g) :: (c,a) -> (d,b)

h  :: (c,a) -> (d,b)
(x, y) :: (c,a)
--------------------------
h (x, y) :: (d,b)


f :: c -> d
x :: c
---------------
f x :: d

g :: a -> b
y :: a 
------------------
g y :: b
-}

{-
appDist f = g
    where g (x, y) = (f x, f y)

appDist :: (a -> b) -> ((a,b) -> (b,b))
f :: a -> b
-----------------------
appDist f :: (a,b) -> (b,b)


g :: (a,b) -> (b,b)
(x, y) :: (a,a)
-----------------------
g (x,y) :: (b,b)

f :: a -> b
x :: a
------------
f x :: b

f :: a -> b
y :: a
------------
f y :: b
-}


--flip f = h
--    where h x = k
--        where k y = (f y) x


{-
flip :: (c -> d) -> (c -> (a -> d))
f :: c -> d 
---------------
flip f :: c -> (a -> d)

h :: c -> (a -> d)
x :: c
---------------
h x :: a -> d

k :: a -> d
y :: a
-------------
k y :: d


f y :: c -> d
x :: c
-----------
(f y) x :: d

... -> ... = b
c -> d = b


f :: a -> (c -> d)
y :: a
-----------------
f y :: c -> d
-}

subst f = h
    where h g = k  where k x = (f x) (g x)

{-
subst f = h
    where h g = k
        where k x = (f x) (g x)
 
subst :: ( a -> (c -> d)) -> ((a -> c) -> (a -> d))
f :: a -> (c -> d)
------------------
subst f :: (a -> c) -> (a -> d)

h :: (a -> c) -> (a -> d)
g :: a -> c
-----------
h g :: a -> d


k :: a -> d
x :: a
----------
k x :: d

f x :: c -> d
g x :: c
------------------
(f x) (g x) :: d


f :: a -> (c -> d)
x :: a 
----------
f x :: c -> d

g :: a -> c
x :: a
-------------
g x :: c



-}

