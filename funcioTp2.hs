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
twice uflip :: 

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

a -> a = (c,b) -> (b,c)
a = (c,b)
a = (b,c)
c = b

paso 2
twice twice :: ((c,c) -> (c,c)) -> ((c,c) -> (c,c))
swap :: (c,c) -> (c,c)
--------------------
(twice twice) swap :: (c,c) -> (c,c)

-}
--Ejercicio 3)
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
flip :: (a -> c -> d) -> i -> a -> j
f :: a -> (c -> d)
----------------------
flip f :: i -> a -> j

e -> i -> j = a -> (c -> d)
e = a 
i -> j = c -> d

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
{-
Ejercicio 4) Para cada una de las siguientes expresiones decidir si poseen tipo. Si es
así indicar cuál es.

a. 1 && 2 == 2
No tiene tipo

b. 1 + if 3 < 5 then 3 else 5
Tiene tipo Int

c. let par = (True, 4)
in (if first par then first par else second par)
No tiene tipo

d. (doble doble) 5
No tiene Tipo

e. doble (doble 5)
Tiene tipo Int

f. twice first 

twice :: (c -> c) -> (c -> c)
first :: (a,b) -> a
-------------------------
twice first ::

c -> c = (a,b) -> a

c = (a,b)
c = a
(a,b) = a
MEH No Tipa porque empieza a ser infinito


g. (twice doble) doble 

Por esto: 
twice doble :: Int -> Int
doble :: Int -> Int
------------------
(twice doble) doble :: 

twice :: (Int -> Int) -> (Int -> Int)
doble :: Int -> Int
------------------
twice doble :: Int -> Int

Int -> Int = c -> c
c = Int
MEH No Tipa porque empieza a ser infinito


h.(twice twice) first

twice twice :: (a -> a) -> (a -> a)
first :: (c,b) -> c
-----------------------------------
(twice twice) first :: 

a -> a = (c,b) -> c
a = (c,b)
a = c
Por Transitividad:
(c,b) = c
((((c,b),b)),b) = c
No tipa, otra vez Infinito como los anteriores

i. apply apply

apply :: ((c -> d) -> (c -> d)) -> ((c -> d) -> (c -> d))
apply :: (c -> d) -> (c -> d)
------------------------------
apply apply :: (c -> d) -> (c -> d)

(c -> d) -> (c -> d) = a -> b

a = (c -> d)
b = (c -> d)
a = b
Si tiene tipo (c -> d) -> (c -> d)
-}
{-
Ejercicio 5) Dar dos ejemplos de expresiones que tengan cada uno de los siguientes
tipos:
a. Bool = True; False
b. (Int, Int) = (1,2); (2,1)
c. Char -> Int  = 
\c -> 1
\c -> 4
d. (Int, Char) -> Bool =
\(x,y) -> True
\(x,y) -> False
e. (Int -> Int) -> Int
\(x,y) -> 1
\(x,y) -> 2
f. (Bool -> Bool, Int)
(\b -> False, 1)
(\b -> True, 2)
g. a -> Bool
\x -> True
\x -> False
-}

{-
Ejercicio 6) Para cada una de las siguientes expresiones, decir a cuál función del
ejercicio 3 es equivalente. Ofrecer argumentos de por qué son equivalentes.
a. \p -> let (f, g) = p
    in \x -> (f x, g x)

h es la funcion anónima principal
k es una funcion anonima

h :: (a -> c, a -> b) -> (a -> (c,b))
(f,g) :: (a -> c, a -> b)
---------------
h (f,g) :: a -> (c,b)

k :: a -> (c,b)
x :: a
----------------
k x :: (c,b)

f :: a -> c
x :: a
---------------
f x :: c

g :: a -> b
x :: a
----------------
g x :: b

SON IGUALES!!!!


appFork :: (a -> c, a -> b) -> (a -> (c,b))
appFork (f, g) = h
    where h x = (f x, g x)


b. \f -> (\g -> (\x -> f x (g x)))


la principal funcion anonima se denomina k
k: \f -> (\g -> (\x -> f x (g x))
j: \g -> (\x -> f x (g x))
h: \x -> f x (g x)




subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))

=

k :: (a -> (b -> d)) -> ((a -> b) -> (a -> d))
f :: a -> (b -> d)
----------
k f :: (a -> b) -> (a -> d)

j :: (a -> b) -> (a -> d)
g :: a -> b
----------
j g :: a -> d

h :: a -> d
x :: a
---------
h x :: d

f x :: b -> d
g x :: b
---------------------
f x (g x) :: d

c = b -> d

f :: a -> (b -> d)
x :: a
-----------
f x :: b -> d

g :: a -> b
x :: a
----------
g x :: b


c. \f -> (\x -> (\y -> (f y) x))

flip = \f -> (\x -> (\y -> (f y) x))
flip f = \x -> (\y -> (f y) x)
flip f x = \y -> (f y) x
flip f x y = (f y) x


d. \f -> (\px -> let (x, y) = px
    in (f x, f y))

appDist = \f -> (\px -> let (x, y) = px in (f x, f y))
appDist f = \px -> let (x, y) = px in (f x, f y)
appDist f px = let (x, y) = px in (f x, f y)
appDist f (x,y) = (f x, f y)

=

appDist :: (a -> b) -> ((a,b) -> (b,b))


e. \x -> (\y -> x)
f x = \y -> x
f x y = x

=

const :: b -> (a -> b)
const x y = x


f. \pf -> let (f, g) = pf
    in \px -> let (x, y) = px
    in (f x, g y)


f = \pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y)
f (f, g) (x, y) = (f x, g y)

=

appFork :: (a -> c, a -> b) -> (a -> (c,b))
appFork (f, g) x = (f x, g x)

g. \f -> (\x -> f (x, x))
fun =\f -> (\x -> f (x, x))
fun f = \x -> f (x, x)
fun f x = f (x, x)

=

appDup :: ((a,a) -> b) -> (a -> b)
appDup f x = f (x, x)

-}

{-
Ejercicio 7) Encontrar cuales de estas expresiones son equivalentes entre sí.
Sugerencia: utilizar funciones anónimas es una forma interesante de encontrar
equivalencias entre expresiones que denotan funciones.

a. appFork (id,id)

(\(f, g) -> \x -> (f x, g x)) (id,id)
\x -> (id x, id x)
\x -> (x, x)

-------------------------------------
b. \f -> appDup (appDist f) = ?

appDup = \g -> \k -> g (k, k)
appDist = \h -> \(x,y) -> (h x, h y)

\f -> appDup (appDist f)
\f -> (\g -> \k -> g (k, k)) (appDist f)
\f -> \k -> (appDist f) (k, k)
\f -> \k -> ((\h -> \(x,y) -> (h x, h y)) f) (k, k)
\f -> \k -> (\(x,y) -> (f x, f y)) (k, k)
\f -> \k -> (f k, f k)

\f -> \x -> (f x, f x)

----------------------------------------------
c. appDup id

f x = f (x, x)

appDup = \g -> \k -> g (k, k)
\k -> id (k, k)
\x -> id (x, x)
\x -> (x, x)

------------------------------------------
d. appDup appFork

appDup = \g -> \k -> g (k, k)
appFork = \(f, h) -> \x -> (f x, h x)

(\g -> \k -> g (k, k)) appFork
\k -> appFork (k, k)
\k -> (\(f, h) -> \x -> (f x, h x)) (k, k)
\k -> (\x -> (k x, k x))

---------------------------------------
e. flip (appDup const)

flip = \f -> \h -> \y -> (f y) h
const = \j -> \l -> j
appDup = \g -> \k -> g (k, k)

(\f -> \h -> \y -> (f y) h) (appDup const)
(\f -> \h -> \y -> (f y) h) ((\g -> \k -> g (k, k)) const)
(\f -> \h -> \y -> (f y) h) (\k -> const (k, k))
(\f -> \h -> \y -> (f y) h) (\k -> (\j -> \l -> j) (k, k))
(\f -> \h -> \y -> (f y) h) (\k -> (\l -> (k, k)))
\h -> \y -> ((\k -> (\l -> (k, k))) y) h)
\h -> \y -> ((\l -> (y, y)) h)
\h -> \y -> (y, y)
------------------------------------------
f. const (appDup id)

const = \j -> \l -> j
appDup = \g -> \k -> g (k, k)

(\j -> \l -> j) ((\g -> \k -> g (k, k)) id)
(\j -> \l -> j) (\k -> id (k, k))
(\j -> \l -> j) (\k -> (k, k))
\l -> (\k -> (k, k))
------------------------------------------

RESPUESTA:
a = c
b = d
e = f


-}