{-
1) Establecer el tipo de (.) (.) (.)

2) Dar una expresion atómica equivalente que no sea (.) (Ni compose)
Sugerencia: Completar con parametros y , el ejercicio anterior como guía y
reducir el cuerpo de la funcion restante

3) Determinar si (.) (.) (.) succ div es parcial o total

4) Determinar si (.) (.) (.) es parcial o total

5) Determinar si yTambien (de la clase 1) es estricta o no estricta
-}

{-
Ejercicio 1)
paso 1:
(.) :: (e -> f) -> (d -> e) -> d -> f
(.) :: (b -> c) -> (a -> b) -> a -> c
-----------------------------------------
(.) (.) :: ---

--------------------------------
(b -> c) -> (a -> b) -> a -> c
=
(e -> f)

e = (b -> c)

f = ((a -> b) -> a -> c)

---------------------------------------

paso 2:

(.) :: ((b -> c) -> (a -> b) -> a -> c) -> (d -> (b -> c)) -> d -> ((a -> b) -> a -> c)
(.) :: (b -> c) -> (a -> b) -> a -> c
-----------------------------------------
(.) (.) :: (d -> b -> c) -> d -> (a -> b) -> a -> c

paso 3:

(.) (.) :: (d -> b -> c) -> d -> (a -> b) -> a -> c
(.) :: (j -> h) -> (i -> j) -> i -> h
---------------------------------------------
(.) (.) (.) :: -----

----------------------
(j -> h) -> (i -> j) -> i -> h
=
d -> b -> c


d = (j -> h)

b -> c = ((i -> j) -> i -> h)

b = (i -> j)
c = (i -> h)
--------------------

paso 4:

(.) (.) :: ((j -> h) -> (i -> j) -> i -> h) -> (j -> h) -> (a -> (i -> j)) -> a -> (i -> h)
(.) :: (j -> h) -> (i -> j) -> i -> h
---------------------------------------------
(.) (.) (.) :: (j -> h) -> (a -> i -> j) -> a -> i -> h

Ejercicio 2)
(.) (.) (.) 

(.) (.) (.) :: (j -> h) -> (a -> i -> j) -> a -> i -> h
(.) (.) (.) = \f g x y -> f (g x y)
-}
{-
(.) (.) (.) succ div
\f g x y -> f (g x y)
succ (div x y)_

Ejercicio 3)
(.) (.) (.) succ div 1 0 da ERROR

(.) (.) (.) succ div 1 0
succ (div 1 0)

Ejercicio 4)
(.) (.) (.)

Ejercicio 5)
yTambien :: Bool -> Bool -> Bool
yTambien b b' = if b then b' else b

necesitas un segundo parámetro
estricta


-}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

doble :: Int -> Int
doble x = x + x


uflip :: ((b,a) -> c) -> ((a,b) -> c)
uflip f p = f (swap p)

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)
