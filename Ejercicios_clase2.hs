apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

const :: b -> (a -> b)
const x y = x

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))
subst f g x = (f x) (g x)

--flip :: (c -> d) -> c -> a -> d
--flip f x y = (f y) x

suma :: Int -> (Int -> Int)
(suma x) y = x + y

doble :: Int -> Int
doble x = x + x

{-
2)Para cada una de las siguientes expresiones

i)suma . doble 2
ii)doble . suma 2
iii)doble . doble 2

a. Agregar todos los parentesis necesarios para que la expresion tenga tipo
b. Dar el tipo de la expresion
c. Escribir la misma Expresion con Maxima cantidad de parentesis invisibles
d. Dar una expresion atómica diferente

i)
((suma .) doble) 2
(\g x -> suma (g x)) doble 2
suma (doble 2)
suma 4

suma . :: Int -> (Int -> Int)
doble :: Int -> Int

(.) :: (Int -> (Int -> Int)) -> ((a -> Int) -> (a -> (Int -> Int)))
suma :: Int -> (Int -> Int)
--------------------------------
(suma .) :: (a -> Int) -> a -> Int -> Int

(suma .) :: (a -> Int) -> a -> Int -> Int
doble :: Int -> Int
------------------------------------
(suma .) doble :: Int -> Int -> Int


(suma .) doble :: Int -> Int -> Int
2 :: Int
---------------------------------------
(suma .) doble 2 :: Int -> Int

((suma .) doble) 4
8
-}


{-
ii)doble . suma 2
(doble .) (suma 2)

(doble .) :: (Int -> Int) -> Int -> Int
suma 2 :: Int -> Int
-----------------------------------------
(doble .) (suma 2) :: Int -> Int


(doble .) (suma 2)
\x -> doble ((suma 2) x)

\x -> doble (suma 2 x)
\x -> doble (2 + x)
\x -> (2 + x) + (2 + x)
\x ->(4 + x * 2)


\g x -> doble (suma 4)
\x -> 8 + x * 2


-}

{-
doble . doble 2
(doble .) doble 2
(\g x -> doble (g x)) doble 2
doble (doble 2)
8


(doble .) :: (Int -> Int) -> Int -> Int
doble :: Int -> Int
-----------------------------------------
(doble .) doble :: Int -> Int

(doble .) doble :: Int -> Int
2 :: Int
-----------------------------------------
(doble .) doble 2 :: Int


-}