doble :: Int -> Int
doble x = x + x

id :: x -> x
id x = x

suma :: Int -> Int -> Int 
suma x = g
  where g y = x + 2

apply :: (a -> b) -> (a -> b)
apply f = g
  where g x = f x

const :: a -> b -> a
const x = g
  where g y = x

twice :: (a -> a) -> (a -> a)
twice f = g
  where g x = f (f x)

comp :: (b -> c) -> ((a -> b) -> (a -> c))
comp f = h
  where h g = k
          where k x = f (g x)

-- comp :: (b -> c) -> ((a -> b) -> (a -> c))
-- f :: b -> c
-- ------------------
-- comp f :: (a -> b) -> (a -> c)

-- h :: (a -> b) -> (a -> c)
-- g :: a -> b
-- ----------------
-- h g :: a -> c

-- k :: a -> c
-- x :: a
-- ----------------
-- k x :: c

-- f :: b -> c
-- g x :: b
-- --------------
-- f (g x) :: c

-- g :: a -> b
-- x :: a
-- ----------------
-- g x :: b

flip :: (c -> d) -> (c -> (a -> d))
flip f = g
  where g x = h
          where h y = (f y) x

subst :: (a -> (c -> d)) -> ((a -> c) -> (a -> d))
subst f = h
  where h g = k
          where k x = (f x) (g x)



-- 1)
a1 = (\f -> f 2 + f 4) id
-- id 2 + id 4
-- 2 + 4
-- 6

b1 = (\f -> f 2 + f 4) doble
-- doble 2 + doble 4
-- (2+2) + (4+4)
-- 4 + 8
-- 12

c1 = (\f -> f 2 + f 4) (suma 17)
-- 19 + 21
-- 30

d1 = (\f -> f 2 + f 4) (const 21)
-- 21 + 21
-- 42


-- 2)
a2 = (suma 2) 3
-- \x -> \y -> x + y 
-- 2 + 3
-- 5 

b2 = ((subst const) suma) 17
-- (((\f -> \g -> \x -> (f x) (g x)) const) suma) 17
-- ((\g -> \x -> (const x) (g x)) suma) 17
-- ((\x -> (const x) (suma x)) 17
-- (((const 17) (suma 17))
-- 17 

c2 = ((subst const) twice) doble
-- (((\f -> \g -> \x -> (f x) (g x)) const) twice) doble
-- ((\g -> \x -> (const x) (g x)) twice) doble
-- ((\g -> \x -> x) twice) doble
-- (\x -> x) doble
-- doble


-- 3) A partir de las reducciones anteriores, calcular expresiones equivalentes para
a3 = ((subst const) suma) = \x -> (\y -> \p -> y + p x)

b3 = \f -> (subst const) f = \f -> (\x -> x)

-- 4) Dar expresiones equivalentes que nombren a las funciones definidas
a4 = id = \x -> x
b4 = const = \x -> \y -> x
c4 = flip  = \f -> \x -> \y -> (f y) x


-- 5) Completar la definición de la función yTambien de forma tal que para todos b1 y b2 ...
--  (yTambien b1) b2 = b1 && b2
yTambien :: 
yTambien b = if b then ?? else ??

yTambien :: Bool -> (Bool -> Bool)
yTambien b = f where f b' = if b then b' else b

-- 6) Mostrar reducción de
((subst (flip apply)) suma) 17
((subst ((\f -> \x -> \y -> (f y) x) apply)) suma) 17
((subst (\x -> \y -> (apply y) x)) suma) 17
((subst (\x -> \y -> y x)) suma) 17
(((\h -> \g -> \z -> (h z) (g z)) (\x -> \y -> y x)) suma) 17
(((\g -> \z -> ((\x -> \y -> y x) z) (g z))) suma) 17
((\g -> \z -> (\y -> y z) (g z)) suma) 17
((\g -> \z -> (\y -> y z) (g z)) suma) 17
((\g -> \z -> g z z) suma) 17
(\z -> suma z z) 17
suma 17 17
34

subst = \h -> \g -> \z -> (h z) (g z)
flip = \f -> \x -> \y -> (f y) x
-- 7) A partir de la reducción anterior dar una expresión equivalente para
(subst (flip apply)) suma = \z -> suma z z
