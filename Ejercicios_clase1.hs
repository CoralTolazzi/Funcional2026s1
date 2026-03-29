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
\x -> \y -> x + y 
2 + 3
5 


b2 = ((subst const) suma) 17

(((\f -> \g -> \x -> (f x) (g x)) (\z -> \w -> z)) (\y -> \p -> y + p)) 17
(((\g -> \x -> ((\z -> \w -> z) x) (g x))) (\y -> \p -> y + p)) 17
((\g -> \x -> ((\w -> x) (g x))) (\y -> \p -> y + p)) 17
((\g -> \x -> (g x)) (\y -> \p -> y + p)) 17
(\x -> (\y -> \p -> y + p x)) 17
\y -> \p -> y + p 17


c2 = ((subst const) twice) doble

((\g -> \x -> (g x)) (\f -> \y -> f (f y))) \


















-- 3) A partir de las reducciones anteriores, calcular expresiones equivalentes para
a3 = ((subst const) suma)
b3 = \f -> (subst const) f


-- 4) Dar expresiones equivalentes que nombren a las funciones definidas
a4 = id
b4 = const
c4 = flip


-- 5) Completar la definición de la función yTambien de forma tal que para todos b1 y b2 ...
--  (yTambien b1) b2 = b1 && b2
yTambien :: ...
yTambien b = if b then ?? else ??


-- 6) Mostrar reducción de
((subst (flip apply)) suma) 17


-- 7) A partir de la reducción anterior dar una expresión equivalente para
(subst (flip apply)) suma

