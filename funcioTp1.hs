-- Escribir ocho expresiones que denoten al número 4, diferentes a las ya
-- vistas (4, 2+2, 3+1, doble 2, doble (1+1)). Al menos seis deben usar
-- funciones (o sea, no ser de la forma de una cuenta de sumas o restas), de las
-- cuales: al menos dos deben usar una expresión lambda, tres deben usar doble y
-- una debe usar cuadruple.
-- Recordar que las definiciones dadas para doble y cuadruple son las siguientes
doble x = x + x
cuadruple x = 4*x 

a = (\x-> x + x ) 2
b = (\x -> (\y -> x - y )) 6 2 
c = cuadruple 2 - (-(doble 2) + (doble 4))  
d = (\x -> (\y -> (\z -> x - y + z ))) 4 8 8
e = (\x-> doble x - x ) 4
f = (\x-> x + x + x + x - doble x ) 2


--Ejercicio 2) Mostrar que la expresión doble (doble 2) puede reducirse de otras
--formas que la vista en clase. Sugerencia: considerar el doble más externo, en
--lugar del interno.

doble (doble 2) --Def de doble con x <- doble 2
doble 2 + doble 2  --Def de doble con x <- 2
(2+2) + (2+2) --Def de la suma
4 + 4 --Def de la suma
8

--Ejercicio 3) Reducir cuadruple 2, y cuadruple (cuadruple 2). ¿Alguna de
--ellas puede hacerse de más de una forma?

cuadruple 2 --Def de cuadruple, con x <- 2
4 * 2 --Def de producto
8

cuadruple (cuadruple 2)  -- Def de cuadruple con x <- 2
cuadruple (4*2) --Def de producto
cuadruple 8 --Def de cuadruple, con x <- 8
(4*8) --Def de producto
32

cuadruple (cuadruple 2) --Def de cuadruple con x <- cuadruple 2
4 * cuadruple 2 --Def de cuadruple con x <- 2
4 * (4*2) --Def de producto
4 * 8 --Def de producto
32

--4.Definir las funciones triple, succ, sumarDos.
--5.Comprobar que twice succ = sumarDos.
--Dar expresiones lambda que sean equivalentes a las siguientes expresiones:

--succ
succ x = x + 1
\x -> x + 1

--triple
triple x = 3 * x
\x -> 3 * x

--sumarDos
sumarDos x = x + 2
\x -> x + 2

--twice
twice f = g
    where g x = f(f x)

\f ->  g
    where g x = f(f x)
    
\f -> (\x -> f(f x))

--twice twice
\x -> (\f -> (\x -> f(f x))) ((\f -> (\x -> f(f x))) x) 

\f -> (\x -> f(f(f(f x))) 



twice succ = sumarDos 
--Def de twice con f <- succ
-- le aplico n a los 2 lados
g n = sumarDos n
--donde g x = succ (succ x)
succ (succ n) = sumarDos n
--Def de succ
succ (n + 1) = sumarDos n
--Def de succ con x <- x + 1
(n + 1) + 1 = sumarDos n
--Def de sumarDos
n + (1 + 1) = n + 2
--Propiedad asociativa de la suma, cambiar de lugar los parentesis
n + 2 = n + 2

twice succ = sumarDos 
--Reemplazo por sus Lambdas

(\f -> (\x -> f(f x))) succ  = sumarDos 
\x -> succ (succ x) = sumarDos 
\x -> succ (x + 1) = sumarDos 
\x -> (x + 1) + 1 = sumarDos 
\x -> x + 2  = sumarDos 
\x -> x + 2  = (\x -> x + 2 )


--Asi se prueba:
-- (\f -> (\x -> f(f x))) (\x -> x + 1) 1
--3

--Dar tres ejemplos de pares de expresiones que sean equivalentes entre
--sí, donde al menos una de ellas sea una expresión atómica, pero no estén
--vinculadas por reducción (además de cuadruple y \x -> 4*x). Dos de ellas
--deben no contenter lambdas y al menos una debe contener una expresión no
--atómica.

4 = doble 2
twice succ 2 = (\x -> x + 1) 3
twice doble = cuadruple

-- Realizar la reducción completa de ((twice twice) doble) 3.
twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

((twice twice) doble) 3
--Def de twice con f <- twice
(g doble) 3
--Donde g x = twice (twice x)
(twice (twice doble)) 3
--Def de twice con f' <- doble
(twice g') 3
--Def de twice con f'' <- g'
g'' 3
--Donde g'' x = g' (g' x)
g' (g' 3)
--Donde g' x = doble (doble x)
doble (doble (g' 3))
--Donde g' x = doble (doble x)
doble (doble (doble (doble 3)))
--Def de doble
doble (doble (doble (3 + 3)))
--Def de doble
--Def de suma
(doble (doble (6 + 6)))
--Def de doble
--Def de suma
(doble (12 + 12)))
--Def de doble
--Def de suma
(24 + 24)
--Def de suma
48


twice f = g
    where g x = f(f x)



--Estudiar cómo es la sintaxis y cómo se utilizan expresiones basadas en
--let, if-then-else, case y where. Reescribir las siguientes funciones sin el uso
--de let, where o if-then-else cuando sea posible.


f x = let (y,z) = (x,x) in y
f x = x

f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
f (x,y) = x

f p = case p of (x,y) -> x
f (x,_) = x

f = \p -> let (x,y) = p in y
f = \p -> snd p