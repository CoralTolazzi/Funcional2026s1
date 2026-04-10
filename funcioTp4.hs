--Ejercicio 1) Determinar si las siguientes funciones son parciales o totales. Justificar.
--a. udiv (x,y) = div x y
--parcial porque y != 0

--b. udivE (x,0) = error "No puedo dividir por 0"
--udivE (x,y) = div x y
--Parcial porque aun asi te da error

--c. udivH = uncurry div
-- parcial porque div no puede tener un parametro con 0

--d. succ x = x + 1
--Total

--e. succH = suma 1
--Total

--f. porLaMitad = flip div 2
--Total

--g. conDieresis 'u' = 'ü'
-- Parcial si le pasas algo distinto al parametro definido explota

--h. conDieresisB 'u' = 'ü'
--   conDieresisB c = conDieresisB c
--Parcial porque es infinito 

--i. conTildePM 'a' = 'á'
--   conTildePM 'e' = 'é'
--   conTildePM 'i' = 'í'
--   conTildePM 'o' = 'ó'
--   conTildePM 'u' = 'ú'
-- Parcial si le pasas algo distinto al parametro definido explota

--j. conTildeE c = if esVocal c
--                 then conTildePM c
--                 else error "El valor recibido no es vocal"
--Parcial

--k. conTilde c = if esVocal c && esMinuscula c
--                  then conTildePM c
--                  else c
--Total


--Ejercicio 2) Para cada una de las funciones del ejercicio anterior, determinar si una o
--más de las otras es equivalente a ella.
--respuesta: 
--d y e
--a b y c
--g y h
--i y j

--Ejercicio 3) Dada la siguiente definición para la función twice,
--twice = \f -> \x -> f (f x)
--determinar cuántos y cuáles son los redexes en las siguientes expresiones.

--a. twice doble       1 por twice = \f -> \x -> f (f x)

     -----

--(\f -> \x -> f (f x)) doble
  ---------------------------

-- \x -> doble (doble x)
        --------------- 
                -------

-- \x -> (doble x) + (doble x)
          -------    -------

-- \x -> (x + x) + (x + x)


--b. (twice doble) 2
    -----

-- ((\f -> \x -> f (f x)) doble) 2
    ---------------------------
-- (\x -> doble (doble x)) 2
         -------

--c. twice
   -----



--Ejercicio 4) Dada la siguiente definición para la función twice,
twice f = g
    where g x = f (f x)
--determinar cuántos y cuáles son los redexes en las siguientes expresiones.

--a. twice doble
    -----------   


--b. twice doble 2
     -----------

--c. twice

--Ejercicio 5) Dada la siguiente definición para la función twice,
--twice f x = f (f x)
--determinar cuántos y cuáles son los redexes en las siguientes expresiones.

--a. twice doble
    
--b. twice doble 2
      ------------

--c. twice

--Ejercicio 6) Para cada tipo a continuación, intentar dar dos expresiones que denoten
--valores diferentes. Las expresiones deben ser diferentes de bottom, y en el caso de
--ser funciones, una debe ser total y otra debe ser parcial. De no ser posible hacer
--alguno de los casos, explicar por qué.

--a. a

--b. Int -> a

--c. a -> b
