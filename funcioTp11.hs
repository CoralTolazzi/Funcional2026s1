-- Ejercicio 1) Definir las siguientes funciones utilizando recursión estructural explícita
-- sobre Pizza:

data Pizza = Prepizza | Capa Ingrediente Pizza
                                    deriving Show

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa
                                    deriving Show

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = unoSi (f i) + cantidadCapasQueCumplen f p

unoSi True  = 1
unoSi False = 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza = Prepizza 
soloLasCapasQue f (Capa i p) = if f i then Capa i (soloLasCapasQue f p) else soloLasCapasQue f p

-- Ejercicio 2) Definir las siguientes funciones utilizando alguna de las definiciones
-- anteriores:

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

esQueso Queso = True
esQueso _     = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso

{-
aptaIntolerantesLactosa p = ((==0) . cantidadCapasQueCumplen esQueso) p
aptaIntolerantesLactosa p = (==0) (cantidadCapasQueCumplen esQueso p)
aptaIntolerantesLactosa p = (==) 0 (cantidadCapasQueCumplen esQueso p)
aptaIntolerantesLactosa p = 0 == cantidadCapasQueCumplen esQueso p
-}

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas aceitunaPorDos

aceitunaPorDos :: Ingrediente -> Ingrediente
aceitunaPorDos (Aceitunas n) = Aceitunas (n*2)
aceitunaPorDos i = i

-- Ejercicio 3) Definir

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada _ x Prepizza   = x
pizzaProcesada f x (Capa i p) = f i (pizzaProcesada f x p)

-- que expresa la definición de fold para la estructura de Pizza.

-- Ejercicio 4) Resolver todas las funciones de los puntos 1) y 2) utilizando la función
-- pizzaProcesada.

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f p = pizzaProcesada (\i n -> (+) (unoSi (f i)) n) 0 p

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f p = pizzaProcesada (\i pz -> Capa (f i) pz) Prepizza p

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f p = pizzaProcesada (\i pz -> capaSi i (f i) pz) Prepizza p

capaSi :: Ingrediente -> Bool -> Pizza -> Pizza
capaSi i True pz = Capa i pz
capaSi _ False pz = pz

sinLactosa' :: Pizza -> Pizza
sinLactosa' p = pizzaProcesada (\i pz -> capaSi i (not (esQueso i)) pz) Prepizza p

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' p = pizzaProcesada (\i b -> esQueso i && b) False p 
 
cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' p = pizzaProcesada (\i n -> unoSi (esQueso i) + n) 0 p

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' p = pizzaProcesada (\i pz -> Capa (aceitunaPorDos i) pz ) Prepizza p

-- Ejercicio 5) Resolver las siguientes funciones utilizando pizzaProcesada (si resulta
-- demasiado complejo resolverlas, dar primero una definición por recursión estructural
-- explícita, y usar la técnica de los “recuadros”):

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas p = pizzaProcesada (\i pz -> (cantidadDeAceitunas i) + pz) 0 p

cantidadDeAceitunas :: Ingrediente -> Int
cantidadDeAceitunas (Aceitunas n) = n
cantidadDeAceitunas _             = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
-- capasQueCumplen f p = pizzaProcesada (\i pz -> singularSi i (f i) ++ pz ) [] p

capasQueCumplen = flip (pizzaProcesada (\i pz f -> singularSi i (f i) ++ pz f) (const []))

-- pizzaProcesada :: (Ingrediente -> ((Ingrediente -> Bool) -> [Ingrediente]) -> (Ingrediente -> Bool) -> [Ingrediente])
--                -> ((Ingrediente -> Bool) -> [Ingrediente])
--                -> Pizza
--                -> ((Ingrediente -> Bool) -> [Ingrediente])

singularSi i True  = [i]
singularSi _ False = []

conDescripcionMejorada' :: Pizza -> Pizza
conDescripcionMejorada' p = pizzaProcesada unirAceitunas Prepizza p

unirAceitunas :: Ingrediente -> Pizza -> Pizza
unirAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
unirAceitunas i p = Capa i p

-- Agrega las capas de la primera pizza sobre la segunda
conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe p1 p2 = pizzaProcesada (\i pz -> Capa i pz) p2 p1

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas n p = pizzaProcesada (\i pz m -> if m > 0 then Capa i (pz (m-1)) else Prepizza) (\_ -> Prepizza) p n

-- pizzaProcesada :: (Ingrediente -> (Int -> Pizza) -> Int -> Pizza) -> (Int -> Pizza) -> Pizza -> (Int -> Pizza)

p1 = Capa Jamón $ Capa Queso $ Capa Salsa $ Capa Anchoas Prepizza
p2 = Capa Jamón $ Capa Queso Prepizza
p3 = Capa (Aceitunas 3) $ Capa (Aceitunas 3) Prepizza

{-
f xs ys zs = xs ++ ys ++ zs
f = (. (++)) . (.) . (++)
f = (++)

 Ejercicio 6) Demostrar las siguientes propiedades sobre el tipo Pizza, utilizando las
 definiciones por recursión estructural explícita para cada función
================================================================================
a. para todo f. length . capasQueCumplen f = cantidadCapasQueCumplen f

¿para todo f. length . capasQueCumplen f = cantidadCapasQueCumplen f?

Por ppio de ext. ¿para todo pz. para todo f. (length . capasQueCumplen f )pz = cantidadCapasQueCumplen f pz?
=                                                                                (def. (.))
¿para todo pz .para todo f. length (capasQueCumplen f pz)= cantidadCapasQueCumplen f pz?

Sean p :: Pizza, g :: (a -> b), por ppio de ind. sobre la estructura de p, se verá que:

Caso base, p = Prepizza:
    ¿length (capasQueCumplen g Prepizza)= cantidadCapasQueCumplen g Prepizza?
Caso ind. p = (Capa i ps):
    HI) ¡length (capasQueCumplen g ps) = cantidadCapasQueCumplen g ps!
    TI) ¿length (capasQueCumplen g (Capa i ps))= cantidadCapasQueCumplen g (Capa i ps)?

Caso Base
IZQ:
length (capasQueCumplen g Prepizza)
        ----------------------------
=                       (capasQueCumplen.1)
length (pizzaProcesada (\i pz -> singularSi i (g i) ++ pz ) [] Prepizza)
=                                               (pizzaProcesada.1)
length []
=                                               (length.1)
0

DER:
cantidadCapasQueCumplen g Prepizza
=                                    (cantidadCapasQueCumplen.1)
0

Vale para este Caso

Caso IND:
IZQ:
length (capasQueCumplen g (Capa i ps))
=                               (capasQueCumplen)
length (pizzaProcesada (\i pz -> singularSi i (g i) ++ pz ) [] (Capa i ps))
=                                       (pizzaProcesada.2)
length ((\h pz -> singularSi h (g h) ++ pz ) i (pizzaProcesada (\h pz -> singularSi h (g h) ++ pz ) [] ps))
=                                                                               (resolver Lambdas)
length ((\pz -> singularSi i (g i) ++ pz ) (pizzaProcesada (\h pz -> singularSi h (g h) ++ pz ) [] ps))
=                                                                               (resolver Lambdas)
length (singularSi i (g i) ++ pizzaProcesada (\h pz -> singularSi h (g h) ++ pz ) [] ps)
=                                                                                        (Lema.1)
length (singularSi i (g i)) + length (pizzaProcesada (\h pz -> singularSi h (g h) ++ pz ) [] ps))
=                                                                                          (def capasQueCumplen)
length (singularSi i (g i)) + length (capasQueCumplen g ps)
=                                        (HI)
length (singularSi i (g i)) + cantidadCapasQueCumplen g ps
=                                             (LEMA.2)
unoSi (g i) + cantidadCapasQueCumplen g ps
=                                   (cantidadCapasQueCumplen.2)
cantidadCapasQueCumplen g (Capa i ps)

Vale para este Caso. Vale la 
================================================================================
LEMA.2
length (singularSi x (g x)) = unoSi (g x) 

(g x) :: Bool

Casos:
g x = True

length (singularSi x True) = unoSi True

length (singularSi x True) 
1
unoSi True
1

False
length (singularSi x False)
0
unoSi False
0

Vale la propiedad
================================================================================
LEMA.1
¿para todo as. bs. length (as ++ bs) = length as + length bs?
Dem: Sean ps::[a], ws::[a] length (ps ++ ws) = length (ps) + length ws
Caso ps = []
IZQ:
length ([] ++ ws)
        ---------
=               ((++).1)
length ws

DER:   
length [] + length ws
-----------
=                  (length.1)
0 + length ws
=                  (aritmetica)
length ws

Caso ps = (h:hs)

IZQ:
length ((h:hs) ++ ws)
length (h : (hs ++ ws))        def. (++)
1 + length (hs ++ ws)          def. length
1 + length hs + length ws      HI: length (hs ++ ws) = length hs + length ws

DER:
length (h:hs) + length ws
1 + length hs + length ws      def. length
Vale para este caso
================================================================================
b. para todo f. para todo p1. para todo p2.
      cantidadCapasQueCumplen f (conCapasDe p1 p2) = 
        cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p

¿para todo f. para todo p1. para todo p2.
      cantidadCapasQueCumplen f (conCapasDe p1 p2) = 
        cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p?

Sean pz1 :: Pizza, pz2 :: Pizza, g :: (a -> b), por ppio de ind. sobre la estructura de pz1, se verá que:

Caso base, pz1 = Prepizza:
    ¿cantidadCapasQueCumplen g (conCapasDe Prepizza pz2) = cantidadCapasQueCumplen g Prepizza + cantidadCapasQueCumplen g pz2?
Caso ind. pz1 = (Capa i ps):
    HI) ¡cantidadCapasQueCumplen g (conCapasDe ps pz2) = cantidadCapasQueCumplen g ps + cantidadCapasQueCumplen g pz2!
    TI) ¿cantidadCapasQueCumplen g (conCapasDe (Capa i ps) pz2) = cantidadCapasQueCumplen g (Capa i ps) + cantidadCapasQueCumplen g pz2?

Caso Base
cantidadCapasQueCumplen g (conCapasDe Prepizza pz2)
cantidadCapasQueCumplen g (pizzaProcesada (\i pz -> Capa i pz) pz2 Prepizza)
cantidadCapasQueCumplen g pz2

cantidadCapasQueCumplen g Prepizza + cantidadCapasQueCumplen g pz2
0 + cantidadCapasQueCumplen g pz2
cantidadCapasQueCumplen g pz2

Vale para este caso

Caso Ind
IZQ:
cantidadCapasQueCumplen g (conCapasDe (Capa i ps) pz2)
cantidadCapasQueCumplen g (pizzaProcesada (\i pz -> Capa i pz) pz2 (Capa i ps))
cantidadCapasQueCumplen g ((\i pz -> Capa i pz) i (pizzaProcesada (\i pz -> Capa i pz) pz2 ps))
cantidadCapasQueCumplen g (Capa i (pizzaProcesada (\i pz -> Capa i pz) pz2 ps))
unoSi (g i) + cantidadCapasQueCumplen g (pizzaProcesada (\i pz -> Capa i pz) pz2 ps)

DER:
cantidadCapasQueCumplen g (Capa i ps) + cantidadCapasQueCumplen g pz2
unoSi (g i) + cantidadCapasQueCumplen g ps + cantidadCapasQueCumplen g pz2
unoSi (g i) + cantidadCapasQueCumplen g (conCapasDe ps pz2)                 (HI)
unoSi (g i) + cantidadCapasQueCumplen g (pizzaProcesada (\i pz -> Capa i pz) pz2 ps)

Vale para este caso Vale la Propiedad
================================================================================

c. para todo f. para todo p1. para todo p2.
    conCapasTransformadas f (conCapasDe p1 p2) =
    conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2

¿para todo f. para todo p1. para todo p2.
conCapasTransformadas f (conCapasDe p1 p2) =
conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2?



Sean pz1 :: Pizza, pz2 :: Pizza, g :: (a -> b), por ppio de ind. sobre la estructura de pz1, se verá que:

Caso base, pz1 = Prepizza:
    ¿conCapasTransformadas g (conCapasDe Prepizza pz2) = conCapasDe (conCapasTransformadas g Prepizza) (conCapasTransformadas g pz2?
Caso ind. pz1 = (Capa i ps):
    HI) ¡conCapasTransformadas g (conCapasDe ps pz2) = conCapasDe (conCapasTransformadas g ps) (conCapasTransformadas g pz2!
    TI) ¿conCapasTransformadas g (conCapasDe (Capa i ps) pz2) = conCapasDe (conCapasTransformadas g (Capa i ps)) (conCapasTransformadas g pz2?


Caso base:

conCapasTransformadas g (conCapasDe Prepizza pz2)
=                                       (conCapasDe)
conCapasTransformadas g (pizzaProcesada (\i pz -> Capa i pz) pz2 Prepizza)
=                                                       (pizzaProcesada.1)
conCapasTransformadas g Prepizza
=
pizzaProcesada (\i pz -> Capa (g i) pz) Prepizza Prepizza
=
Prepizza



conCapasDe (conCapasTransformadas g Prepizza) (conCapasTransformadas g pz2)
=                                      (conCapasTransformadas)
conCapasDe (pizzaProcesada (\i pz -> Capa (g i) pz) Prepizza Prepizza) (pizzaProcesada (\i pz -> Capa (g i) pz) Prepizza pz2)
=                                                       (pizzaProcesada.1)
conCapasDe Prepizza (pizzaProcesada (\i pz -> Capa (g i) pz) Prepizza pz2)
=
pizzaProcesada (\i pz -> Capa i pz) (\i pz -> Capa (g i) pz) Prepizza pz2) Prepizza
=
Prepizza

Caso IND:

conCapasTransformadas g (conCapasDe (Capa i ps) pz2)
=
conCapasTransformadas g (pizzaProcesada (\i pz -> Capa i pz) pz2 (Capa i ps))
conCapasTransformadas g ((\i pz -> Capa i pz) i (pizzaProcesada (\i pz -> Capa i pz) pz2 (Capa i ps)))

conCapasDe (conCapasTransformadas g (Capa i ps)) (conCapasTransformadas g pz2
=

???

================================================================================
 d. para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f

???
================================================================================
-}
-- Ejercicio 7) Definir las siguientes funciones de esquemas sobre listas, utilizando
-- recursión estructural de forma explícita:

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = [] 
map'' f (x:xs)= f x : map'' f xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' f (x:xs) = singularSi x (f x) ++ filter'' f xs    

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f y [] = y
foldr'' f y (x:xs) = f x (foldr'' f y xs)

recr'' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr'' y _ [] = y
recr'' y f (x:xs) = f x xs (recr'' y f xs) 

foldr1'' :: (a -> a -> a) -> [a] -> a
foldr1'' f [] = error "buee"
foldr1'' f [x] = x 
foldr1'' f (x:xs) = f x (foldr1'' f xs)

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs [] = []
zipWith'' f [] ys = []
zipWith'' f (x:xs) (y:ys) = f x y : zipWith'' f xs ys

scanr'' :: (a -> b -> b) -> b -> [a] -> [b]
scanr'' f y [] = [y]
scanr'' f y (x:xs) = let (z:zs) = scanr'' f y xs in  f x y : z:zs
{-
-- Ejercicio 8) Demostrar las siguientes propiedades utilizando las definiciones
-- anteriores (y algunas otras de prácticas anteriores):

-- a. map id = id


¿map id = id?


Por ppio de ext. ¿para todo ys. . map id ys = id ys?

Sean xs :: [a] , por ppio de ind. sobre la estructura de xs, se verá que:
Caso base, xs = []:
    ¿map id [] = id []?
Caso ind. xs = (z:zs):
    HI) ¡map id zs = id zs!
    TI) ¿map id (z:zs) = id (z:zs)?


Caso Base
Vale para este caso (muy facil)

Caso Ind
map id (z:zs)
id z : (map g zs)
id z : id zs
z:zs

id (z:zs)
z:zs


-- b. para todo f. para todo g. map f . map g = map (f . g)

¿para todo f. para todo g. map f . map g = map (f . g)?
=                                               (.)
¿para todo f. para todo g. map f map g = map (f . g)?

Caso base xs = []:

  map f (map g [])
= map f []              -- (map.[])
= []                    -- (map.[])

= map (f . g) []        -- (map.[])
[]

Caso inductivo xs = z:zs
HI) map f (map g zs) = map (f . g) zs
TI) map f (map g (z:zs)) = map (f . g) (z:zs)

  map f (map g (z:zs))
= map f (g z : map g zs)         -- (map.:)  [la de adentro]
= f (g z) : map f (map g zs)     -- (map.:)  [la de afuera]
= f (g z) : map (f . g) zs       -- HI
= (f . g) z : map (f . g) zs     -- (.)
= map (f . g) (z:zs)             -- (map.Inverso)

(Hecho asi nomas)

-- c. para todo f. para todo xs. para todo ys.
-- map f (xs ++ ys) = map f xs ++ map f ys

Lo que use
[]     ++ ys = ys                 -- (++.[])
(x:xs) ++ ys = x : (xs ++ ys)     -- (++.:)
map f []     = []                 -- (map.[])
map f (x:xs) = f x : map f xs     -- (map.:)


Caso base xs = []:
TI) map f ([] ++ ys) = map f [] ++ map f ys
  map f ([] ++ ys)
= map f ys                 -- (++.[])

  map f [] ++ map f ys
= [] ++ map f ys           -- (map.[])
= map f ys                 -- (++.[])
Ambos lados dan map f ys. 
Caso inductivo xs = z:zs:
HI) map f (zs ++ ys) = map f zs ++ map f ys
TI) map f ((z:zs) ++ ys) = map f (z:zs) ++ map f ys
Lado izquierdo:
  map f ((z:zs) ++ ys)
= map f (z : (zs ++ ys))         -- (++.:)
= f z : map f (zs ++ ys)         -- (map.:)
= f z : (map f zs ++ map f ys)   -- HI
Lado derecho:
  map f (z:zs) ++ map f ys
= (f z : map f zs) ++ map f ys   -- (map.:)
= f z : (map f zs ++ map f ys)   -- (++.:)
Los dos lados llegan a f z : (map f zs ++ map f ys), así que la TI vale. 


-- d. para todo f. concat . map (map f) = map f . concat

Por extensionalidad, 
basta ver ∀ xss. (concat . map (map f)) xss = (map f . concat) xss. 
Deshaciendo las dos composiciones con (.), el objetivo es:
∀ xss. concat (map (map f) xss) = map f (concat xss)
Definiciones:
concat []       = []                  -- (concat.[])
concat (xs:xss) = xs ++ concat xss    -- (concat.:)
map f []        = []                  -- (map.[])
map f (x:xs)    = f x : map f xs      -- (map.:)

y uso como lema lo probado en (c): map f (xs ++ ys) = map f xs ++ map f ys — lo llamo (c).
Sea xss :: [[a]] y f :: a -> b fija. Inducción sobre la estructura de xss:
Caso base xss = []:
TI) concat (map (map f) []) = map f (concat [])
  concat (map (map f) [])
= concat []                  -- (map.[])
= []                         -- (concat.[])

  map f (concat [])
= map f []                   -- (concat.[])
= []                         -- (map.[])
Ambos lados dan []. 
Caso inductivo xss = z:zs (con z :: [a], zs :: [[a]]):
HI) concat (map (map f) zs) = map f (concat zs)
TI) concat (map (map f) (z:zs)) = map f (concat (z:zs))
Lado izquierdo:
  concat (map (map f) (z:zs))
= concat (map f z : map (map f) zs)     -- (map.:)
= map f z ++ concat (map (map f) zs)    -- (concat.:)
= map f z ++ map f (concat zs)          -- HI
Lado derecho:
  map f (concat (z:zs))
= map f (z ++ concat zs)                -- (concat.:)
= map f z ++ map f (concat zs)          -- (c)
Los dos lados llegan a map f z ++ map f (concat zs), así que la TI vale. ∎


-- e. foldr ((+) . suma') 0 = sum . map suma'

Por extensionalidad, basta ver ∀ xs. foldr ((+) . suma') 0 xs = (sum . map suma') xs.
Deshaciendo la composición de la derecha con (.), el objetivo es:
∀ xs. foldr ((+) . suma') 0 xs = sum (map suma' xs)
Definiciones:
foldr f z []     = z                      -- (foldr.[])
foldr f z (x:xs) = f x (foldr f z xs)     -- (foldr.:)
map f []     = []                         -- (map.[])
map f (x:xs) = f x : map f xs             -- (map.:)
sum []     = 0                            -- (sum.[])
sum (x:xs) = x + sum xs                   -- (sum.:)
(f . g) x  = f (g x)                      -- (.)
Conviene tener a mano cómo se reduce el operador: ((+) . suma') x = (+) (suma' x), 
así que ((+) . suma') x y = suma' x + y.
Inducción sobre la estructura de xs:
Caso base xs = []:
TI) foldr ((+) . suma') 0 [] = sum (map suma' [])
  foldr ((+) . suma') 0 []
= 0                              -- (foldr.[])

  sum (map suma' [])
= sum []                        -- (map.[])
= 0                             -- (sum.[])
Ambos lados dan 0. 
Caso inductivo xs = z:zs:
HI) foldr ((+) . suma') 0 zs = sum (map suma' zs)
TI) foldr ((+) . suma') 0 (z:zs) = sum (map suma' (z:zs))
Lado izquierdo:
  foldr ((+) . suma') 0 (z:zs)
= ((+) . suma') z (foldr ((+) . suma') 0 zs)   -- (foldr.:)
= suma' z + foldr ((+) . suma') 0 zs           -- (.)
= suma' z + sum (map suma' zs)                 -- HI
Lado derecho:
  sum (map suma' (z:zs))
= sum (suma' z : map suma' zs)                 -- (map.:)
= suma' z + sum (map suma' zs)                 -- (sum.:)
Los dos lados llegan a suma' z + sum (map suma' zs), así que la TI vale. ∎

-- f. para todo f. para todo z. foldr f z . foldr (:) [] = foldr f z

Por extensionalidad, basta ver ∀ xs. (foldr f z . foldr (:) []) xs = foldr f z xs. 
Deshaciendo la composición con (.), el objetivo es:
∀ xs. foldr f z (foldr (:) [] xs) = foldr f z xs
Definiciones:
foldr g e []     = e                      -- (foldr.[])
foldr g e (x:xs) = g x (foldr g e xs)     -- (foldr.:)
(Renombro el parámetro de foldr a g e para no chocar con la f z fijas del enunciado.)
Sean f y z fijas. Inducción sobre la estructura de xs:
Caso base xs = []:
TI) foldr f z (foldr (:) [] []) = foldr f z []
  foldr f z (foldr (:) [] [])
= foldr f z []                  -- (foldr.[]), la de adentro
= z                             -- (foldr.[])

  foldr f z []
= z                             -- (foldr.[])
Ambos lados dan z. 
Caso inductivo xs = w:ws:
HI) foldr f z (foldr (:) [] ws) = foldr f z ws
TI) foldr f z (foldr (:) [] (w:ws)) = foldr f z (w:ws)
Lado izquierdo:
  foldr f z (foldr (:) [] (w:ws))
= foldr f z (w : foldr (:) [] ws)        -- (foldr.:), la de adentro
= f w (foldr f z (foldr (:) [] ws))      -- (foldr.:), la de afuera
= f w (foldr f z ws)                     -- HI
Lado derecho:
  foldr f z (w:ws)
= f w (foldr f z ws)                     -- (foldr.:)
Los dos lados llegan a f w (foldr f z ws), así que la TI vale. ∎

-- g. para todo f. para todo z. para todo xs. para todo ys.
-- foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs


-- h. (+1) . foldr (+) 0 = foldr (+) 1 ???

-- i. para todo n. para todo f. many n f = foldr (.) id (replicate n f)
-- siendo many 0 f = id
-- many n f = f . many (n - 1) f

-- j. para todo f. para todo xs. para todo ys.
-- zipWith (flip f) xs ys
-- = map (uncurry f) (flip zip xs ys)

Definiciones:
many 0 f = id                        -- (many.0)
many n f = f . many (n-1) f          -- (many.n),  n > 0
replicate 0 x = []                   -- (rep.0)
replicate n x = x : replicate (n-1) x -- (rep.n),  n > 0
foldr g e []     = e                 -- (foldr.[])
foldr g e (x:xs) = g x (foldr g e xs)-- (foldr.:)
Objetivo: ∀ n. many n f = foldr (.) id (replicate n f). Inducción sobre n.
Caso base n = 0:
TI) many 0 f = foldr (.) id (replicate 0 f)
  foldr (.) id (replicate 0 f)
= foldr (.) id []                  -- (rep.0)
= id                               -- (foldr.[])

  many 0 f
= id                               -- (many.0)
Ambos lados dan id. 
Caso inductivo n = k+1 (con k ≥ 0):
HI) many k f = foldr (.) id (replicate k f)
TI) many (k+1) f = foldr (.) id (replicate (k+1) f)
Lado derecho:
  foldr (.) id (replicate (k+1) f)
= foldr (.) id (f : replicate k f)          -- (rep.n), k+1 > 0
= (.) f (foldr (.) id (replicate k f))      -- (foldr.:)
= f . foldr (.) id (replicate k f)          -- notación infija de (.)
= f . many k f                              -- HI
Lado izquierdo:
  many (k+1) f
= f . many ((k+1)-1) f                       -- (many.n), k+1 > 0
= f . many k f                               -- aritmética
Los dos lados llegan a f . many k f, así que la TI vale. 

-- k. para todo f. para todo g. para todo h. para todo z.
-- si para todo x. para todo y. h (f x y) = g x (h y)
-- entonces h . foldr f z = foldr g (h z)
-- Dar un ejemplo de uso específico de esta propiedad. ????


-}
-- Ejercicio 9) Definir las siguientes funciones utilizando solamente foldr en conjunto
-- con algunos combinadores (como por ejemplo flip). Tener en cuenta utilizar la
-- cantidad mínima necesaria de argumentos en las definiciones.

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

-- flip :: (c -> d) -> (c -> (a -> d))
-- flip f x y = (f y) x

-- 2 [1,6,3,1] -> 3

{-
-- Ejercicio 10) Indicar cuáles de las siguientes expresiones tienen tipo, y para aquellas
-- que lo tengan, decir cuál es ese tipo:
-- a. filter id
filter :: (a -> Bool) -> [a] -> [a]
id :: a -> a
---------------------------------
[Bool] -> [Bool]

-- b. 
map (\x y z -> (x, y, z))
map (a-> b -> c -> (a,b,c))

map                   :: (a -> b) -> [a] -> [b]
(\x y z -> (x, y, z)) :: a -> b -> c -> (a, b, c)
-------------------------------------------------
[a] -> [b -> c -> (a, b, c)]

-- c. map (+)
(a -> b) -> [a] -> [b]
a -> a -> a
------------------------
[a] -> [a -> a]

-- d. filter fst

filter :: (a -> Bool) -> [a] -> [a]
fst :: (c,b) -> c
--------------------------------
[(Bool, b) ] -> [(Bool, b) ]

(a -> Bool)
(c,b) -> c
(Bool, b) -> Bool

-- e. filter ((flip const) (+))

flip :: (a -> b -> c) -> b -> a -> c
const :: a -> b -> a
------------------------------------
        b -> a -> c
(+)     a -> a -> a
--------------------------------
           a -> a
filter :: (a -> Bool) -> [a] -> [a]
-------------------------------------
[Bool] -> [Bool]

-- f. map const

map :: (a -> b) -> [a] -> [b]
const :: c -> d -> c
[c] -> [d -> c]

-- g. map twice

map :: (a -> b) -> [a] -> [b]
twice :: (a -> a) -> a -> a
----------------------------
[a -> a] -> [a -> a]

 h. foldr twice

foldr :: (a -> b -> b) -> b -> [a] -> b
twice :: (c -> c) -> c -> c
----------------------------------------
c -> [(c -> c)] -> c


-- i. zipWith fst
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
fst :: (a, b) -> a
-------------------------------------------------
[(b -> c, e)] -> [b] -> [c]

-- j. foldr (\x r z -> (x, z) : r z) (const [])


(a -> b -> b) -> b -> [a] -> b

(a -> (b -> [(x, z)]) -> b -> [(x, z)]) 
-> ( b -> [(x, z)]) 
-> [a]
-> ( b -> [(x, z)])


-}

twice :: (a -> a) -> (a -> a)
twice f x = f (f x)