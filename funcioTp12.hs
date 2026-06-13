--Ejercicio 1) Dada la definición de ExpA:
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

--a. Dar el tipo y definir foldExpA, que expresa el esquema de recursión
--estructural para la estructura ExpA.

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n) = c n
foldExpA c s p (Suma e1 e2) = s (foldExpA c s p e1)
                                (foldExpA c s p e2)

foldExpA c s p (Prod e1 e2) = p (foldExpA c s p e1)
                                (foldExpA c s p e2)

--b. Resolver las siguientes funciones utilizando foldExpA:
--i. que describe la cantidad de ceros explícitos en la expresión dada. 

--foldExpA :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> ExpA -> Int
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA (\n -> unoSi (n==0)) (\n1 n2 -> n1 + n2) (\n1 n2 -> n1 + n2) 

--ii. describe si la expresión dada no tiene números negativos de manera explícita.
--foldExpA :: (Int -> Bool) -> (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> ExpA -> Bool
noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

--iii. describe una expresión con el mismo significado que la dada, pero que no tiene
--sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución
--debe ser exclusivamente simbólica.
simplificarExpA :: ExpA -> ExpA
simplificarExpA = foldExpA Cte simplificarSuma simplificarProd

simplificarSuma :: ExpA -> ExpA -> ExpA 
simplificarSuma (Cte 0) e = e
simplificarSuma e (Cte 0) = e
simplificarSuma e1 e2 = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA 
simplificarProd (Cte 1) e = e
simplificarProd (Cte 0) e = Cte 0
simplificarProd e (Cte 0) = Cte 0
simplificarProd e (Cte 1) = e
simplificarProd e1 e2 = Prod e1 e2

--iv. que describe el número que resulta de evaluar la cuenta representada por
--la expresión aritmética dada.
evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*) 

--v. que describe el string sin espacios y con paréntesis 
--correspondiente a la expresión dada.
showExpA :: ExpA -> String
showExpA = foldExpA show (mostrarOp "+") (mostrarOp "*")

mostrarOp :: String -> String -> String -> String
mostrarOp op e1 e2 = "(" ++ e1 ++ op ++ e2 ++ ")"
--c. Demostrar que evalExpA' es equivalente a evalExpA (ejercicio 6.a.i de la práctica 8) – o sea evalExpA' = evalExpA.
{-
evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

¿evalExpA' = evalExpA?

Por ppio de ext. ¿para todo ex. evalExpA' ex = evalExpA ex?
Sean e :: ExpA, por ppio de ind. sobre la estructura de e, se verá que:


Caso base, e = (Cte n))
    ¿evalExpA' (Cte n) = evalExpA (Cte n)?
CI1. e = (Suma n1 n2))
    HI1) ¡evalExpA' n1 = evalExpA n1!
    HI2) ¡evalExpA' n2 = evalExpA n2!
    TI)  ¿evalExpA' (Suma n1 n2) = evalExpA (Suma n1 n2)?
CI2. e = (Prod n1 n2))
    HI1) ¡evalExpA' n1 = evalExpA n1!
    HI2) ¡evalExpA' n2 = evalExpA n2!
    TI)  ¿evalExpA' (Prod n1 n2) = evalExpA (Prod n1 n2) ?

Caso Base
IZQ:
evalExpA' (Cte n)
=                   (evalExpA')      
foldExpA id (+) (*) (Cte n)
=                   (foldExpA.1)
id n
=                   (id)
n

Der:
evalExpA (Cte n)
=                 evalExpA.1
n

Vale para este Caso

CI1
evalExpA' (Suma n1 n2)
=                   (evalExpA')
foldExpA id (+) (*) (Suma n1 n2)
=                   (foldExpA.2)
(+) (foldExpA id (+) (*) n1) (foldExpA id (+) (*) n2)
=                                          (def de (+))
foldExpA id (+) (*) n1 + foldExpA id (+) (*) n2

evalExpA (Suma n1 n2)
=                       (evalExpA)
evalExpA n1 + evalExpA n2
=                        (HI)
evalExpA' n1 + evalExpA' n2
=
foldExpA id (+) (*) n1 + foldExpA id (+) (*) n2

Vale para este Caso

CI2

evalExpA' (Prod n1 n2)
=                   (evalExpA')
foldExpA id (+) (*) (Prod n1 n2)
=                   (foldExpA.2)
(*) (foldExpA id (+) (*) n1) (foldExpA id (+) (*) n2)
=                                          (def de (*))
foldExpA id (+) (*) n1 * foldExpA id (+) (*) n2


evalExpA (Prod n1 n2)
=                       (evalExpA)
evalExpA n1 * evalExpA n2
=                        (HI)
evalExpA' n1 * evalExpA' n2
=
foldExpA id (+) (*) n1 * foldExpA id (+) (*) n2

Vale para este Caso

-}
--d. Dar el tipo y definir recExpA, que expresa el esquema de recursión primitiva para la estructura ExpA.

recExpA :: (Int -> b) -> (b -> ExpA -> b -> ExpA -> b) ->  (b -> ExpA -> b -> ExpA -> b) -> ExpA -> b
recExpA c s p (Cte n) = c n
recExpA c s p (Suma e1 e2) = s (recExpA c s p e1) e1 (recExpA c s p e2) e2
recExpA c s p (Prod e1 e2) = p (recExpA c s p e1) e1 (recExpA c s p e2) e2

--e. Resolver las siguientes funciones utilizando el esquema de recursión estructural o 
--primitiva sobre ExpA según corresponda:


--i. que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero.
--recExpA :: (Int -> Int) -> (Int -> ExpA -> Int -> ExpA -> Int) -> (Int -> ExpA -> Int -> ExpA -> Int) -> ExpA -> Int
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) (\n1 e1 n2 e2 -> unoSi (esCteCero e1 || esCteCero e2) + n1 + n2) (\n1 _ n2 _ -> n1 + n2)

esCteCero :: ExpA -> Bool
esCteCero (Cte n) = 0 == n

--ii.que describe la cantidad de constructores de producto con al menos uno de sus hijos constante uno.

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (const 0) (\n1 _ n2 _ -> n1 + n2)
                                    (\n1 e1 n2 e2 -> unoSi (esCteUno e1 || esCteUno e2) + n1 + n2) 

esCteUno :: ExpA -> Bool
esCteUno (Cte n) = 1 == n

--Ejercicio 2) Dada la definición de EA:
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul
                    deriving Show
--a. Dar el tipo y definir foldEA, que expresa el esquema de recursión estructural
--para la estructura EA.

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA c b (Const n) = c n
foldEA c b (BOp bi e1 e2) = b bi (foldEA c b e1) (foldEA c b e2)

--b. Resolver las siguientes funciones utilizando el esquema de recursión
--estructural o primitiva sobre EA según corresponda:

--i. describe si la expresión dada no tiene números negativos de manera explícita.
--foldEA :: (Int -> Bool) -> (BinOp -> Bool -> Bool -> Bool) -> EA -> Bool
noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>=0) (const (&&))

--ii. que describe una expresión con
--el mismo significado que la dada, pero que no tiene sumas del
--número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser
--exclusivamente simbólica.
simplificarEA' :: EA -> EA
simplificarEA' = foldEA  Const (\b e1 e2 -> case b of
                                            Mul -> simplificarMul e1 e2
                                            Sum -> simplificarSum e1 e2)

simplificarSum :: EA -> EA -> EA 
simplificarSum (Const 0) e = e
simplificarSum e (Const 0) = e
simplificarSum e1 e2 = BOp Sum e1 e2

simplificarMul :: EA -> EA -> EA 
simplificarMul (Const 1) e = e
simplificarMul (Const 0) e = Const 0
simplificarMul e (Const 0) = Const 0
simplificarMul e (Const 1) = e
simplificarMul e1 e2 = BOp Mul e1 e2


--iii. que describe el número que resulta de
--evaluar la cuenta representada por la expresión aritmética dada.
evalEA' :: EA -> Int 
evalEA'= foldEA id (\b -> case b of
                                Sum -> (+)
                                Mul -> (*))


--iv. que describe el string sin espacios y
--con paréntesis correspondiente a la expresión dada.
showEA :: EA -> String
showEA = foldEA show (mostrarOp . show)

--v.que describe una expresión aritmética representada con el tipo ExpA, 
--cuyo significado es el mismo que la dada.

--foldEA :: (Int -> ExpA) -> (BinOp -> ExpA -> ExpA -> ExpA) -> EA -> ExpA
ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte (\b -> case b of
                                    Mul -> Prod
                                    Sum -> Suma)


--vi.que describe la representación como elemento del tipo 
--ABTree BinOp Int de la expresión aritmética dada.
data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b)

--foldEA :: (Int -> ABTree BinOp Int ) -> (BinOp -> ABTree BinOp Int  -> ABTree BinOp Int  -> ABTree BinOp Int ) -> EA -> ABTree BinOp Int 
ea2Arbol' :: EA -> ABTree BinOp Int 
ea2Arbol' = foldEA Leaf Node 



--c. Demostrar que evalEA' es equivalente a evalEA (ejercicio 1.a.i de la práctica 9) – o sea que evalEA' = evalEA. 
--NO HECHO


--Ejercicio 3) Dada la definición de Tree:
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

--a. Dar el tipo y definir la función foldT, que expresa el esquema
-- de recursión estructural para la estructura Tree.

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT e _ EmptyT = e
foldT e n (NodeT x t1 t2) = n x (foldT e n t1)
                              (foldT e n t2)
--b. Definir las siguientes funciones utilizando foldT:
--i. 

--foldT :: Tree b -> (a -> Tree b -> Tree b -> Tree b) -> Tree a -> Tree b
mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)
--                (\x -> (Node . f) x)
--                (\x -> Node (f x))


--foldT :: Int -> (a -> Int -> Int -> Int) -> Tree a -> Int
sumT :: Tree Int -> Int
sumT = foldT 0 (\x n1 n2 -> x + n1 + n2)

heightT :: Tree a -> Int
heightT = foldT 0 (\_ t1 t2-> 1 + max t1 t2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x t1 t2 -> x : t1 ++ t2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x t1 t2 -> t1 ++ [x] ++ t2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x t1 t2 -> t1 ++ t2 ++ [x])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\x t1 t2 -> NodeT x t2 t1)


--foldT :: ((a -> Bool) -> Int)
--        -> (a -> ((a -> Bool) -> Int) -> ((a -> Bool) -> Int) -> (a -> Bool) -> Int)
--        -> Tree a
--        -> (a -> Bool) -> Int

--countByT :: Tree a  -> ((a -> Bool) -> Int)
countByT :: (a -> Bool) -> Tree a -> Int
countByT = flip (foldT (const 0) (\x t1 t2 f -> unoSi (f x) + t1 f + t2 f))


--foldT :: ((a -> Bool) -> ([a], [a])) 
--      -> (a -> ((a -> Bool) -> ([a], [a])) -> ((a -> Bool) -> ([a], [a])) -> (a -> Bool) -> ([a], [a]))
--      -> Tree a 
--      -> ((a -> Bool) -> ([a], [a]))

--partitionT :: Tree a -> ((a -> Bool) -> ([a], [a]))
partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT = flip (foldT (const ([],[])) (\x t1 t2 f -> unirPartitionSi (f x) x (t1 f) (t2 f)))

unirPartitionSi :: Bool -> a -> ([a], [a]) -> ([a], [a]) -> ([a], [a])
unirPartitionSi True x (y1,y2) (z1,z2) = (x:y1++z1,y2++z2)
unirPartitionSi False x (y1,y2) (z1,z2) = (y1++z1,x:y2++z2)


--foldT :: (Tree b -> Tree c) 
--          -> (a -> (Tree b -> Tree c) -> (Tree b -> Tree c) -> Tree b -> Tree c) 
--          -> Tree a 
--          -> (Tree b -> Tree c)
--zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
---zipWithT f = foldT (const EmptyT) (\x tbf1 tbf2 tb -> case tb of
--                                    EmptyT -> EmptyT
--                                    (NodeT y t3 t4) -> NodeT (f x y) (tbf1 t3) (tbf2 t4))

--Sin F
--foldT :: (Tree b -> Tree c) 
--          -> (a -> (Tree b -> Tree c) -> (Tree b -> Tree c) -> Tree b -> Tree c) 
--          -> Tree a 
--          -> (Tree b -> Tree c)

--foldT :: ((a->b->c) -> Tree b -> Tree c) 
--         -> (a -> ((a->b->c) -> Tree b -> Tree c) -> ((a->b->c) -> Tree b -> Tree c) -> (a->b->c) -> Tree b -> Tree c) 
--         -> Tree a 
--         -> ((a->b->c) -> Tree b -> Tree c)
--zipWithT :: Tree a -> (a->b->c) -> Tree b -> Tree c
zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT = flip (foldT (const (const EmptyT))  
    (\x ft1 ft2 fabc tb -> case tb of
                        EmptyT -> EmptyT
                        (NodeT y t1 t2) -> let c = fabc x y in NodeT c (ft1 fabc t1) (ft2 fabc t2)))


--foldT :: [a] -> (a -> [a] -> [a] -> [a]) -> Tree a -> [a]
caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\x xs1 xs2 -> if length xs1 > length xs2 then x : xs1 else x : xs2 )

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\x xss1 xss2 -> agregarCamino xss1 x ++ agregarCamino xss2 x) 

agregarCamino :: [[a]] -> a -> [[a]]
agregarCamino [] _ = []
agregarCamino (xs:xss) y = (y : xs) : agregarCamino xss y

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] (\x xss1 xss2 -> [x] : juntarNiveles xss1 xss2 )

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xss [] = xss
juntarNiveles [] xss = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

--foldT :: (Int -> [a]) 
--         -> (a -> (Int -> [a]) -> (Int -> [a]) -> Int -> [a])
--         -> Tree a 
--         -> (Int -> [a])
nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x fxs1 fxs2 n -> if n == 0 then [x] else fxs1 (n-1) ++ fxs2 (n-1))

--c. Dar el tipo y definir la función recT, que expresa el esquema de recursión
--primitiva para la estructura Tree.

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT z f EmptyT = z
recT z f (NodeT x t1 t2) = f x t1 t2 (recT z f t1) (recT z f t2) 

--d. Definir las siguientes funciones utilizando recT:

--i.que describe el árbol resultante de insertar el elemento dado 
--en el árbol dado, teniendo en cuenta invariantes de BST.
insertT :: Ord a => a -> Tree a -> Tree a
insertT x = recT (NodeT x EmptyT EmptyT) (\y t1 t2 tr1 tr2 -> if x == y then NodeT y t1 t2 else
                                                           if x < y then NodeT y tr1 t2 
                                                           else NodeT y t1 tr2)

--ii. que describe el camino hasta el elemento
-- dado en el árbol dado.
--Precondición: existe el elemento en el árbol.

caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta y t = case caminoHastaM t y of  Just xs -> xs
                                            Nothing -> error "Error"

--foldT :: (a -> Maybe [a]) -> (a -> (a -> Maybe [a]) -> (a -> Maybe [a]) -> a -> Maybe [a]) 
--        -> Tree a -> a -> Maybe [a]
caminoHastaM :: Eq a => Tree a -> a -> Maybe [a]
caminoHastaM = foldT (const Nothing) (\x f g y -> if y == x then Just [x] else 
                                                case f y of 
                                                Just xs -> Just (y : xs)
                                                Nothing -> case g y of  
                                                           Just xs' -> Just (y : xs')
                                                           Nothing -> Nothing)

--e. Demostrar las siguientes propiedades utilizando las definiciones anteriores:

--i. para todo f. sizeT . mapT f = sizeT
{-
¿para todo f. sizeT . mapT f = sizeT?
 
Por ppio de ext t. ¿para todo t. para todo f. (sizeT . mapT f) t = sizeT t?
                =                                                       (.)
                para todo t. para todo f. sizeT (mapT f t) = sizeT t
Sean  t :: Tree a, por ppio de ind. sobre la estructura de t, se verá que:


Caso base, t = (EmptyT))
    ¿sizeT (mapT f EmptyT)= sizeT EmptyT?
CI1. t = (NodeT x t1 t2)
    HI1) ¡sizeT (mapT f t1) = sizeT t1!
    HI2) ¡sizeT (mapT f t2) = sizeT t2!
    TI)  ¿sizeT (mapT f (NodeT x t1 t2)) = sizeT (NodeT x t1 t2)?

Caso Base
IZQ:
sizeT (mapT f EmptyT)
=                       (map)
sizeT (foldT EmptyT (NodeT . f) EmptyT)
=                        (foldT.1)
sizeT EmptyT

Vale para este Caso

CI1. t = 
sizeT (mapT f (NodeT x t1 t2)) 
=                       (mapT)
sizeT (foldT EmptyT (NodeT . f) (NodeT x t1 t2))
=                            (foldT)   
sizeT ((NodeT . f) x (foldT EmptyT (NodeT . f) t1) (foldT EmptyT (NodeT . f) t2))
=                                                        (def. (.))
sizeT (NodeT (f x) (foldT EmptyT (NodeT . f) t1) (foldT EmptyT (NodeT . f) t2))
=                                                        (sizeT)   -- despliego a foldT
foldT 0 g (NodeT (f x) (foldT EmptyT (NodeT . f) t1) (foldT EmptyT (NodeT . f) t2))
=                                                        (foldT.2)
g (f x) (foldT 0 g (foldT EmptyT (NodeT . f) t1)) (foldT 0 g (foldT EmptyT (NodeT . f) t2))
=                                                        (aplico g / beta)
1 + (foldT 0 g (foldT EmptyT (NodeT . f) t1)) + (foldT 0 g (foldT EmptyT (NodeT . f) t2))
=                                                        (sizeT, al revés)
1 + sizeT (foldT EmptyT (NodeT . f) t1) + sizeT (foldT EmptyT (NodeT . f) t2)
=                                                        (mapT, al revés)
1 + sizeT (mapT f t1) + sizeT (mapT f t2)
=                                                        (HI1)
1 + sizeT t1 + sizeT (mapT f t2)
=                                                        (HI2)
1 + sizeT t1 + sizeT t2


Der
sizeT (NodeT x t1 t2)
=                       (sizeT)
foldT 0 g (NodeT x t1 t2)
=                       (foldT.2)
g x (foldT 0 g t1) (foldT 0 g t2)
=                       (aplico g / beta)
1 + foldT 0 g t1 + foldT 0 g t2
=                       (sizeT, al revés)
1 + sizeT t1 + sizeT t2
-}
--ii. para todo f. para todo g. mapT f . mapT g = mapT (f . g)

{-
¿para todo f. para todo g. mapT f . mapT g = mapT (f . g)?

Por ppio de ext t. ¿para todo t. (mapT f . mapT g) t = mapT (f . g) t?
                =                                                       (.)
                ¿para todo t. mapT f (mapT g t) = mapT (f . g) t?
Sean t :: Tree a, por ppio de ind. sobre la estructura de t:

Caso base, t = EmptyT
    ¿mapT f (mapT g EmptyT) = mapT (f . g) EmptyT?
CI. t = NodeT x t1 t2
    HI1) ¡mapT f (mapT g t1) = mapT (f . g) t1!
    HI2) ¡mapT f (mapT g t2) = mapT (f . g) t2!
    TI)  ¿mapT f (mapT g (NodeT x t1 t2)) = mapT (f . g) (NodeT x t1 t2)?


Caso Base
IZQ:
mapT f (mapT g EmptyT)
=                       (mapT)        -- el de adentro
mapT f (foldT EmptyT (NodeT . g) EmptyT)
=                       (foldT.1)
mapT f EmptyT
=                       (mapT)
foldT EmptyT (NodeT . f) EmptyT
=                       (foldT.1)
EmptyT

DER:
mapT (f . g) EmptyT
=                       (mapT)
foldT EmptyT (NodeT . (f . g)) EmptyT
=                       (foldT.1)
EmptyT

Ambos lados dan EmptyT. Vale para este caso.


CI.
IZQ:
mapT f (mapT g (NodeT x t1 t2))
=                       (mapT)        -- el de adentro
mapT f (foldT EmptyT (NodeT . g) (NodeT x t1 t2))
=                       (foldT.2)
mapT f ((NodeT . g) x (foldT EmptyT (NodeT . g) t1) (foldT EmptyT (NodeT . g) t2))
=                       (def. (.))
mapT f (NodeT (g x) (foldT EmptyT (NodeT . g) t1) (foldT EmptyT (NodeT . g) t2))
=                       (mapT, al revés)   -- reconozco los folds como mapT g
mapT f (NodeT (g x) (mapT g t1) (mapT g t2))
=                       (mapT)        -- el de afuera
foldT EmptyT (NodeT . f) (NodeT (g x) (mapT g t1) (mapT g t2))
=                       (foldT.2)
(NodeT . f) (g x) (foldT EmptyT (NodeT . f) (mapT g t1)) (foldT EmptyT (NodeT . f) (mapT g t2))
=                       (def. (.))
NodeT (f (g x)) (foldT EmptyT (NodeT . f) (mapT g t1)) (foldT EmptyT (NodeT . f) (mapT g t2))
=                       (mapT, al revés)
NodeT (f (g x)) (mapT f (mapT g t1)) (mapT f (mapT g t2))
=                       (HI1)
NodeT (f (g x)) (mapT (f . g) t1) (mapT f (mapT g t2))
=                       (HI2)
NodeT (f (g x)) (mapT (f . g) t1) (mapT (f . g) t2)

DER:
mapT (f . g) (NodeT x t1 t2)
=                       (mapT)
foldT EmptyT (NodeT . (f . g)) (NodeT x t1 t2)
=                       (foldT.2)
(NodeT . (f . g)) x (foldT EmptyT (NodeT . (f . g)) t1) (foldT EmptyT (NodeT . (f . g)) t2)
=                       (def. (.))
NodeT ((f . g) x) (foldT EmptyT (NodeT . (f . g)) t1) (foldT EmptyT (NodeT . (f . g)) t2)
=                       (mapT, al revés)
NodeT ((f . g) x) (mapT (f . g) t1) (mapT (f . g) t2)
=                       (def. (.))
NodeT (f (g x)) (mapT (f . g) t1) (mapT (f . g) t2)

Ambos lados dan  NodeT (f (g x)) (mapT (f . g) t1) (mapT (f . g) t2).  Vale TI. 
-}

--iii. foldT EmptyT NodeT = id
{-
¿foldT EmptyT NodeT = id?

Por ppio de ext t. ¿para todo t. foldT EmptyT NodeT t = id t?
                =                                            (id)
                ¿para todo t. foldT EmptyT NodeT t = t?
Sean t :: Tree a, por ppio de ind. sobre la estructura de t:

Caso base, t = EmptyT
    ¿foldT EmptyT NodeT EmptyT = EmptyT?
CI. t = NodeT x t1 t2
    HI1) ¡foldT EmptyT NodeT t1 = t1!
    HI2) ¡foldT EmptyT NodeT t2 = t2!
    TI)  ¿foldT EmptyT NodeT (NodeT x t1 t2) = NodeT x t1 t2?


Caso Base
foldT EmptyT NodeT EmptyT
=                       (foldT.1)
EmptyT

(y  id EmptyT = EmptyT  por def. de id).  Vale para este caso.


CI.
foldT EmptyT NodeT (NodeT x t1 t2)
=                       (foldT.2)
NodeT x (foldT EmptyT NodeT t1) (foldT EmptyT NodeT t2)
=                       (HI1)
NodeT x t1 (foldT EmptyT NodeT t2)
=                       (HI2)
NodeT x t1 t2

(y  id (NodeT x t1 t2) = NodeT x t1 t2  por def. de id).  Vale TI.  ∎
-}

--Ejercicio 4) Dadas las siguientes definiciones para representar mapas con diferentes
--puntos de interés que pueden presentar objetos:

data Dir = Izq | Der | Rect
data Mapa a = Cofre [a]
    | Bifurcacion [a] (Mapa a) (Mapa a)
    | Nada (Mapa a)

--a. Dar el tipo y definir foldM y recM, que expresan los esquemas de recursión
--estructural y primitiva, respectivamente, para la estructura Mapa.

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f g h (Cofre xs) = f xs
foldM f g h (Nada m) = g (foldM f g h m) 
foldM f g h (Bifurcacion xs m1 m2) = h xs (foldM f g h m1) (foldM f g h m2)  

recM :: ([a] -> b)
     -> (Mapa a -> b -> b)
     -> ([a] -> Mapa a -> Mapa a -> b -> b -> b)
     -> Mapa a -> b
recM f g h (Cofre xs)             = f xs
recM f g h (Nada m)               = g m (recM f g h m)
recM f g h (Bifurcacion xs m1 m2) = h xs m1 m2 (recM f g h m1) (recM f g h m2)

--b. Definir las siguientes funciones sin utilizar recursión explícita, utilizando el
--esquema de recursión estructural o primitiva sobre Mapa según corresponda::

--i.que describe la lista de todos los
--objetos presentes en el mapa dado.

--foldM :: ([a] -> [a]) -> ([a] -> [a]) -> ([a] -> [a] -> [a] -> [a]) -> Mapa a -> [a]
objects :: Mapa a -> [a]
objects = foldM id id (\ys ys1 ys2 -> ys ++ ys1 ++ ys2)

--ii. que transforma los objetos del mapa dado aplicando la función dada.
--foldM :: ([a] -> (a -> b) -> Mapa b )
--       -> (((a -> b) -> Mapa b) -> (a -> b) -> Mapa b)
--       -> ([a] -> ((a -> b) -> Mapa b) -> ((a -> b) -> Mapa b) -> (a -> b) -> Mapa b) 
--       -> Mapa a 
--       -> ((a -> b) -> Mapa b)
--mapM :: Mapa a -> (a -> b) -> Mapa b 
mapM :: (a -> b) -> Mapa a -> Mapa b 
mapM = flip (foldM (\xs g -> Cofre (map g xs)) 
                   (Nada .) -- <- (\g -> Nada . g) <- (\g f -> Nada (g f)) 
                   (\xs f g h -> Bifurcacion (map h xs) (f h) (g h)))

--iii. que indica si existe
--algún objeto que cumpla con la condición dada en el mapa dado.

--foldM :: ([a] -> (a -> Bool) -> Bool) 
--          -> (((a -> Bool) -> Bool) -> (a -> Bool) -> Bool) 
--          -> ([a] -> ((a -> Bool) -> Bool) -> ((a -> Bool) -> Bool) -> (a -> Bool) -> Bool)
--          -> Mapa a
--          -> ((a -> Bool) -> Bool)

--has :: Mapa a -> (a -> Bool) -> Bool 
has :: (a -> Bool) -> Mapa a -> Bool
has = flip (foldM (\xs f -> any f xs ) (\f g -> f g) (\xs f g h -> any h xs || g h || f h))

--iv. que indica si un objeto al final del camino dado cumple con la
--condición dada en el mapa dado.


--foldM :: ([a] -> (a->Bool) -> [Dir] -> Bool) 
--       -> (((a->Bool) -> [Dir] -> Bool) -> (a->Bool) -> [Dir] -> Bool) 
--       -> ([a] -> ((a->Bool) -> [Dir] -> Bool) -> ((a->Bool) -> [Dir] -> Bool) -> (a->Bool) -> [Dir] -> Bool) 
--       -> Mapa a 
--       -> ((a->Bool) -> [Dir] -> Bool)
-- hasObjectAt ::  Mapa a -> ((a->Bool) -> [Dir] -> Bool)
hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt = flip (foldM (\xs f ds -> if null ds then any f xs else error "")
                          (\f g ds -> if null ds then error "" else 
                            case head ds of
                            Rect -> f g (tail ds)
                            _ -> error "")
                          (\xs f g h ds -> if null ds then error "" else 
                            case head ds of
                            Izq -> f h (tail ds)
                            Der -> g h (tail ds)
                            _ -> error ""))

--v. que describe el camino más
--largo en el mapa dado.

--foldM :: ([a] -> [Dir]) -> ([Dir] -> [Dir]) -> ([a] -> [Dir] -> [Dir] -> [Dir]) -> Mapa a -> [Dir]
longestPath :: Mapa a -> [Dir] 
longestPath = foldM (const []) (\ds -> Rect:ds) (\_ ds1 ds2 -> if length ds1 > length ds2 then Izq : ds1 else Der : ds2)

--vi.que describe la lista con los objetos presentes en el
-- camino más largo del mapa dado.
objectsOfLongestPath :: Mapa a -> [a] 
objectsOfLongestPath = foldM id id (\xs ds1 ds2 -> if length ds1 > length ds2 then xs ++ ds1 else xs ++ ds2)

--vii. que describe la lista con
--todos los caminos del mapa dado.

--foldM :: ([a] -> [[Dir]]) -> ([[Dir]] -> [[Dir]]) -> ([a] -> [[Dir]] -> [[Dir]] -> [[Dir]]) -> Mapa a -> [[Dir]]
allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (const []) (\dss -> agregarCamino' dss Rect) (\_ dss1 dss2 -> agregarCamino' dss1 Izq ++ agregarCamino' dss2 Der)

agregarCamino' :: [[Dir]] -> Dir -> [[Dir]]
agregarCamino' [] _ = []
agregarCamino' (xs:xss) y = (y : xs) : agregarCamino' xss y

--viii. que describe la lista
--con todos los objetos por niveles del mapa dado.
objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (\xs -> [xs]) id (\xs dss1 dss2 -> [xs] ++ juntarNiveles dss1 dss2)

--c. Demostrar la siguiente propiedad:
--para todo x. has (==x) = any (elem x) . objectsPerLevel

{-
¿para todo x. has (==x) = any (elem x) . objectsPerLevel?

Por ppio de ext m. ¿para todo m. para todo x. has (==x) m = (any (elem x) . objectsPerLevel) m?
                =                                                                          (.)
                ¿para todo m. para todo x. has (==x) m = any (elem x) (objectsPerLevel m)?

Fijo x arbitrario. Abrevio los argumentos de los folds:
  has            = flip (foldM cf cn cb)
     cf = \xs f -> any f xs
     cn = \f g -> f g
     cb = \xs f g h -> any h xs || g h || f h
  objectsPerLevel = foldM pf pn pb
     pf = \xs -> [xs]
     pn = id
     pb = \xs dss1 dss2 -> [xs] ++ juntarNiveles dss1 dss2

Observación (desplegar has): por def. de has y de flip,
  has (==x) m = flip (foldM cf cn cb) (==x) m = foldM cf cn cb m (==x)

Sea m :: Mapa a, por ppio de ind. sobre la estructura de m:


====================  Caso m = Cofre xs  ====================
IZQ:
has (==x) (Cofre xs)
=                    (def. has / flip)
foldM cf cn cb (Cofre xs) (==x)
=                    (foldM.1)
cf xs (==x)
=                    (def. cf, beta)
any (==x) xs
=                    (elem, al revés)
elem x xs

DER:
any (elem x) (objectsPerLevel (Cofre xs))
=                    (def. objectsPerLevel)
any (elem x) (foldM pf pn pb (Cofre xs))
=                    (foldM.1)
any (elem x) (pf xs)
=                    (def. pf)
any (elem x) [xs]
=                    (any.2)
elem x xs || any (elem x) []
=                    (any.1)
elem x xs || False
=
elem x xs

Ambos lados dan  elem x xs.  Vale este caso.


====================  Caso m = Nada m'  ====================
HI) has (==x) m' = any (elem x) (objectsPerLevel m')

IZQ:
has (==x) (Nada m')
=                    (def. has / flip)
foldM cf cn cb (Nada m') (==x)
=                    (foldM.2)
cn (foldM cf cn cb m') (==x)
=                    (def. cn, beta)   -- cn f g = f g
(foldM cf cn cb m') (==x)
=                    (def. has, al revés)
has (==x) m'
=                    (HI)
any (elem x) (objectsPerLevel m')

DER:
any (elem x) (objectsPerLevel (Nada m'))
=                    (def. objectsPerLevel)
any (elem x) (foldM pf pn pb (Nada m'))
=                    (foldM.2)
any (elem x) (pn (foldM pf pn pb m'))
=                    (def. pn = id)
any (elem x) (foldM pf pn pb m')
=                    (def. objectsPerLevel, al revés)
any (elem x) (objectsPerLevel m')

Ambos lados dan  any (elem x) (objectsPerLevel m').  Vale este caso.


====================  Caso m = Bifurcacion xs m1 m2  ====================
HI1) has (==x) m1 = any (elem x) (objectsPerLevel m1)
HI2) has (==x) m2 = any (elem x) (objectsPerLevel m2)

IZQ:
has (==x) (Bifurcacion xs m1 m2)
=                    (def. has / flip)
foldM cf cn cb (Bifurcacion xs m1 m2) (==x)
=                    (foldM.3)
cb xs (foldM cf cn cb m1) (foldM cf cn cb m2) (==x)
=                    (def. cb, beta)   -- cb ys f g h = any h ys || g h || f h
any (==x) xs || (foldM cf cn cb m2) (==x) || (foldM cf cn cb m1) (==x)
=                    (def. has, al revés)
any (==x) xs || has (==x) m2 || has (==x) m1
=                    (elem, al revés)
elem x xs || has (==x) m2 || has (==x) m1
=                    (HI2, HI1)
elem x xs || any (elem x) (objectsPerLevel m2) || any (elem x) (objectsPerLevel m1)

DER:
any (elem x) (objectsPerLevel (Bifurcacion xs m1 m2))
=                    (def. objectsPerLevel)
any (elem x) (foldM pf pn pb (Bifurcacion xs m1 m2))
=                    (foldM.3)
any (elem x) (pb xs (foldM pf pn pb m1) (foldM pf pn pb m2))
=                    (def. pb)
any (elem x) ([xs] ++ juntarNiveles (objectsPerLevel m1) (objectsPerLevel m2))
=                    (any.++)
any (elem x) [xs] || any (elem x) (juntarNiveles (objectsPerLevel m1) (objectsPerLevel m2))
=                    (any.2, any.1)
elem x xs || any (elem x) (juntarNiveles (objectsPerLevel m1) (objectsPerLevel m2))
=                    (LEMA L)
elem x xs || any (elem x) (objectsPerLevel m1) || any (elem x) (objectsPerLevel m2)

Comparo:
IZQ = elem x xs || any (elem x) (objectsPerLevel m2) || any (elem x) (objectsPerLevel m1)
DER = elem x xs || any (elem x) (objectsPerLevel m1) || any (elem x) (objectsPerLevel m2)

Coinciden por conmutatividad de ||.  Vale TI.   

LEMA L: para todo x. para todo A, B :: [[a]].
        any (elem x) (juntarNiveles A B) = any (elem x) A || any (elem x) B

Por ind. sobre la estructura de A:

Caso A = []
  any (elem x) (juntarNiveles [] B)
  =                      (jN.1)
  any (elem x) B
  =                      (any.1, al revés)  -- False || y = y
  any (elem x) [] || any (elem x) B                      

Caso A = (as:ass)
  HI) para todo B'. any (elem x) (juntarNiveles ass B') = any (elem x) ass || any (elem x) B'

  Subcaso B = []
    any (elem x) (juntarNiveles (as:ass) [])
    =                    (jN.2)
    any (elem x) (as:ass)
    =                    (any.1, al revés)
    any (elem x) (as:ass) || any (elem x) []             

  Subcaso B = (bs:bss)
    any (elem x) (juntarNiveles (as:ass) (bs:bss))
    =                    (jN.3)
    any (elem x) ((as++bs) : juntarNiveles ass bss)
    =                    (any.2)
    elem x (as++bs) || any (elem x) (juntarNiveles ass bss)
    =                    (elem x sobre ++)   -- elem x (as++bs) = elem x as || elem x bs
    (elem x as || elem x bs) || any (elem x) (juntarNiveles ass bss)
    =                    (HI con B' = bss)
    (elem x as || elem x bs) || (any (elem x) ass || any (elem x) bss)

    y el lado que quiero:
    any (elem x) (as:ass) || any (elem x) (bs:bss)
    =                    (any.2 dos veces)
    (elem x as || any (elem x) ass) || (elem x bs || any (elem x) bss)

    Ambos coinciden por asociatividad/conmutatividad de ||.   
-}
-- ========================================================================================
--ARBOL GENERAL NO HECHO
--Ejercicio 6) Dadas las siguientes definiciones para representar un sistema de archivos:

type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content
    | Folder Name [FileSystem]


--a. Definir foldFS y recFS, que expresan la recursión estructural y primitiva,
--respectivamente, para la estructura FileSystem.

--map :: (a -> b) -> [a] -> [b]
--map :: (FileSystem -> b) -> [FileSystem] -> [b]
--            foldFS f g      fs
foldFS

{-
--b. Definir las siguientes funciones sin utilizar recursión explícita, utilizando el
--esquema de recursión estructural o primitiva sobre FileSystem según
--corresponda:

--i. que describe la cantidad de archivos en el filesystem dado.
amountOfFiles :: FileSystem -> Int 

--ii. que describe el contenido del archivo con el nombre dado en el filesystem
--dado.
find :: Name -> FileSystem -> Maybe Content 

--iii. que describe la ruta
--desde la raiz hasta el nombre dado en el filesystem dado.
--Precondición: el nombre existe en el filesystem.
pathOf :: Name -> FileSystem -> Path 

--iv. que describe el filesystem resultante de transformar
--todos los archivos en el filesystem dado aplicando la función dada.
mapContents :: (Content -> Content) -> FileSystem -> FileSystem

--v. resultante de transformar el filesystem dado aplicando la función
asociada a cada archivo en la lista dada.
targetedMapContents :: [(Name, Content -> Content)] -> FileSystem -> FileSystem, que describe el filesystem

-}

-- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- 
--FUNCIONES AUXILIARES

--foldA :: (a -> b) -> (a -> b -> b -> b) -> Arbol a -> b

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0


-- (f . g) x = f (g x)
-- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- 