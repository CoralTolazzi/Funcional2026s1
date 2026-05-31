-- ===========================================================================
{-
Ejercicio 1) Dada la siguiente representación de expresiones aritméticas
-}
data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

{-
a. implementar las siguientes funciones por recursión estructural (o primitiva en
caso de ser necesario):
i. evalEA :: EA -> Int, que describe el número que resulta de
evaluar la cuenta representada por la expresión aritmética dada.
ii. ea2ExpA :: EA -> ExpA, que describe una expresión aritmética
representada con el tipo ExpA, cuya estructura y significado son los
mismos que la dada.
iii. expA2ea :: ExpA -> EA, que describe una expresión aritmética
representada con el tipo EA, cuya estructura y significado son los
mismos que la dada.
-}
data ExpA = Cte Int 
        |   Suma ExpA ExpA
        |   Prod ExpA ExpA

evalEA :: EA -> Int 
evalEA (Const n) = n
evalEA (BOp bop ea1 ea2) = evalBinOp bop (evalEA ea1) (evalEA ea2)

evalBinOp :: BinOp -> Int -> Int -> Int 
evalBinOp Sum = (+)
evalBinOp Mul = (*)

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp bop ea1 ea2) = binOp2expAOp bop (ea2ExpA ea1) (ea2ExpA ea2)

binOp2expAOp :: BinOp -> ExpA -> ExpA -> ExpA 
binOp2expAOp Sum = Suma
binOp2expAOp Mul = Prod

expA2ea :: ExpA -> EA 
expA2ea (Cte n) = Const n
expA2ea (Suma exp1 exp2) = BOp Sum (expA2ea exp1) (expA2ea exp2)
expA2ea (Prod exp1 exp2) = BOp Mul (expA2ea exp1) (expA2ea exp2)

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n)      = Cte n
simplificarExpA (Suma e1 e2) = simpSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = simpProd (simplificarExpA e1) (simplificarExpA e2)

simpSuma :: ExpA -> ExpA -> ExpA
simpSuma (Cte 0) e       = e
simpSuma e       (Cte 0) = e
simpSuma e       e'      = Suma e e'

simpProd :: ExpA -> ExpA -> ExpA
simpProd (Cte 0) _       = Cte 0
simpProd _       (Cte 0) = Cte 0
simpProd (Cte 1) e       = e
simpProd e       (Cte 1) = e
simpProd e       e'      = Prod e e'

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)      = 0
cantidadDeSumaCero (Suma e1 e2) =
    esSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) =
    cantidadDeSumaCero e1 + cantidadDeSumaCero e2

esSumaCero :: ExpA -> ExpA -> Int
esSumaCero (Cte 0) _       = 1
esSumaCero _       (Cte 0) = 1
esSumaCero _       _       = 0

{-
b. demostrar la siguiente propiedad:
===========================================================================
i. ea2ExpA . expA2ea = id

Por ppio. de ext.:
¿para todo e.
        ea2ExpA . expA2ea e = id e?
    =                               ((.))
¿para todo e.
        ea2ExpA (expA2ea e) = id e?

Sea n :: ExpA. Qvq: 
Por ppio. de ind. en la estructura de n es equivalente demostrar:


Caso base, n = (Cte n'))
    ¿ea2ExpA (expA2ea (Cte n')) = id (Cte n')?
CI1. n = Suma n1 n2)
    HI1) ¡ea2ExpA (expA2ea n1) = id n1!
    HI2) ¡ea2ExpA (expA2ea n2) = id n2!
    TI)  ¿ea2ExpA (expA2ea (Suma n1 n2)) = id (Suma n1 n2)?
CI2. n = Prod n1 n2)
    HI1) ¡ea2ExpA (expA2ea n1) = id n1!
    HI2) ¡ea2ExpA (expA2ea n2) = id n2!
    TI)  ¿ea2ExpA (expA2ea (Prod n1 n2)) = id (Prod n1 n2)?

CB1. n = Cte n')
    Lado izq. 
            ea2ExpA (expA2ea (Cte n'))
    =               ----------------- (expA2ea.1)
            ea2ExpA (Const n')
    =       -----------------         (ea2ExpA.1)
            Cte n
    Lado der.
            id (Cte n')
    =       -----------               (id, x <- Cte n')
            Cte n'

Vale la propiedad

Dem.:
    Lado izq.
         ea2ExpA (expA2ea (Suma n1 n2))
    =             --------------------  (expA2ea.2)
         ea2ExpA (BOp Sum (expA2ea n1) (expA2ea n2)) 
         -------------------------------------------
    =                                    (ea2ExpA.2)
        binOp2expAOp Sum (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
    =   --------------------------------------------------------------     
                                         (binOp2expAOp.1)
        Suma (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
    =        ----------------------      (HI1)
        Suma (id n1) (ea2ExpA (expA2ea n2))
    =                ----------------------
                                         (HI2)
        Suma (id n1) (id n2)
    =        ------  -------             (id)
        Suma n1 n


    Lado der.
         id (Suma n1 n2)  
    =    ---------------        (id, x <- Suma n1 n2)   
        Suma n1 n2

Vale la propiedad    

Dem.:
    Lado izq.
         ea2ExpA (expA2ea (Prod n1 n2))
    =             --------------------  (expA2ea.3)
         ea2ExpA (BOp Mul (expA2ea n1) (expA2ea n2)) 
         -------------------------------------------
    =                                    (ea2ExpA.2)
        binOp2expAOp Mul (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
    =   --------------------------------------------------------------     
                                         (binOp2expAOp.2)
        Prod (ea2ExpA (expA2ea n1)) (ea2ExpA (expA2ea n2))
    =        ----------------------      (HI1)
        Prod (id n1) (ea2ExpA (expA2ea n2))
    =                ----------------------
                                         (HI2)
        Prod (id n1) (id n2)
    =        ------  -------             (id)
        Prod n1 n

    Lado der.
         id (Prod n1 n2)  
    =    ---------------        (id, x <- Prod n1 n2)   
        Prod n1 n

Vale la propiedad para todos los casos

===========================================================================
ii. expA2ea . ea2ExpA = id

Por ppio. de ext.:
¿para todo e.
        expA2ea . ea2ExpA e = id e?
    =                               ((.))
¿para todo e.
        expA2ea (ea2ExpA e) = id e?

Sea n :: EA. Qvq: 
Por ppio. de ind. en la estructura de n es equivalente demostrar:

Caso base, n = (Const x))
    ¿expA2ea (ea2ExpA (Const x)) = id (Const x)?
CI1. n = (BOp b e1 e2))
    HI1) ¡expA2ea (ea2ExpA e1) = id e1!
    HI2) ¡expA2ea (ea2ExpA e2) = id e2!
    TI)  ¿expA2ea (ea2ExpA (BOp b e1 e2)) = id (BOp b e1 e2)?

Caso Base:

IZQ:
expA2ea (ea2ExpA (Const x))
=                       (ea2ExpA.1)
expA2ea (Cte x)
=                        (expA2ea.1)
Const x

DER:
id (Const x)
=           (id)
Const x

Vale para la propiedad 

CI
IZQ:
    expA2ea (ea2ExpA (BOp b e1 e2))
=            -----------------------    (ea2ExpA.2)
    expA2ea (binOp2expAOp b (ea2ExpA e1) (ea2ExpA e2))
=                                       (Lema1)
    BOp b (expA2ea (ea2ExpA e1)) (expA2ea (ea2ExpA e2))
=           ---------------------------------------------
                                        (HI.1, HI.2)
    BOp b (id e1) (id e2)
=           ------  ------- (id)
    BOp b e1 e2

DER:
id (BOp b e1 e2)
=                (id)
BOp b e1 e2


Lema1
¿para todo b.ex.es.expA2ea (binOp2expAOp b ex es) = BOp b (expA2ea ex) (expA2ea es)?
Sean bip::BinOp, e1::ExpA, e2::ExpA es equivalente demostrar:

Caso 1 (bip = Mul): ¿para todo b.e1.e2.expA2ea (binOp2expAOp Mul e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)?

IZQ:
expA2ea (binOp2expAOp Mul e1 e2) 
=                       (binOp2expAOp.2)
expA2ea (Prod e1 e2)
=                       (expA2ea.3) 
BOp Mul (expA2ea exp1) (expA2ea exp2)

Vale para este caso


Caso 2 (bip = Sum): ¿para todo b.e1.e2.expA2ea (binOp2expAOp Sum e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)?

expA2ea (binOp2expAOp Sum e1 e2)
=                             (binOp2expAOp.1)
expA2ea (Suma exp1 exp2)
=                            (expA2ea.2)
BOp Sum (expA2ea exp1) (expA2ea exp2)


Vale para este caso
===========================================================================
iii. evalExpA . ea2ExpA = evalEA

Por ppio. de ext.:
¿para todo e.
        evalExpA . ea2ExpA e= evalEA e?
    =                               ((.))
¿para todo e.
        evalExpA (ea2ExpA e) = evalEA e?

Sea n :: EA. Qvq: 
Por ppio. de ind. en la estructura de n es equivalente demostrar:

Caso base, n = (Const n1))
    ¿evalExpA (ea2ExpA (Const n1)) = evalEA (Const n1)?
CI1. n = (BOp b e1 e2))
    HI1) ¡evalExpA (ea2ExpA e1) = evalEA e1!
    HI2) ¡evalExpA (ea2ExpA e2) = evalEA e2!
    TI)  ¿evalExpA (ea2ExpA (BOp b e1 e2)) = evalEA (BOp b e1 e2)?

Caso Base
IZQ:
evalExpA (ea2ExpA (Const n1))
=                       (ea2ExpA.1)
evalExpA (Cte n)
=                   (evalExpA.1)
n

DER:
evalEA (Const n1)
=               (evalEA.1)
n

vale para este caso

CI1
Dem:
IZQ:
evalExpA (ea2ExpA (BOp b e1 e2))
=                       (ea2ExpA.2)
evalExpA (binOp2expAOp b (ea2ExpA e1) (ea2ExpA e2))
=                       (LEMA 2)
evalBinOp b (evalExpA (ea2ExpA e1)) (evalExpA (ea2ExpA e2))

DER:
evalEA (BOp b e1 e2) 
=                   (evalEA.2)
evalBinOp b (evalEA e1) (evalEA e2)
=                                    (HI.1, HI.2)
evalBinOp b (evalExpA (ea2ExpA e1)) (evalExpA (ea2ExpA e2))

Vale para estos casos
Vale para la propiedad
=========================================================================
LEMA 2: evalExpA (binOp2expAOp b e1 e2) = evalBinOp b (evalExpA e1) (evalExpA e2)

¿Para todo b, e1, e2. evalExpA (binOp2expAOp b e1 e2) = evalBinOp b (evalExpA e1) (evalExpA e2)?

Sean e::EA,e'::EA, ¿Para todo b, evalExpA (binOp2expAOp b e e') = evalBinOp b (evalExpA e) (evalExpA e')?

Caso b=Sum: ¿evalExpA (binOp2expAOp Sum e e') = evalBinOp Sum (evalExpA e) (evalExpA e')?
IZQ:
evalExpA (binOp2expAOp Sum e e')
=                           (binOp2expAOp.1)
evalExpA (Suma e e')
=                           (evalExpA.2)
evalExpA e + evalExpA e'

DER:
evalBinOp Sum (evalExpA e) (evalExpA e')
(+) (evalExpA (ea2ExpA e)) (evalExpA (ea2ExpA e'))
evalExpA e + evalExpA e'


Caso b=Mul: ¿evalExpA (binOp2expAOp Mul e e') = evalBinOp Mul (evalExpA e) (evalExpA e')?
IZQ
evalExpA (binOp2expAOp Mul e e')
evalExpA (Prod e e')
evalExpA e * evalExpA e'

DER:
evalBinOp Mul (evalExpA e) (evalExpA e')
(*) (evalExpA e) (evalExpA e')
evalExpA e * evalExpA e'

Vale para estos casos

=========================================================================
iv. evalEA . expA2ea = evalExpA

¿evalEA . expA2ea = evalExpA?
=
Por ppio de ext. ¿Para todo e. (evalEA . expA2ea) e = evalExpA e?
=                          (def. (.))
¿Para todo e. evalEA (expA2ea e) = evalExpA e?

Sea e'::ExpA, por ppio de ind. sobre la estructura de e', se verá que:

Caso base: e'=(Cte n))
    ¿evalEA (expA2ea (Cte n)) = evalExpA (Cte n)?
Caso ind. 1: e'=(Suma e1 e2)
    HI1) ¡evalEA (expA2ea e1) = evalExpA e1!
    HI2) ¡evalEA (expA2ea e2) = evalExpA e2!
    TI) ¿evalEA (expA2ea (Suma e1 e2)) = evalExpA (Suma e1 e2)?
Caso ind. 2: e'=(Prod e1 e2)
    HI1) ¡evalEA (expA2ea e1) = evalExpA e1!
    HI2) ¡evalEA (expA2ea e2) = evalExpA e2!
    TI) ¿evalEA (expA2ea (Prod e1 e2)) = evalExpA (Prod e1 e2)?

Caso base IZQ:
evalEA (expA2ea (Cte n))
evalEA (Const n)
n

Caso base DER:
evalExpA (Cte n)
n


Caso ind. 1 IZQ:
evalEA (expA2ea (Suma e1 e2))
evalEA (BOp Sum (expA2ea e1) (expA2ea e2))
evalBinOp Sum (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
(+) (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
evalEA (expA2ea e1) + evalEA (expA2ea e2)
-------------------   ------------------
                                        (HI.1 HI.2)
evalExpA e1 + evalExpA e2

Caso ind. 1 DER:
evalExpA (Suma e1 e2)
evalExpA e1 + evalExpA e2


Caso ind. 2 IZQ:
evalEA (expA2ea (Prod e1 e2))
evalEA (BOp Mul (expA2ea e1) (expA2ea e2))
evalBinOp Mul (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
(*) (evalEA (expA2ea e1)) (evalEA (expA2ea e2))
evalEA (expA2ea e1) * evalEA (expA2ea e2)
evalExpA e1 * evalExpA e2

Caso ind. 2 DER:
evalExpA (Prod e1 e2)
evalExpA e1 * evalExpA e2

=====================================================================

Ejercicio 2) Dada la siguiente definición
-}
data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

{-
que representa una estructura de árboles binarios;
a. implementar las siguientes funciones por recursión estructural (o primitiva en
caso de ser necesario):
-}

-- Describe la cantidad de hojas en el árbol dado.
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Nodo _ a1 a2) = cantidadDeHojas a1 + cantidadDeHojas a2  

-- Describe la cantidad de nodos en el árbol dado.
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _) = 0
cantidadDeNodos (Nodo _ a1 a2)= 1 + cantidadDeNodos a1 + cantidadDeNodos a2

-- Describe la cantidad de constructores en el árbol dado.
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ a1 a2) = 1 + cantidadDeConstructores a1 + cantidadDeConstructores a2

-- Describe la representación como elemento del tipo Arbol BinOp Int de la
-- expresión aritmética dada.
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)     = Hoja n
ea2Arbol (BOp b e1 e2) = Nodo b (ea2Arbol e1) (ea2Arbol e2)

{-
b. demostrar la siguiente propiedad:
para todo t :: Arbol a b
    cantidadDeHojas t + cantidadDeNodos t
        = cantidadDeConstructores t

para todo t :: Arbol a b. ¿ch t + cn t = cc t?
Sea t':: Arbol a b, por ppio de ind. sobre la estructura de t', se verá que:
Caso base, t'=(Hoja y):
    ¿ch (Hoja y) + cn (Hoja y) = cc (Hoja y)?
Caso ind., t'=(Nodo x a1 a2):
    HI1) ¡ch a1 + cn a1 = cc a1!
    HI2) ¡ch a2 + cn a2 = cc a2!
    TI)  ¿ch (Nodo x a1 a2) + cn (Nodo x a1 a2) = cc (Nodo x a1 a2)?

Caso base IZQ:
ch (Hoja y) + cn (Hoja y)
1 + 0
1

Caso base DER:
cc (Hoja y)
1

Caso ind IZQ:
ch (Nodo x a1 a2) + cn (Nodo x a1 a2)
(ch a1 + ch a2) + (1 + cn a1 + cn a2)
1 + (ch a1 + cn a1) + (ch a2 + cn a2)
    ---------------   ---------------
1 + cc a1 + cc a2

Caso ind DER:
cc (Nodo x a1 a2)
1 + cc a1 + cc a2


=====================================================================

Ejercicio 3) Dada la siguiente definición
-}
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
{-
que expresa la estructura de árboles binarios;
a. implementar las siguientes funciones utilizando recursión estructural (o
primitiva en caso de ser necesario):
-}

-- Describe el número resultante de sumar todos los números en el árbol dado.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- Describe la cantidad de elementos en el árbol dado.
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

-- Indica si en el árbol dado hay al menos un elemento que cumple con el predicado
anyT :: (a -> Bool) -> Tree a -> Bool
anyT p EmptyT          = False
anyT p (NodeT x t1 t2) = p x || anyT p t1 || anyT p t2

-- Describe la cantidad de elementos en el árbol dado que cumplen con el predicado dado.
countT :: (a -> Bool) -> Tree a -> Int
countT p EmptyT          = 0
countT p (NodeT x t1 t2) = unoSi (p x) + countT p t1 + countT p t2

unoSi True = 1
unoSi False = 0

-- Describe la cantidad de hojas del árbol dado.
countLeaves :: Tree a -> Int
countLeaves EmptyT                  = 0
countLeaves (NodeT x EmptyT EmptyT) = 1   
countLeaves (NodeT _ t1 t2)         = countLeaves t1 + countLeaves t2

-- Describe la altura del árbol dado.
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

-- Describe la lista in order con los elementos del árbol dado.
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2

-- Describe la lista donde cada elemento es una lista con los elementos de un nivel del árbol
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles:: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss = yss
juntarNiveles xss [] = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss 

--        1
--    2        3
-- 4    5    6   7

-- Describe un árbol con los mismos elemento que el árbol dado pero en orden inverso.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- Describe la lista con los elementos del nivel dado en el árbol dado.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x _  _)  = [x]
levelN n (NodeT _ t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- Describe la lista con los elementos de la rama más larga del árbol.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = x : masLarga (ramaMasLarga t1) (ramaMasLarga t2)

masLarga :: [a] -> [a] -> [a]
masLarga xs [] = xs
masLarga [] ys = ys
masLarga xs ys = if length xs > length ys then xs else ys

-- Describe la lista con todos los caminos existentes en el árbol dado.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1     t2)     = consAll x (todosLosCaminos t1 ++ todosLosCaminos t2)

consAll :: a -> [[a]] -> [[a]]
consAll _ []       = []
consAll x (ys:yss) = (x : ys) : consAll x yss

{-
b. demostrar las siguientes propiedades:
i. heightT = length . ramaMasLarga

¿heightT = length . ramaMasLarga?

Por ppio de ext. ¿Para todo a. heightT a = length . ramaMasLarga a?
=                          (def. (.))
¿Para todo a. heightT a = length (ramaMasLarga a)?

Sea t::Tree a, por ppio de ind. sobre la estructura de t, se verá que:

Caso base, t= EmptyT:
    ¿heightT EmptyT = length (ramaMasLarga EmptyT)?
Caso ind., t=(NodeT x t1 t2):
    HI1) ¡heightT t1= length (ramaMasLarga t1)!
    HI2) ¡heightT t2= length (ramaMasLarga t2)!
    TI)  ¿heightT (NodeT x t1 t2) = length (ramaMasLarga (NodeT x t1 t2))?

Caso Base:
IZQ:
heightT EmptyT 
0

length (ramaMasLarga EmptyT)
length []
0

Vale para este caso
Caso Ind.
IZQ:
heightT (NodeT x t1 t2)
1 + max (heightT t1) (heightT t2)


Der
length (ramaMasLarga (NodeT x t1 t2))
Lema4
1 + max (length (ramaMasLarga t1)) (length (ramaMasLarga t2))
h1 h2
1 + max (heightT t1) (heightT t2)

Lema 4: 
    Prop.: ¿length (ramaMasLarga (NodeT x li ld)) = 1 + max (length (ramaMasLarga li)) (length (ramaMasLarga ld))?
    Dem.: 
        Demostracion por analisis de casos:
    
        Caso a) length (ramaMasLarga li) > (length (ramaMasLarga ld)
            Lado izq.:
                    length (ramaMasLarga (NodeT x li ld))
                =           ---------------------------- (ramaMasLarga.2)
                    length (x : ramaMasLarga li)
                =   
                    1 + (length (ramaMasLarga li))
            
            Lado der.:
                    1 + max (length (ramaMasLarga li)) (length (ramaMasLarga ld))
                =       ---------------------------------------------------------
                    1 + (length (ramaMasLarga li))

    Se cumple para este caso.

=====================================================================
ii. reverse . inOrder = inOrder . mirrorT

¿reverse . inOrder = inOrder . mirrorT?

Por ppio de ext. ¿Para todo a. reverse . inOrder a = inOrder . mirrorT a?
=                          (def. (.))
¿Para todo a. reverse (inOrder a )= inOrder (mirrorT a)?

Sea t::Tree a, por ppio de ind. sobre la estructura de t, se verá que:

Caso base, t= EmptyT:
    ¿reverse (inOrder EmptyT)= inOrder (mirrorT EmptyT)?
Caso ind., t=(NodeT x t1 t2):
    HI1) ¡reverse (inOrder t1 )= inOrder (mirrorT t1)!
    HI2) ¡reverse (inOrder t2 )= inOrder (mirrorT t2)!
    TI)  ¿reverse (inOrder (NodeT x t1 t2))= inOrder (mirrorT (NodeT x t1 t2))?

caso b
IZQ
reverse (inOrder EmptyT)
reverse []
[]

DER:
inOrder (mirrorT EmptyT)
inOrder EmptyT
[]

caso Ind.
IZQ:
reverse (inOrder (NodeT x t1 t2))
reverse (inOrder t1 ++ [x] ++ inOrder t2)
                                        LEMA 5 
reverse (inOrder t1) ++ reverse [x] ++ reverse (inOrder t2)
                                            (HIP 1 HIP 2)
inOrder (mirrorT t1) ++ reverse [x] ++ inOrder (mirrorT t2)
                                            (reverse)
inOrder (mirrorT t1) ++ [x] ++ inOrder (mirrorT t2)
                                        
DER:
inOrder (mirrorT (NodeT x t1 t2))
inOrder (NodeT x (mirrorT t1) (mirrorT t2))
inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t2)


LEMA 5:
reverse (xs ++ ys ++ zs) = reverse xs ++ reverse ys ++ reverse zs

reverse (xs ++ ys ++ zs)
reverse ((xs ++ ys) ++ zs)        
(asociatividad de ++)
reverse zs ++ reverse (xs ++ ys) 
 (asociatividad de ++)
reverse zs ++ (reverse ys ++ reverse xs)
 (asociatividad de ++)
reverse zs ++ reverse ys ++ reverse xs


====================================================================
-}
--Ejercicio 4) Dada la siguiente definición
data AppList a = Single a | Append (AppList a) (AppList a)
--cuya intención es describir una representación no lineal de listas no vacías;

--a. implementar las siguientes funciones por recursión estructural (o primitiva en
--caso de ser necesario):

--Describe la cantidad de elementos de la lista.
lenAL :: AppList a -> Int
lenAL (Single _) = 1
lenAL (Append ap1 ap2) = lenAL ap1 + lenAL ap2

--Describe la lista resultante de agregar el elemento
--dado al principio de la lista dada.
consAL :: a -> AppList a -> AppList a
consAL x = Append (Single x) 

--Describe el primer elemento de la lista dada.
headAL :: AppList a -> a
headAL (Single n) = n
headAL (Append ap1 ap2) = headAL ap1

-- que describe la lista resultante de quitar el
-- primer elemento de la lista dada.
tailAL :: AppList a -> AppList a
tailAL (Single l)       = error ""
tailAL (Append al1 al2) = tailALL (tailAL al1) (tailAL al2)

tailALL :: AppList a -> AppList a -> AppList a
tailALL (Single l) appl       = appl  
tailALL (Append al1 al2) app1 = Append (tailALL al1 al2) app1

--Describe la lista resultante de agregar
--el elemento dado al final de la lista dada.
snocAL :: AppList a -> a -> AppList a 
snocAL xs x = Append xs (Single x)

--Describe el último elemento de la lista dada.
lastAL :: AppList a -> a 
lastAL (Single l)       = l  
lastAL (Append al1 al2) = lastAL al2

--Describe la lista dada sin su último elemento.
initAL :: AppList a -> AppList a
initAL (Single n) = error "" 
initAL (Append al1 al2) = initALL (initAL al1) (initAL al2)

initALL :: AppList a -> AppList a -> AppList a
initALL appl (Single l)       = appl  
initALL (Append al1 al2) app1 = Append (initALL al1 al2) app1


--Describe la lista dada con sus elementos en orden inverso.
reverseAL :: AppList a -> AppList a 
reverseAL (Single l)       = Single l 
reverseAL (Append al1 al2) = Append (reverseAL al2) (reverseAL al1)

--que indica si el elemento dado se
-- encuentra en la lista dada.
elemAL :: Eq a => a -> AppList a -> Bool
elemAL x (Single y) = x == y 
elemAL x (Append al1 al2) = elemAL x al1 || elemAL x al2

--Describe el resultado de agregar los elementos de la primera lista
--adelante de los elementos de la segunda.
--NOTA: buscar la manera más eficiente de hacerlo.
appendAL :: AppList a -> AppList a -> AppList a
appendAL = Append

--Describe la representación lineal de la lista dada.
appListToList :: AppList a -> [a]
appListToList (Single n) = [n]
appListToList (Append ap1 ap2) = appListToList ap1 ++ appListToList ap2

{-
=====================================================================
b. demostrar las siguientes propiedades:
i. para todo xs :: AppList a. para todo ys :: AppList a.
lenAL (appendAL xs ys) = lenAL xs + lenAL ys

¿para todo xs :: AppList a. para todo ys :: AppList a.
lenAL (appendAL xs ys) = lenAL xs + lenAL ys?

Sean zs :: AppList a. para todo ws :: AppList a.
sobre la estructura de zs, se verá que:

Caso, zs=(Append ap1 ap2):
    TI)  ¿lenAL (appendAL (Append ap1 ap2) ws) = lenAL (Append ap1 ap2) + lenAL ws?

IZQ:
lenAL (appendAL (Append ap1 ap2) ws)
=                               (appendAL)
lenAL (Append (Append ap1 ap2) ws)
=                                (lenAL.2)
lenAL (Append ap1 ap2) + lenAL ws

DER:
lenAL (Append ap1 ap2) + lenAL ws

Vale para este caso
Vale la propiedad
=====================================================================
ii. reverseAL . reverseAL = id

¿reverseAL . reverseAL = id?

Por ppio de ext. ¿Para todo apl. reverseAL . reverseAL apl = id apl?
=                          (def. (.))
¿Para todo apl. reverseAL (reverseAL apl) = id apl?
=                          (def. id)
¿Para todo apl. reverseAL (reverseAL apl) = apl?

Sea apls :: AppList a, por ppio de ind. sobre la estructura de apls, se verá que:

Caso base, apls = (Single x):
    ¿reverseAL (reverseAL (Single x)) = (Single x)?
Caso ind. apls = (Append ap1 ap2):
    HI1) ¡reverseAL (reverseAL ap1) = ap1!
    HI2) ¡reverseAL (reverseAL ap2) = ap2!
    TI)  ¿reverseAL (reverseAL (Append ap1 ap2)) = (Append ap1 ap2)?

Caso base
IZQ:
reverseAL (reverseAL (Single x))
=                       (reverseAL.1)
reverseAL (Single x)
=                       (reverseAL.1)
Single x

Der:
Single x

Vale para este caso

Caso Ind
IZQ:
reverseAL (reverseAL (Append ap1 ap2))
          ----------------------------
=                                   (reverseAL.2)
reverseAL (Append (reverseAL ap2) (reverseAL ap1))
=                                   (reverseAL.2)
Append (reverseAL (reverseAL ap1)) (reverseAL (reverseAL ap2))
=                                       (hi.1, hi.2)
Append ap1 ap2

DER:
Append ap1 ap2)

Vale para este caso
Vale la propiedad
=====================================================================
iii. (reverseAL .) . flip consAL . reverseAL = snocAL

¿(reverseAL .) . flip consAL . reverseAL = snocAL?

Por ppio de ext. ¿Para todo apl. para todo t. ((reverseAL .) . flip consAL . reverseAL) apl t = snocAL apl t?
=                                                                                 (def. (.))
¿Para todo apl. para todo t. (((reverseAL .) . flip consAL) . reverseAL) apl t = snocAL apl t?
¿Para todo apl. para todo t. ((reverseAL .) . flip consAL) (reverseAL apl) t = snocAL apl t?
¿Para todo apl. para todo t. (reverseAL .) (flip consAL (reverseAL apl)) t = snocAL apl t?
¿Para todo apl. para todo t. reverseAL ((flip consAL (reverseAL apl)) t) = snocAL apl t?
¿Para todo apl. para todo t. reverseAL (flip consAL (reverseAL apl) t) = snocAL apl t?
¿Para todo apl. para todo t. reverseAL (consAL t (reverseAL apl)) = Append apl (Single t)?
¿Para todo apl. para todo t. reverseAL (Append (Single t) (reverseAL apl)) = Append apl (Single t)?

¿Para todo apl. para todo t. Append (reverseAL (reverseAL apl) (reverseAL (Single t))) = Append apl (Single t)?
¿Para todo apl. para todo t. Append apl (reverseAL (Single t)) = Append apl (Single t)?
¿Para todo apl. para todo t. Append apl (Single t) = Append apl (Single t)?

queda demostrado para todo apl y para todo t

=====================================================================
Ejercicio 5) Dadas las siguientes definiciones
-}
data QuadTree a = LeafQ a
                | NodeQ (QuadTree a) (QuadTree a)
                        (QuadTree a) (QuadTree a)
data Color = RGB Int Int Int
type Image = QuadTree Color
--donde QuadTree representa a la estructura de los árboles cuaternarios, Color
--representa a los colores con precisión TrueColor, e Image representa imágenes
--cuadradas de tamaños arbitrarios;

--a. implementar las siguientes funciones por recursión estructural (o primitiva en caso de ser necesario):
--i. que describe la altura del árbol dado.
heightQT :: QuadTree a -> Int 
heightQT (LeafQ n) =  0
heightQT (NodeQ q1 q2 q3 q4) = 1 + max (max (heightQT q1) (heightQT q2))
                                       (max (heightQT q3) (heightQT q4))

--ii. que describe la cantidad de hojas del árbol dado.
countLeavesQT :: QuadTree a -> Int 
countLeavesQT (LeafQ n) = 1 
countLeavesQT (NodeQ q1 q2 q3 q4) = countLeavesQT q1 + countLeavesQT q2
                                  + countLeavesQT q3 + countLeavesQT q4

--iii. que describe la cantidad de constructores del árbol dado.
sizeQT :: QuadTree a -> Int
sizeQT (LeafQ n) = 1 
sizeQT (NodeQ q1 q2 q3 q4) = 1 + sizeQT q1 + sizeQT q2 + sizeQT q3 + sizeQT q4

--iv. que describe el árbol resultante de transformar en hoja
--todos aquellos nodos para los que se cumpla que todas sus hojas tengan el mismo valor.
compress :: Eq a => QuadTree a -> QuadTree a
compress (LeafQ n) = (LeafQ n)
compress (NodeQ q1 q2 q3 q4) = compressL (compress q1) (compress q2) (compress q3) (compress q4)

compressL (LeafQ n1) (LeafQ n2) (LeafQ n3) (LeafQ n4) =
    if (n1 == n2) == (n3 == n4)
        then LeafQ n1
        else NodeQ (LeafQ n1) (LeafQ n2) (LeafQ n3) (LeafQ n4)
compressL q1 q2 q3 q4 = NodeQ q1 q2 q3 q4


-- v. Describe el árbol resultante de transformar en nodo (manteniendo el dato de la
-- hoja correspondiente) todas aquellas hojas que no se encuentren en
-- el nivel de la altura del árbol.
uncompress :: QuadTree a -> QuadTree a
uncompress q = descomprimirHasta (heightQT q) q

descomprimirHasta :: Int -> QuadTree a -> QuadTree a
descomprimirHasta 0 q                   = q
descomprimirHasta n (LeafQ x)           = NodeQ (descomprimirHasta (n-1) (LeafQ x))
                                                (descomprimirHasta (n-1) (LeafQ x))
                                                (descomprimirHasta (n-1) (LeafQ x))
                                                (descomprimirHasta (n-1) (LeafQ x))
descomprimirHasta n (NodeQ q1 q2 q3 q4) = NodeQ (descomprimirHasta (n-1) q1)
                                                (descomprimirHasta (n-1) q2)
                                                (descomprimirHasta (n-1) q3)
                                                (descomprimirHasta (n-1) q4)

-- vi. Describe la imagen dada en el tamaño dado.
-- Precondición: el tamaño dado es potencia de 4 y es mayor o igual a la
-- altura del árbol dado elevado a la 4ta potencia.
-- NOTA: Una imagen tiene tamaño t cuando todas las hojas se
-- encuentran en el nivel log4(t).
-- AYUDA: Esta operación es similar a uncompress, pero pudiendo
-- variar la altura del árbol
render :: Image -> Int -> Image
render (LeafQ c)                   1 = LeafQ c
render _                           1 = error "El tamaño debería ser mas grande que la altura"
render (LeafQ c)                   n = NodeQ (render (LeafQ c) (n `div` 4))
                                             (render (LeafQ c) (n `div` 4))
                                             (render (LeafQ c) (n `div` 4))
                                             (render (LeafQ c) (n `div` 4))
render (NodeQ img1 img2 img3 img4) n = NodeQ (render img1 (n `div` 4))
                                             (render img2 (n `div` 4))
                                             (render img3 (n `div` 4))
                                             (render img4 (n `div` 4))
{-
b. demostrar las siguientes propiedades:

i. countLeavesQT . uncompress = (logBase 4) . heightQT
siendo logBase :: Int -> Int -> Int que dadas una base y un valor,
describe el logaritmo del valor en la base dada.

¿countLeavesQT . uncompress = (logBase 4) . heightQT?
Ppio de ext.
¿p.t. q. (countLeavesQT . uncompress) q = ((logBase 4) . heightQT) q?
¿p.t. q. countLeavesQT (uncompress q) = logBase 4 (heightQT q)?

Sea qt::(QuadTree a), por ppio de ind. sobre la estructura de qt, se verá que:

Caso base, qt=(LeafQ n):
    ¿countLeavesQT (uncompress (LeafQ n)) = logBase 4 (heightQT (LeafQ n))?
Caso ind., qt=(NodeQ q1 q2 q3 q4):
    ¡countLeavesQT (uncompress q1) = logBase 4 (heightQT q1)!
    ¡countLeavesQT (uncompress q2) = logBase 4 (heightQT q2)!
    ¡countLeavesQT (uncompress q3) = logBase 4 (heightQT q3)!
    ¡countLeavesQT (uncompress q4) = logBase 4 (heightQT q4)!
    ¿countLeavesQT (uncompress (NodeQ q1 q2 q3 q4)) = logBase 4 (heightQT (NodeQ q1 q2 q3 q4))?

Caso Base IZQ:
countLeavesQT (uncompress (LeafQ n))
countLeavesQT (descomprimirHasta (heightQT (LeafQ n)) (LeafQ n))
countLeavesQT (descomprimirHasta 1 (LeafQ n))
countLeavesQT (LeafQ n)
1

Caso Base DER:
logBase 4 (heightQT (LeafQ n))
logBase 4 1
0

=================================================================================
ii. uncompress = subst render heightQT
¿uncompress = subst render heightQT?
Por ppio de ext.
¿p.t. q. uncompress q = subst render heightQT q?
¿p.t. q. uncompress q = render q (heightQT q)?

Sea qt::(QuadTree a), por ppio de ind. sobre la estructura de qt, qvq:

Caso base, qt=(LeafQ x):
    ¿uncompress (LeafQ x) = render (LeafQ x) (heightQT (LeafQ x))?
Caso ind., qt=(NodeQ q1 q2 q3 q4):
    ¡uncompress q1 = render q1 (heightQT q1)!
    ¡uncompress q2 = render q2 (heightQT q2)!
    ¡uncompress q3 = render q3 (heightQT q3)!
    ¡uncompress q4 = render q4 (heightQT q4)!
    ¿uncompress (NodeQ q1 q2 q3 q4) = render (NodeQ q1 q2 q3 q4) (heightQT (NodeQ q1 q2 q3 q4))?

Caso Base IZQ:
uncompress (LeafQ x)
descomprimirHasta (heightQT (LeafQ x)) (LeafQ x)
descomprimirHasta 0 (LeafQ x)
LeafQ x

Caso Base DER:
render (LeafQ x) (heightQT (LeafQ x))
render (LeafQ x) 0
render (LeafQ x) 0

-}