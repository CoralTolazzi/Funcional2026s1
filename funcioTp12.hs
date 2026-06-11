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
--NO HECHO
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

sizeT :: Tree a -> Int
sizeT = foldT 0 (\x t1 t2 -> 1 + t1 + t2)

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
--SACAR F


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
insertT :: a -> Tree a -> Tree a
insertT x = recT (NodeT x EmptyT EmptyT) (\y t1 t2 tr1 tr2 if x == y then NodeT y t1 t2 else
                                                           if x < y then NodeT y tr1 t2 
                                                           else NodeT y t1 tr2)

--ii. que describe el camino hasta el elemento
-- dado en el árbol dado.
--Precondición: existe el elemento en el árbol.
caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta x = recT (error "Error") (\x t1 t2 xs1 xs2 -> if x == y then [x] else
                                                          if perteneceT y t1 then x : xs1 else x : xs2

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT x (NodeT y t1 t2)= x == y || perteneceT x t1 || perteneceT x t2

--e. Demostrar las siguientes propiedades utilizando las definiciones anteriores:

--i. para todo f. sizeT . mapT f = sizeT
--ii. para todo f. para todo g. mapT f . mapT g = mapT (f . g)
--iii. foldT EmptyT NodeT = id



{-INICIO
Ejercicio 4) Dadas las siguientes definiciones para representar mapas con diferentes
puntos de interés que pueden presentar objetos:

data Dir = Left | Right | Straight
data Mapa a = Cofre [a]
| Nada (Mapa a)
| Bifurcacion [a] (Mapa a) (Mapa a)

--a. Dar el tipo y definir foldM y recM, que expresan los esquemas de recursión
--estructural y primitiva, respectivamente, para la estructura Mapa.

--b. Definir las siguientes funciones sin utilizar recursión explícita, utilizando el
--esquema de recursión estructural o primitiva sobre Mapa según corresponda::

--i.que describe la lista de todos los
--objetos presentes en el mapa dado.
objects :: Mapa a -> [a] 

--ii. que transforma los
--objetos del mapa dado aplicando la función dada.
mapM :: (a -> b) -> Mapa a -> Mapa b

--iii. que indica si existe
--algún objeto que cumpla con la condición dada en el mapa dado.
has :: (a -> Bool) -> Mapa a -> Bool 

--iv. que indica si un objeto al final del camino dado cumple con la
--condición dada en el mapa dado.
hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool

--v. que describe el camino más
--largo en el mapa dado.
longestPath :: Mapa a -> [Dir] 

--vi.que describe la lista con los objetos presentes en el
-- camino más largo del mapa dado.
objectsOfLongestPath :: Mapa a -> [a] 

--vii. que describe la lista con
--todos los caminos del mapa dado.
allPaths :: Mapa a -> [[Dir]]

--viii. que describe la lista
--con todos los objetos por niveles del mapa dado.
objectsPerLevel :: Mapa a -> [[a]]

--c. Demostrar la siguiente propiedad:
--para todo x. has (==x) = any (elem x) . objectsPerLevel

--Ejercicio 5) Dada la siguiente definición para representar árboles generales:
data GTree a = GNode a [GTree a]
--a. Dar tipo y definir las tres versiones de foldGT y recGT, que expresan los
--esquemas de recursión estructural y primitiva, respectivamente, para la
--estructura GTree.
--ATENCIÓN: recordar que no siguen la secuencia dada para tipos recurslvos
--con recursión directa, porque la recursión en GTree NO es directa. Estas
--versiones deben tener en cuenta el esquema de recursión sobre listas
--correspondiente, como se trató en clase teórica.

--b. Definir las siguientes funciones sin utilizar recursión explícita:
i. mapGT :: (a -> b) -> GTree a -> GTree b
ii. sumGT :: GTree Int -> Int
iii. sizeGT :: GTree a -> Int
iv. heightGT :: GTree a -> Int
v. preOrderGT :: GTree a -> [a]
vi. postOrderGT :: GTree a -> [a]
vii. mirrorGT :: GTree a -> GTree a
viii. countByGT :: (a -> Bool) -> GTree a -> Int
ix. partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
x. zipWithGT :: (a->b->c) -> GTree a -> GTree b -> GTree c
xi. caminoMasLargoGT :: GTree a -> [a]
xii. todosLosCaminosGT :: GTree a -> [[a]]
xiii. todosLosNivelesGT :: GTree a -> [[a]]
xiv. caminoHastaGT :: Eq a => a -> GTree a -> [a]
xv. nivelNGT :: GTree a -> Int -> [a]

--Ejercicio 6) Dadas las siguientes definiciones para representar un sistema de
archivos:
type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content
| Folder Name [FileSystem]

--a. Definir foldFS y recFS, que expresan la recursión estructural y primitiva,
--respectivamente, para la estructura FileSystem.

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

TERMINA-}

-- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- 
--FUNCIONES AUXILIARES

--foldA :: (a -> b) -> (a -> b -> b -> b) -> Arbol a -> b

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0


-- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- -- ++ -- 


-- * * * * * * *  * * * * * * * *  * * * * * * * *  * * * * * * * *  * * * * * * * *  * * * * * * * *  * * * * * * * *  * * * * * * * *  * 

{-
EJERCICIOS ADICIONALES (innecesariamente complejos)
Ejercicio 7) Dada la siguiente definición:
type Record f v = [(f,v)]
donde la idea de este tipo es representar una fila de una base de datos (el valor de
tipo f es el nombre del campo, y el valor de tipo v es el valor de ese campo). Por
ejemplo, para representar una fila de una base de datos de personas, se puede usar
el siguiente dato:
[("nombre","Grace"),("apellido","H"),("edad", "42"),("género","F"),("salario","120")] ::
Record String String
Con esto, puede definirse el tipo:
type Table f v = [ Record f v ]
donde se entiende que una tabla de una base de datos está compuesta por muchos
registros, y se espera que todos compartan los mismos “campos” (o sea, los valores
de tipo a en cada registro). Por ejemplo:
personas =
[
[("nombre","Graciela"),("apellido","H")
,("edad", "42"),("género","F"),("salario","120")]
, [("nombre","Alonso"),("apellido","Ch")
,("edad", "35"),("género","M"),("salario","250")]
, [("nombre","Adela"),("apellido","G")
,("edad", "39"),("género","F"),("salario","130")]
, [("nombre","Fer"),("apellido","R")
,("edad", "22"),("género","X"),("salario","100")]
, [("nombre","Felipe"),("apellido","W")
,("edad", "50"),("género","M"),("salario","280")]
]
Definir las siguientes funciones sin utilizar recursión estructural explícita:
a. select :: (Record f v -> Bool) -> Table f v -> Table f v,
que a partir de la lista de registros dada describe la lista de los registros que
cumplen con la condición dada.
Por ejemplo,
select (\r -> elem ("género", "F") r)
selecciona todos los registros de personas de género femenino.
Otro ejemplo es
select (\r -> any (\(c,v) -> c=="salario"
&& str2num v < 200) r)
que selecciona todos los registros de personas con salarios por debajo de
200.
Página 6 de 9
Programación Funcional
b. project :: (f -> Bool) -> Table f v -> Table f v, que a partir
de la lista de registros dada describe la lista de registros solo con los campos
que cumplen la condición dada.
Por ejemplo,
project (\f -> f=="nombre" || f=="apellido")
selecciona los campos nombre y apellido de un tabla.
c. conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool, que
describe el predicado que da True solo cuando los dos predicados dados lo
hacen.
Por ejemplo
conjunt (=="nombre") (=="apellido")
es otra forma de escribir la condición del ejemplo anterior.
d. crossWith :: (a -> b -> c) -> [a] -> [b] -> [c], que describe
el resultado de aplicar una función a cada elemento del producto cartesiano
de las dos listas dadas.
Por ejemplo
crossWith (+) [1,2,3] [10,20]
da como resultado
[11,21,12,22,13,23].
e. product :: Table f v -> Table f v -> Table f v, que describe
la lista de registros resultante del producto cartesiano combinado de las dos
listas de registros dadas. Es decir, la unión de los campos en los registros del
producto cartesiano.
Por ejemplo
product
[
[("apellido","W"),("idMat","42")]
,[("apellido","G"),("idMat","17")]
]
[
[("idMat", "17"),("nomMat","PO")]
,[("idMat","666"),("nomMat","PI")]
,[("idMat", "42"),("nomMat","PF")]
]
da como resultado
[
[("apellido","W"),("idMat","42")
,("idMat", "17"),("nomMat","PO")]
,[("apellido","W"),("idMat","42")
,("idMat","666"),("nomMat","PI")]
,[("apellido","W"),("idMat","42")
,("idMat", "42"),("nomMat","PF")]
,[("apellido","G"),("idMat","17")
,("idMat", "17"),("nomMat","PO")]
,[("apellido","G"),("idMat","17")
,("idMat","666"),("nomMat","PI")]
Página 7 de 9
Programación Funcional
,[("apellido","G"),("idMat","17")
,("idMat", "42"),("nomMat","PF")]
]
f. similar :: Record f v -> Record f v, que describe el registro
resultante de descartar datos cuyos campos sean iguales (o sea, el mismo
dato asociado al mismo campo).
Por ejemplo,
similar
[("apellido","W"),("idMat","42")
,("idMat","42"),("nombreMat","PF")]
da como resultado
[("apellido","W"),("idMat","42")
,("nombreMat","PF")].
Ejercicio 8) Una expresión del cálculo relacional de tuplas permite expresar consultas
sobre un modelo relacional con los siguientes componentes:
data Query f v
= Table [Record f v] -- Table (Table f v)
| Product (Query f v) (Query f v)
| Projection (f -> Bool) (Query f v)
| Selection (Record f v -> Bool) (Query f v)
Este modelo es utilizado para estudiar y manipular las consultas hechas a motores
de bases de datos. En este contexto vemos a cada registro como una fila y a cada
elemento como un par columna-valor. Por ejemplo:
Projection
(\f -> f== "nombre" || f=="apellido")
(Selection
(conjunct (\r -> any (\(c,v)-> c == "salario"
&& str2num v > 200) r) (\r -> elem
("género", "F") r))
(Table personas))
describe la tabla con los nombres y apellidos de todas las personas de género femenino con
salarios altos.
a. Dar el tipo y definir la función foldQ, que expresa el esquema de recursión
estructural para la estructura Query.
a. Definir las siguientes funciones sin utilizar recursión explícita:
i. tables :: Query a b -> [Table a b], que describe la lista de
todas las tablas involucradas en la query dada.
ii. execute :: Query a b -> Table a b, que describe el
resultado de ejecutar la query dada.
iii. compact :: Query a b -> Query a b, que describe la query
resultante de compactar las selecciones y proyecciones consecutivas
en la query dada.
b. Demostrar la siguiente propiedad:
execute . compact = execute
-}