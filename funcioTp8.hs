--NOTAS

compose :: (b -> c) -> (a -> b) -> a -> c   
compose f g x = f (g x)

flip :: (c -> d) -> (c -> (a -> d))
flip f x y = (f y) x

{-
succ 2 + succ 3
3 + succ 3
(+) 3 (succ 3)
(+) 3 4
7

a s s f a . f b
(.) (a s s f a) (f b)
compose (a s s f a) (f b)

10 - 3 - 2
(10 - 3) - 2
(-) (10 - 3) 2
(-) ((-) 10 3) 2

f . (g . h)
-----------------------------
----------Sección I----------
-----------------------------
Ejercicio 1) Definir las siguientes funciones sobre listas utilizando recursión
estructural (o primitiva de ser necesario):

a. length :: [a] -> Int, que describe la cantidad de elementos de la lista.
-}
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

--b. sum :: [Int] -> Int, que describe la suma de todos los elementos de la
--lista.

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

--c. product :: [Int] -> Int, que describe el producto entre todos los
--elementos de la lista.

product :: [Int] -> Int
product [] = 0
product (x:xs) = x * product xs 

--d. concat :: [[a]] -> [a], que describe la lista resultante de concatenar
--todas las listas que son elementos de la dada.

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss 

--e. elem :: Eq a => a -> [a] -> Bool, que indica si el elemento dado
--pertenece a la lista.

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys 

--f. all :: (a -> Bool) -> [a] -> Bool, que indica si todos los
--elementos de la lista cumplen el predicado dado.

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && all f xs

--g. any :: (a -> Bool) -> [a] -> Bool, que indica si algún elemento de
--la lista cumple el predicado dado.

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs 

--count :: (a -> Bool) -> [a] -> Int, que describe la cantidad de
--elementos de la lista que cumplen el predicado dado.

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs) = unoSi (f x) + count f xs 

unoSi :: Bool -> Int 
unoSi True = 1
unoSi False = 0
--i. subset :: Eq a => [a] -> [a] -> Bool, que indica si todos los
--elementos de la primera lista se encuentran en la segunda.

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys = elem x ys && subset xs ys


--j. (++) :: [a] -> [a] -> [a], que describe el resultado de agregar los
--elementos de la primera lista adelante de los elementos de la segunda.

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys =  x : (++) xs ys

--k. reverse :: [a] -> [a], que describe la lista que tiene los elementos en
--el orden inverso a la lista dada.

reverse :: [a] -> [a]
reverse xs = g xs [] where 
    g [] zs = zs
    g (y:ys) zs = g ys (y:zs)

--l. zip :: [a] -> [b] -> [(a,b)], que describe la lista resultante de
--juntar de a pares los elementos de ambas listas, según la posición que
--comparten en cada una.

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

--m. unzip :: [(a,b)] -> ([a],[b]), que describe el par de listas que
--resulta de desarmar la lista dada; la primera componente del resultado se
--corresponde con las primeras componentes de los pares dados, y la segunda
--componente con las segundas componentes de dichos pares.

unzip :: [(a,b)] -> ([a],[b])
unzip [] = []
unzip ((x,y):xys) = let (xs,ys) = unzip xys in (x:xs,y:ys)

{-
Ejercicio 2) Demostrar por inducción estructural las siguientes propiedades:

===========================================================================
a. para todo xs. para todo ys.
length (xs ++ ys) = length xs + length ys

¿para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys?
Dem: Sean zs y ws listas cualesquiera (finitas y totalmente definidas).
 Por ppio. de ind. en la estructura de zs es equivalente demostrar
Caso base, zs = [])
 ¿len ([] ++ ws) = len [] + len ws?
Caso ind., zs = x:xs')
HI) ¡len (xs' ++ ws) = len xs' + len ws!
TI) ¿len ((x:xs') ++ ws) = len (x:xs') + len ws?

Prop: ¿para todo xs e ys.
len (xs ++ ys) = len xs + len ys?
Dem: Caso base, zs = []) ¿len ([] ++ ws) = len [] + len ws?

len ([] ++ ws)                   len [] + len ws
=           ((++) y append.1)    =              (len.1)
 len ws                          0 + len ws
                                 =              (aritm.)
                                 len ws
                                                    Vale este caso

Prop: ¿para todo xs e ys.
len (xs ++ ys) = len xs + len ys?

Dem: Caso ind., zs = x:xs') HI) ¡len (xs' ++ ws) = len xs' + len ws!
    TI) ¿len ((x:xs')++ ws) = len (x:xs') + len ws?

 len ((x:xs') ++ ws)                 len (x:xs') + len ws
=                ((++) y append.2)   =                  (len.2)
 len (x : (xs'++ws))                 (1 + len xs') + len ws
=               (len.2)              =                  (aritm.)
 1 + len (xs'++ws)                   1 + (len xs' + len ws)
= (HI)
 1 + (len xs' + len ws)

                                                Vale este caso
                                                Vale la propiedad


===========================================================================
b. para todo xs. para todo ys. para todo zs.
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)

¿para todo xs. para todo ys. para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)?
Dem: Sean xs', ys' y zs' listas cualesquiera (finitas y totalmente definidas).
 Por ppio. de ind. en la estructura de  xs' es equivalente demostrar
Caso Base:
    xs' = [])
    ¿([] ++ ys') ++ zs' = [] ++ (ys' ++ zs')?
Caso ind., xs'= p:ps)
    HI) ¡(ps ++ ys') ++ zs' = ps ++ (ys' ++ zs')!
    TI) ¿((p:ps) ++ ys') ++ zs' = (p:ps) ++ (ys' ++ zs')?

Dem: 
Caso Base:
IZQ:
xs' = [])
¿([] ++ ys') ++ zs' = [] ++ (ys' ++ zs')?

 ([] ++ ys') ++ zs'
 -----------
=               ((++).2) 
 ys' ++ zs'

DER:
 [] ++ (ys' ++ zs')
 ------------------
=               ((++).1) 
 ys' ++ zs'

Vale este caso

Dem: Caso ind., xs'= p:ps)
    HI) ¡(ps ++ ys') ++ zs' = ps ++ (ys' ++ zs')!
    TI) ¿((p:ps) ++ ys') ++ zs' = (p:ps) ++ (ys' ++ zs')?

IZQ:
 ((p:ps) ++ ys') ++ zs'
 ---------------
=                      ((++).2)
 (p : (ps ++ ys')) ++ zs'
 ------------------------
=                      ((++).2)
 p: ((ps ++ ys') ++ zs')
    --------------------
=                      (HI)
 p:ps ++ (ys' ++ zs')

 Der:
 (p:ps) ++ (ys' ++ zs')

Vale este caso
Vale la propiedad


===========================================================================
c. count (const True) = length
Por el principio de extensionalidad. ¿Para todo xs. count (const True) xs = length xs?
Dem: Sea ys una lista cualesquiera (finitas y totalmente definidas).
 Por ppio. de ind. en la estructura de ys es equivalente demostrar
Caso Base:
    ys = [])
    ¿count (const True) [] = length []?
Caso ind. ys = z:zs)
    HI) ¡count (const True) zs = length zs!
    TI) ¿count (const True) (z:zs) = length (z:zs)?

Dem: 
Caso Base:
IZQ:
count (const True) []
------------------------
=                   (count.1)
0

Der:
length []
---------
=                   (length.1)
0

Vale Para Este Caso

Dem: 
Caso ind:
IZQ:
count (const True) (z:zs)
---------------------------
=                       (count.2)
unoSi (const True z) + count (const True) zs 
       -------------  
=                       (const)
unoSi True + count (const True) zs 
-----------
=                       (unoSi.1)
1 + count (const True) zs
   ------------------------
=                       (HI)
1 + length zs

DER:
length (z:zs) 
=               (length.2)
1 + length zs

Vale este caso
Vale la propiedad


===========================================================================
d. elem = any . (==)
Por el principio de extensionalidad. ¿Para todo xs. Para todo y. elem y xs = (any . (==)) y xs?
Dem: Sea una ys lista cualesquiera (finitas y totalmente definidas) y x un elemento 
de tipo Cualquiera.
 Por ppio. de ind. en la estructura de ys es equivalente demostrar
Caso Base: ys = [])
        ¿elem x [] = (any . (==)) x []?
Caso ind: ys = z:zs)
    HI) ¡elem x zs = (any . (==)) x zs!
    TI) ¿elem x (z:zs) = (any . (==)) x (z:zs)?

Dem:
Caso Base:
DER:
(any . (==)) x []
--------------
=               ((.))
any (==x) []
-----------
=             (any.1)
False

IZQ:
elem x [] 
---------
=          (elem.1)
False

Dem:
Caso ind:
DER:
(any . (==)) x (z:zs)
-------------
=               ((.))
any ((==) x) (z:zs)
------------------  
=               (any.2)
((==) x) z || any ((==) x) zs 
              -------------
=                           ((.))
((==) x) z || (any . (==)) x zs
               -------------
=                            (HI)
((==) x) z || elem x zs 
-----------
=                              ((==))
z == x || elem x zs 

IZQ:
elem x (z:zs) 
-------------
=               (elem.2)
x == z || elem x zs 

Vale este caso
Vale la propiedad


===========================================================================
e. para todo x. any (elem x) = elem x . concat
Por el ppio. de ext. ¿Para todo xss. para todo x. any (elem x) xss = (elem x . concat) xss?
=                                                                                        (def. (.))
 ¿Para todo xss. para todo x. any (elem x) xss = elem x (concat xss)?

Dem: Sea una yss::[[a]], y::a
Por ppio. de ind. en la estructura de yss es equivalente demostrar:

Caso Base: yss = [])
        ¿any (elem y) [] = elem y (concat [])?
Caso ind: yss = (zs:zss))
    HI) ¡any (elem y) zss = elem y (concat zss)!
    TI) ¿any (elem y) (zs:zss) = elem y (concat (zs:zss))?

Dem: 
Caso Base:
IZQ:
any (elem y) []
---------------
=                  (any.1)
False

DER:
elem y (concat [])
        ----------
=                   (concat.1)     
elem y []
--------
=                   (elem.1)
False

Vale para este Caso


Dem: 
Caso ind:
IZQ:
any (elem y) (zs:zss)
---------------------
=                  (any.2)
elem y zs || any (elem y) zss
               ----------------
=                              (HI)
elem y zs || elem y (concat zss)

DER:
elem y (concat (zs:zss))
---------------------
=                    (concat.2) 
elem y (zs ++ concat zss)
-------------------------
=                        (LEMA.1)
elem y zs || elem y (concat zss)

Vale para este caso
Vale esta propiedad

===========================================================================
LEMA.1: para todo x. xs. ys. elem x (xs ++ ys) = elem x xs || elem x ys

¿para todo x. xs. ys. elem x (xs ++ ys) = elem x xs || elem x ys?
Sean y::a, zs::[a], ws::[a] es equivalente demostrar:
Por ppio. de ind. en la estructura de zs es equivalente demostrar
Caso base, zs = [])
¿elem y ([] ++ ws) = elem y [] || elem y ws?
Caso ind., zs = p:ps)
HI) ¡elem y (ps ++ ws) = elem y ps || elem y ws!
TI) ¿elem y ((p:ps) ++ ws) = elem y (p:ps) || elem y ws?


Caso Base:
IZQ:
elem y ([] ++ ws)
        --------
=                   ((++).1)
elem y ws 

DER:
elem y [] || elem y ws
---------
=                      (elem.1)
False || elem y ws
=                       (Def de (||))
elem y ws

Vale para este caso

Caso Ind:
DER:
elem y (p:ps) || elem y ws
--------------
=                              (elem.2)
y == p || elem y ps || elem y ws
          ----------------------
=                               (HI)
y == p || elem y (ps ++ ws)
        
IZQ:
elem y ((p:ps) ++ ws) 
        ------------
=                       ((++).2)
elem y (p : (ps ++ ws))
-----------------------
=                       (elem.2)
y == p || elem y (ps ++ ws)

Vale para este caso
Vale para esta propiedad

===========================================================================
f. para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs

¿para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs?

Dem: Sean una zs::[a], ws::[a]
Por ppio. de ind. en la estructura de zs es equivalente demostrar:
Caso Base: zs = [])
        ¿subset [] ws = all (flip elem ws) []?
Caso ind: zs = (p:ps))
    HI) ¡subset ps ws = all (flip elem ws) ps!
    TI) ¿subset (p:ps) ws = all (flip elem ws) (p:ps)?

Caso Base:
IZQ:
subset [] ws
------------
=                  (subset.1)
True

DER:
all (flip elem ws) []
---------------------
=                  (all.1)
True

Vale para este Caso

Caso Ind:
DER:
subset (p:ps) ws
----------------
=               (subset.2)
elem p ws && subset ps ws

IZQ:
all (flip elem ws) (p:ps)
-------------------------
=                       (all.2)
(flip elem ws) p && all (flip elem ws) ps
                    ---------------------
=                                   (HI)
(flip elem ws) p && subset ps ws
----------------
=                                   (flip)
elem p ws && subset ps ws

Vale para este caso
Vale la Propiedad

===========================================================================
g. all null = null . concat




Por el ppio. de ext. ¿Para todo xs. all null xs = (null . concat) xs?
=                                                                   (def. (.))
¿Para todo xs. all null xs = null (concat xs)?

Dem: Sea una ys::[[a]]
Por ppio. de ind. en la estructura de ys es equivalente demostrar:

Caso Base: ys = [])
        ¿all null [] = null (concat [])?
Caso ind: ys = (z:zs))
    HI) ¡all null zs = null (concat zs)!
    TI) ¿all null (z:zs) = null (concat (z:zs))?

Caso Base:
IZQ:
all null []
-----------
=           (all.2)
True

Der:
null (concat [])
      ---------
=           (concat.1)
null []
-------
=           (null)
True

Vale para este caso

Caso Ind:
IZQ:
all null (z:zs)
---------------
=           (all.2)
null z && all null zs  
          ------------
=                   (HI)
null z && null (concat zs)

DER:
null (concat (z:zs))
      --------------
=                       (dem.2)
null (z ++ concat zs)



===========================================================================
h. length = length . reverse

===========================================================================
i. para todo xs. para todo ys.
reverse (xs ++ ys) = reverse ys ++ reverse xs


===========================================================================
j. para todo xs. para todo ys.
all p (xs++ys) = all p (reverse xs) && all p (reverse ys)

===========================================================================
k. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)
(en este caso, mostrar que no vale)

-----------------------------
----------Sección II---------
-----------------------------

Ejercicio 1) Dada la siguiente definición

    data N = Z | S N

cuya intención es describir representaciones unarias de números naturales,

a. implementar las siguientes funciones por recursión estructural (o primitiva de
ser necesario):
    i. evalN :: N -> Int, que describe el número representado por el
    elemento dado.
    ii. addN :: N -> N -> N, que describe la representación unaria de la
    suma de los números representados por los argumentos. La
    resolución debe ser exclusivamente simbólica, o sea, SIN calcular
    cuáles son esos números.
    iii. prodN :: N -> N -> N, que describe la representación unaria del
    producto de los números representados por los argumentos. La
    resolución debe ser exclusivamente simbólica.
    iv. int2N :: Int -> N, que describe la representación unaria del
    número dado usando el tipo N.

b. demostrar las siguientes propiedades:

i. para todo n1. para todo n2.
evalN (addN n1 n2) = evalN n1 + evalN n2
ii. para todo n1. para todo n2.
evalN (prodN n1 n2) = evalN n1 * evalN n2
iii. int2N . evalN = id
iv. evalN . int2N = id

Ejercicio 2) Dada la siguiente definición
   
    type NU = [()]

cuya intención es describir representaciones unarias de números como listas de
símbolos. El tipo () se lee Unit, y su único elemento es (); es equivalente a la
siguiente definición:

data Unit = Unit

a. implementar las siguientes funciones por recursión estructural (o primitiva de
ser necesario):
    i. evalNU :: NU -> Int, que describe el número representado por
    el elemento dado.

    ii. succNU :: NU -> NU, que describe la representación unaria del
    resultado de sumarle uno al número representado por el argumento.
    La resolución debe ser exclusivamente simbólica.

    iii. addNU :: NU -> NU -> NU, que describe la representación unaria
    de la suma de los números representados por los argumentos. La
    resolución debe ser exclusivamente simbólica.

    iv. nu2n :: NU -> N, que describe la representación unaria dada por
    el tipo N correspondiente al número representado por el argumento.

    v. n2nu :: N -> NU, que describe la representación unaria dada por
    el tipo NU correspondiente al número representado por el argumento.

b. demostrar las siguientes propiedades:
    i. evalNU . succNU = (+1) . evalNU
    ii. para todo n1. para todo n2.
    evalNU (addNU n1 n2) = evalNU n1 + evalNU n2
    iii. nu2n . n2nu = id
    iv. n2nu . nu2n = id
Ejercicio 3) Dada la siguiente definición

    type NBin = [DigBin]

cuya intención es describir representaciones binarias de números con el dígito
menos significativo a la izquierda, y siendo DigBin el tipo definido en el ejercicio 2
de la práctica 5. Es recomendable reutilizar las funciones definidas en el ejercicio
mencionado.

a. implementar las siguientes funciones por recursión estructural (o primitiva de
ser necesario):
    i. evalNB :: NBin -> Int, que describe el número representado
    por el elemento dado.
   
    ii. normalizarNB :: NBin -> NBin, que describe la representación
    binaria del número representado por el argumento, pero sin “ceros a
    la izquierda” (dígitos redundantes).
    OBSERVACIÓN: por la forma de la representación, los “ceros a
    izquierda” aparecen a la derecha de la lista. Entonces la propiedad
    indica que una lista de dígitos normalizada no puede terminar con el
    dígito 0.
   
    iii. succNB :: NBin -> NBin, que describe la representación binaria
    normalizada del resultado de sumarle uno al número representado
    por el argumento. La resolución debe ser exclusivamente simbólica, y
    no debe utilizar normalizarNB. Se puede suponer como
    precondición que el argumento está normalizado.
   
    iv. addNB :: NBin -> NBin -> NBin, que describe la
    representación binaria normalizada de la suma de los números
    representados por los argumentos. La resolución debe ser
    exclusivamente simbólica (o sea, no usar ninguna forma de eval), y
    no debe utilizar normalizarNB. Se puede suponer como
    precondición que los argumentos están normalizados.
    AYUDA: considerar dos operaciones auxiliares
    addNBConCarry :: NBin -> NBin -> DigBin -> NBin, y
    addDBConCarry :: DigBin -> DigBin
    -> DigBin -> (DigBin, DigBin).
   
    v. nb2n :: NBin -> N, que describe la representación unaria dada
    por el tipo N correspondiente al número representado por el
    argumento.
  
    vi. n2nb :: N -> NBin, que describe la representación binaria
    normalizada dada por el tipo NBin correspondiente al número
    representado por el argumento.

b. demostrar las siguientes propiedades:

===========================================================================
i. evalNB . normalizarNB = evalNB
    
===========================================================================
ii. evalNB . succNB = (+1) . evalNB
    
===========================================================================
iii. para todo n1. para todo n2.
evalNB (addNB n1 n2) = evalNB n1 + evalNB n2

===========================================================================
iv. nb2n . n2nb = id

===========================================================================
v. normalizarNB . normalizarNB = normalizarNB

c. solamente una de las siguientes propiedades es verdadera. Dar un
contraejemplo para la que no lo sea, y demostrar la que sí lo sea.

i. n2nb . nb2n = id

ii. n2nb . nb2n = normalizarNB

Ejercicio 4) (ADICIONAL con mayor extensión pero no mayor complejidad)
Dada la siguiente definición
    
    type NDec = [DigDec]

cuya intención es describir representaciones decimales de números con el dígito
menos significativo a la izquierda, y siendo DigDec el tipo definido en el ejercicio 3
de la práctica 5. Es recomendable reutilizar las funciones definidas en el ejercicio
mencionado.
a. implementar las siguientes funciones por recursión estructural (o primitiva de
ser necesario):
    i. evalND :: NDec -> Int, que describe el número representado
    por el elemento dado.
    
    ii. normalizarND :: NDec -> NDec, que describe la representación
    decimal del número representado por el argumento, pero sin “ceros a
    la izquierda” (dígitos redundantes).
    OBSERVACIÓN: por la forma de la representación, los “ceros a
    izquierda” aparecen a la derecha de la lista. Entonces la propiedad
    indica que una lista de dígitos normalizada no puede terminar con el
    dígito 0.
    
    iii. succNDec :: NDec -> NDec, que describe la representación
    decimal normalizada del resultado de sumarle uno al número
    representado por el argumento. La resolución debe ser
    exclusivamente simbólica, y no debe utilizar normalizarND. Se
    puede suponer como precondición que el argumento está
    normalizado.
    
    iv. addNDec :: NDec -> NDec -> NDec, que describe la
    representación decimal normalizada de la suma de los números
    representados por los argumentos. La resolución debe ser
    exclusivamente simbólica, y no debe utilizar normalizarND. Se
    puede suponer como precondición que los argumentos está
    normalizados.
    
    v. nd2nb :: NDec -> NBin, que describe la representación binaria
    normalizada correspondiente al número representado por el
    argumento.
    
    vi. nb2nd :: NBin -> NDec, que describe la representación decimal
    normalizada correspondiente al número representado por el
    argumento.

b. demostrar por inducción estructural las siguientes propiedades o dar
contraejemplos en los casos que no valgan (y proponer alguna variante de la
propiedad que sí sea válida):
    i. evalNDec . succNDec = (+1) . evalNDec

    ii. para todo n1. para todo n2.
    evalNDec (addNDec n1 n2)
    = evalNDec n1 + evalNDec n2
    
    iii. nd2nb . nb2nd = normalizarNB
   
    iv. nb2nd . nd2nb = id

Ejercicio 5) Dar la representación de los números 17 y 42 como N, NU, NBin y NDec.
-----------------------------
----------Sección III--------
-----------------------------

Ejercicio 1) Dada la siguiente representación de expresiones aritméticas

    data ExpA = Cte Int
    | Suma ExpA ExpA
    | Prod ExpA ExpA
a. implementar las siguientes funciones por recursión estructural (o primitiva de
ser necesario):
    i. evalExpA :: ExpA -> Int, que describe el número que resulta
    de evaluar la cuenta representada por la expresión aritmética dada.
    ii. simplificarExpA :: ExpA -> ExpA, que describe una
    expresión aritmética con el mismo significado que la dada, pero que
    no tiene sumas del número 0, ni multiplicaciones por 1 o por 0. La
    resolución debe ser exclusivamente simbólica.
    iii. cantidadDeSumaCero :: ExpA -> Int, que describe la cantidad
    de veces que aparece suma cero en la expresión aritmética dada. La
    resolución debe ser exclusivamente simbólica.
b. demostrar por inducción estructural las siguientes propiedades:
    i. evalExpA . simplificarExpA = evalExpA
    ii. cantidadSumaCero . simplificarExpA = const 0
Ejercicio 2) Dada la siguiente representación de expresiones aritméticas
    data ExpS = CteS N
    | SumS ExpS ExpS
    | ProdS ExpS ExpS
a. implementar las siguientes funciones por inducción estructural (o primitiva de
ser necesario):
    i. evalES :: ExpS -> Int, que describe el número que resulta de
    evaluar la cuenta representada por la expresión aritmética dada.
    ii. es2ExpA :: ExpS -> ExpA, que describe una expresión
    aritmética representada con el tipo ExpA, que tiene el mismo
    significado que la dada.
    iii. expA2es :: ExpA -> ExpS, que describe una expresión
    aritmética representada con el tipo ExpS, que tiene el mismo
    significado que la dada.
b. demostrar por inducción estructural las siguientes propiedades:
    i. evalExpA . es2ExpA = evalES
    ii. evalES . expA2es = evalExpA
    iii. es2ExpA . expA2es = id
    iv. expA2es . es2ExpA = id

-}