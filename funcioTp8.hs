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
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]  


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
Caso ind: ys = (zs:zss))
    HI) ¡all null zss = null (concat zss)!
    TI) ¿all null (zs:zss) = null (concat (zs:zss))?

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
all null (zs:zss)
---------------
=           (all.2)
null zs && all null zss  
          ------------
=                   (HI)
null zs && null (concat zss)

DER:
null (concat (zs:zss))
      --------------
=                       (concat.2)
null (zs ++ concat zss)
----------------------
=                       (LEMA.2)
null zs && null (concat zss)

Vale La propiedad
Vale este caso
===========================================================================
LEMA.2
¿para todo xs. ys. null (xs ++ ys) = null xs && null ys?
Dem: Sean zs::[a], ws::[a]  null (zs ++ ws) = null zs && null ws 
Caso zs = []
IZQ:
null ([] ++ ws)
      --------
=               (def (++).1)
null ws

DER:
null [] && null ws
-------
=               (null.1)
True && null ws
--------------
=               (def (&&))
null ws

Vale para este Caso

caso zs = (p:ps)
IZQ:
null ((p:ps) ++ ws)
-------------------
=               (def (++))
null (p : (ps ++ ws))
---------------------
=               (def null)
False


DER:
null (p:ps) && null ws
-----------
=               (def null)
False && null ws
--------
=               (def (&&))
False


Vale para este Caso
===========================================================================
h. length = length . reverse

Por el ppio. de ext. ¿Para todo xs. length xs = (length . reverse) xs?
=                                                                     (def. (.))
 ¿Para todo xs.  length xs = length (reverse xs)?

Dem: Sea una ys::[a]
Por ppio. de ind. en la estructura de ys es equivalente demostrar:

Caso Base: ys = [])
        ¿length [] = length (reverse [])?
Caso ind: zs = (z:zs))
    HI) ¡length zs = length (reverse zs)!
    TI) ¿length (z:zs) = length (reverse (z:zs))?

Caso Base:
IZQ:
length [] 
---------- 
=          (length.1)
0

DER:
length (reverse [])
        ----------
=           (def reverse.1)
length [] 
---------- 
=          (length.1)
0

Vale para este caso


Caso ind:
IZQ:
length (z:zs)
-------------
=           (length.2)
1 + length zs

DER:
length (reverse (z:zs))
        --------------
=                   (reverse.2)
length (reverse zs ++ [z])
--------------------------
=                    (LEMA.3)
length (reverse zs) + length [z]
--------------------------------
=                       (HI)       
length zs + length [z]
            ----------
=                       (length.2)
length zs + 1 + length []
                ----------
=                       (length.1)
length zs + 1 + 0
            -----
=                    (aritm. conmutatividad)
1 + length zs

Vale Para este caso
Vale la Propiedad

LEMA.3
¿para todo as. x. length (as ++ [x]) = length (as) + length [x]?
Dem: Sean ps::[a], w::a length (ps ++ [w]) = length (ps) + length [w]
Caso ps = []
IZQ:
length ([] ++ [w])
        ---------
=               ((++).1)
length [w]
----------
=               (length.2)
1 + length []
    ---------
=               (length.1)
1 + 0
-----
=               (aritmetica)
1

DER:   
length [] + length [w]
-----------
=                  (length.1)
0 + length [w]
=                  (length.2)
0 + 1 + length []
=                  (length.1)
0 + 1 + 0
=                  (aritmetica)
1

Caso ps = (h:hs)

IZQ:
length ((h:hs) ++ [w])
length (h : (hs ++ [w]))
1 + length (hs ++ [w])
2 + length hs

(me dio paja kssss)
DER:
length (h:hs) + length [w]
1 + length hs + 1
2 + length hs

Vale para este caso
===========================================================================
i. para todo xs. para todo gs.
reverse (xs ++ gs) = reverse gs ++ reverse xs

¿para todo xs. para todo gs. reverse (xs ++ gs) = reverse gs ++ reverse xs?

Dem: Sean una zs::[a], ws::[a]
Por ppio. de ind. en la estructura de gs es equivalente demostrar:

Caso Base: zs = [])
        ¿reverse ([] ++ ws) = reverse ws ++ reverse []?
Caso ind: zs = (y:ys))
    HI) ¡reverse (ys ++ ws) = reverse ws ++ reverse ys!
    TI) ¿reverse ((y:ys) ++ ws) = reverse ws ++ reverse (y:ys)?

Caso Base:
IZQ:
reverse ([] ++ ws) 
=               ((++).1)
reverse ws

DER:
reverse ws ++ reverse []
            ------------
=                     (reverse)
reverse ws ++ []
        --------
=                     ((++).1)
reverse ws

Vale para este caso

Caso Ind:
IZQ:
reverse ((y:ys) ++ ws) 
=                      ((++).2)
reverse (y : (ys ++ ws))
=                       (reverse.2)
reverse (ys ++ ws) ++ [y]  

DER:
reverse ws ++ reverse (y:ys)
              --------------
reverse ws ++ (reverse ys ++ [y])
=                             (asociatividad)
(reverse ws ++ reverse ys) ++ [y]
=                              (HI)
reverse (ys ++ ws) ++ [y]

Vale para este Caso
Vale la propiedad
===========================================================================
j. para todo xs. para todo ys.
all p (xs++ys) = all p (reverse xs) && all p (reverse ys)

¿para todo xs. para todo ys.
all p (xs++ys) = all p (reverse xs) && all p (reverse ys)?

Dem: Sea una ws::[a] ts::[a]
Por ppio. de ind. en la estructura de ws es equivalente demostrar:

Caso Base: ws = [])
        ¿all p ([]++ts) = all p (reverse []) && all p (reverse ts)?

Caso ind: ws = (z:zs))
    HI) ¡all p (zs++ts) = all p (reverse zs) && all p (reverse ts)!
    TI) ¿all p ((z:zs)++ts) = all p (reverse (z:zs)) && all p (reverse ts)?


Caso Base:
DER:
all p (reverse []) && all p (reverse ts)
       ---------
=                                   (reverse.1)
all p [] && all p (reverse ts)
--------
=                                   (all.1)
True && all p (reverse ts)
-------
=                               (def de &&)
all p (reverse ts)
------------------
=                                (LEMA.4)
all p ([]++ts)
       -------
=           ((++).1)
all p ts


IZQ:
all p ([]++ts)
       -------
=           ((++).1)
all p ts

Vale para este caso

CASO IND:
IZQ:
all p ((z:zs)++ts)
=                ((++).1)
all p (z:(zs ++ ts))
=                   (all.2)
p z && all (zs ++ ts)
=                    (HI)
p z && all p (reverse zs) && all p (reverse ts)

DER:
all p (reverse (z:zs)) && all p (reverse ts)
----------------------
=                                          (LEMA.4)
all p (z:zs) && all p (reverse ts)
-------------
=                               (all.2)
p z && all p zs && all p (reverse ts)
       ---------
=                               (LEMA.4)
p z && all p (reverse zs) && all p (reverse ts)

Vale para este caso
Vale la Propiedad

===========================================================================

LEMA.4
all p (reverse ys) = all p ys 

¿para todo ys. all p (reverse ys) = all p ys?
Dem: Sea xs::[a], all p (reverse xs) = all p xs? 
Por ppio. de ind. en la estructura de ys es equivalente demostrar:

CB: xs = []
    ¿all p (reverse []) = all p []?
CI: xs=(w:ws)
    HI: ¡all p (reverse ws) = all p ws!
    TI: ¿all p (reverse (w:ws)) = all p (w:ws)?

CB
IZQ:
all p (reverse [])
=               (reverse.1)
all p []

DER:
all p []

vale para este caso

IND:
IZQ:
all p (reverse (w:ws))
=                    (reverse.2)
all p (reverse ws ++ [w])
=                      (LEMA.5)
all p (reverse ws) && all p [w]
=                            (HI)
all p ws && all p [w]
            ---------
=                    (all.2)
all p ws && p x && all p []
                    -------
=                    (all.1)
all p xs' && p x && True
=                    (&&)
all p xs' && p x 
=                 (conmutatividad)
p x && all p xs'

DER:
all p (x:xs')
=                (all.2)
p x && all p xs'

Vale para este caso
Vale la Propiedad
===========================================================================
LEMA.5
all p (ps ++ ys) = all p ps && all p ys
¿para todo ps. ys. all p (ps ++ ys) = all p ps && all p ys?

Dem: Sea una ws::[a] ts::[a]
Por ppio. de ind. en la estructura de ws es equivalente demostrar:

Caso Base: ws = [])
        ¿all p ([] ++ ts) = all p [] && all p ts?

Caso ind: ws = (z:zs))
    HI) ¡all p (zs ++ ts) = all p zs && all p ts!
    TI) ¿all p ((z:zs) ++ ts) = all p (z:zs) && all p ts?

CB:
IZQ:
all p ([] ++ ts)
        -------
=              ((++).1)
all p ts

DER:
all p [] && all p ts
--------
=               (all.1)
True && all p ts
----------------
=               ((&&))
all p ts

Vale para este caso

CI:
IZQ:
all p ((z:zs) ++ ts)
       ------------
=                   ((++).2)
all p (z : (zs ++ ts))
----------------------
=                     (all.2)
p z && all p ((++) zs ts)
=                      (HI)
p z && all p zs && all ws

DER:
all p (z:zs) && all p ts
-----------
=                       (all.2)
p z && all p zs && all p ts

Vale para este caso
Vale la propiedad


===========================================================================
k. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)
(en este caso, mostrar que no vale)

¿para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?
la propiedad es falsa.

Contraejemplo:
Sea:
    ws=[]
    zs=[1]
    ¿unzip (zip [] [1]) = ([], [1])?

I:
unzip (zip [] [1])
=                  (zip.1)
unzip []
=                   (unzip.1)
([], [])

D:
([], [1])

no se puede demuestrar, por ende la propiedad es falsa.
-----------------------------
----------Sección II---------
-----------------------------

Ejercicio 1) Dada la siguiente definición

-}
    data N = Z | S N
{-

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
-}
evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n = n
addN (S n1) n2 = S (addN n1 n2)

prodN :: N -> N -> N
prodN Z _ = Z
prodN _ Z
prodN (S n1) n2= addN (prodN n1 n2) n2

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

{-
b. demostrar las siguientes propiedades:
===========================================================================
i. para todo n1. para todo n2.
evalN (addN n1 n2) = evalN n1 + evalN n2

¿para todo n1. para todo n2.
evalN (addN n1 n2) = evalN n1 + evalN n2?

Dem: Sea una n11::N n12::N
Por ppio. de ind. en la estructura de n11 es equivalente demostrar:

Caso Base: n11 = Z)
        ¿evalN (addN Z n12) = evalN Z + evalN n12?
Caso ind: n11 = (S n13))
    HI) ¡evalN (addN n13 n12) = evalN n13 + evalN n12!
    TI) ¿evalN (addN (S n13) n12) = evalN (S n13) + evalN n12?

Caso Base:
IZQ:
evalN (addN Z n12)
       ----------
=                  (addN.1)
evalN n12

DER:
evalN Z + evalN n12
--------
=                  (evalN.1)
0 + evalN n12
---
=                  (neutro de la Suma)
evalN n12

Vale para este caso

Caso IND:
IZQ:
evalN (addN (S n13) n12)
      -----------------
=                      (addN.2)
evalN (S (addN n13 n12))
=                       (evalN.2)
1 + evalN (addN n13 n12))
    --------------------
=                       (HI)
1 + evalN n13 + evalN n12

DER:
evalN (S n13) + evalN n12
=                       (evalN.2)
1 + evalN n13 + eval n12

Vale para este Caso
Vale la Propiedad
===========================================================================
ii. para todo n1. para todo n2.
evalN (prodN n1 n2) = evalN n1 * evalN n2

¿para todo n1. para todo n2.
evalN (prodN n1 n2) = evalN n1 * evalN n2?

Dem: Sea una n11::N n12::N
Por ppio. de ind. en la estructura de n11 es equivalente demostrar:

Caso Base: n11 = Z)
        ¿evalN (prodN Z n12) = evalN Z * evalN n12?
Caso ind: n11 = (S n13))
    HI) ¡evalN (prodN n13 n12) = evalN n13 * evalN n12!
    TI) ¿evalN (prodN (S n13) n12) = evalN (S n13) * evalN n12?

Caso Base:
IZQ:
evalN (prodN Z n12)
=                  (prodN.1)
evalN Z
=                  (evalN.1)
0

DER:
evalN Z * evalN n12
=                  (evalN.1)
0  * evalN n12
=                   (0 * Int = 0)
0

Vale para este caso

Caso IND:
IZQ:
evalN (prodN (S n13) n12)
=                   (prodN.3)
evalN (addN (prodN n13 n12) n12)
=                   (LEMA6)
evalN (prodN n13 n12) + evalN n12
=                      (HI)
(evalN n13 * evalN n12) + evalN n12


DER:
evalN (S n13) * evalN n12
=                       (evalN.2)
(1 + evalN n13) * eval n12
=                        (Aritm. distributiva)
(1 * eval n12) + (evalN n13 * eval n12)
=                        (Aritm. 1 neutro)
eval n12 + (evalN n13 * eval n12)
=                        (Aritm. conmutatividad)
(evalN n13 * evalN n12) + evalN n12

LEMA6
evalN (addN a b) = evalN a + evalN b
Sean n1::N, n2::N. Qvq:

Por induccion estructural sobre n1, se vera que:
    evalN (addN n1 n2) = evalN n1 + evalN n2

Caso base, n1 = Z:
    ¿evalN (addN Z n2) = evalN Z + evalN n2?
Caso ind, n1 = S n:
    HI: ¡evalN (addN n n2) = evalN n + evalN n2!
    TI: ¿evalN (addN (S n) n2) = evalN (S n) + evalN n2?

Caso base:
Izq:
    evalN (addN Z n2)
=                (addN.1)
    eval n2

Der:
    evalN Z + evalN n2
=                    (evalN.1)
    0 + evalN n2
=                    (Aritm.)
    evalN n2

Caso ind:
Izq:
    evalN (addN (S n) n2)
=                    (addN.2)
    evalN (S (addN n n2))
=                    (evalN.2)
    1 + evalN (addN n n2)

Der:
    evalN (S n) + evalN n2
=                        (evalN.2)
    1 + evalN n + evalN n2
=                        (HI)
    1 + evalN (addN n n2)


Vale  para este Caso
Vale la propiedad
===========================================================================
iii. 
int2N . evalN = id

Por el ppio. de ext. ¿para todo n. int2N . evalN n = id n?
=                                                                     (def. (.))
 ¿para todo n. int2N (evalN n) = id n?

Dem: Sea n1:: N
Por ppio. de ind. en la estructura de n1 es equivalente demostrar:

Caso Base: n1 = Z)
        ¿int2N (evalN Z) = id Z?
Caso ind: n1 = (S n2))
    HI) ¡int2N (evalN n2) = id n2!
    TI) ¿int2N (evalN (S n2)) = id (S n2)?

Dem: 
Caso Base:
IZQ:
int2N (evalN Z)
=               (evalN.1)
int2N 0
=               (int2N.1)
Z

DER:
id Z
=   (id)
Z

Vale para este Caso

Dem:
Caso Ind:
IZQ:
int2N (evalN (S n2)) 
=                  (evalN.2)
int2N (1 + evalN n2)
=                   (int2N.2)
S (int2N ((1 + evalN n2) -1)) 
=                  (aritmetica)
S (int2N (evalN n2))
=                    (HI)
S (id n2)
=                   (id)
(S n2)

Der:
id (S n2)
=       (id)
S n2

Vale para este caso
Vale la Propiedad
===========================================================================
iv. evalN . int2N = id

Por ppio. de ext.:
para todo x.
    ¿(evalN . int2N) x = id x?
=                                (.)
    ¿evalN (int2N x) = id x?

Sea n::Int. Qvq:

Por ind. matematica sobre n:
    ¿evalN (int2N n) = id n?

Caso base, n=0:
    ¿evalN (int2N 0) = id 0?
Caso ind, n > 0:
    H1: ¡evalN (int2N (n - 1)) = id (n - 1)!
    TI: ¿evalN (int2N n) = id n?

CB:
I:
    evalN (int2N 0)
=              (int2N)
    evalN Z
=              (evalN)
    0

D:
    id 0
=            (id)
    0

Vale para este caso

CI:
I:
    evalN (int2N n)
=                   (int2N)
    evalN (S (int2N (n-1)))
=                   (evalN)
    1 + evalN (int2N (n-1))
=                   (HI)
    1 + id (n - 1)
=                    (id)
    1 + (n - 1)
=                 (aritm.)
    n

D:
    id n
=      (id)
    n

Vale para este caso, vale la propiedad
===========================================================================
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
-}
type NU = [()]

evalNU :: NU -> Int
evalNU [] = 0
evalNU (_:xs) = 1 + evalNU xs

succNU :: NU -> NU
succNU xs = () : xs

addNU :: NU -> NU -> NU
addNU [] ys = ys
addNU (():xs) ys = () : (addNU xs ys)

nu2n :: NU -> N
nu2n [] = Z
nu2n (():xs) = S (nu2n xs)

n2nu :: N -> NU
n2nu Z = []
n2nu (S n) = () : (n2nu n)
{-
b. demostrar las siguientes propiedades:
    i. evalNU . succNU = (+1) . evalNU

Por el ppio. de ext. ¿Para todo n. evalNU . succNU n = (+1) . evalNU n
=                                                                 (def. (.))
¿Para todo n. evalNU (succNU n) = (+1) (evalNU n)?

Dem: Sea una ns:: NU
Por ppio. de ind. en la estructura de ns es equivalente demostrar:

Caso Base: ns = [])
        ¿evalNU (succNU []) = (+1) (evalNU [])?
Caso ind: ns = (():zs))
    HI) ¡evalNU (succNU zs) = (+1) (evalNU zs)!
    TI) ¿evalNU (succNU (():zs)) = (+1) (evalNU (():zs))?

Caso Base:
IZQ:
evalNU (succNU []) 
=               (succNU)
evalNU [()]
=           (evalNU.2)
1 + evalNU []
=           (evalNU.1)
1 + 0
=           (aritmetica)
1


(+1) (evalNU [])
=           (evalNU.1)
1 + 0
=           (aritmetica)
1

Vale para este caso

dem Caso induccion:
IZQ:
evalNU (succNU (():zs))
=               (succNU)
evalNU (():(():zs))
=           (evalNU.2)
1 + evalNU (():zs)
=           (evalNU.2)
1 + 1 + evalNU zs)
=           (aritmetica)
2 + evalNU zs

DER:
(+1) (evalNU (():zs))
=           (evalNU.2)
(+1) (1 + evalNU zs)
=               (HI)
(+1) (1 + evalNU zs)
=           (aritmetica)
2 + evalNU zs

Vale para este caso
Vale la propiedad
===========================================================================
ii. para todo n1. para todo n2.
evalNU (addNU n1 n2) = evalNU n1 + evalNU n2

¿para todo n1. para todo n2.
evalNU (addNU n1 n2) = evalNU n1 + evalNU n2?

DEM:
Sea n11::NU y n22::NU

Por ppio. de ind. en la estructura de n1 es equivalente demostrar:

Caso Base: n11 = [])
    ¿evalNU (addNU [] n22) = evalNU [] + evalNU n22?
Caso ind: n11 = (():zs))
    HI) ¡evalNU (addNU zs n22) = evalNU zs + evalNU n22!
    TI) ¿evalNU (addNU (():zs) n22) = evalNU (():zs) + evalNU n22?


CB:
IZQ:
evalNU (addNU [] n22) 
=                   (addNU.1)
evalNU n22

DER:
evalNU [] + evalNU n22
----------
=                   (evalNU.1)
0 + evalNU n22
=                   (aritmetica)
evalNU n22

Vale para este caso

CI:
IZQ:
evalNU (addNU (():zs) n22)
=                       (addNU.2)
evalNU (() : (addNU zs n22))
=                           (evalNU.1)
1 + evalNU (addNU zs n22)
=                        (HI)
1 + evalNU zs + evalNU n22!

DER:
evalNU (():zs) + evalNU n22
--------------
=                          (evalNU.2)
1 + evalNU zs + evalNU n22


Vale para este caso
Vale la propiedad
===========================================================================
iii. nu2n . n2nu = id

Por ppio. de ext.:
para todo n.
    ¿nu2n . n2nu n = id n?
=                                (.)
    ¿nu2n (n2nu n) = id n?

Sea n1::N. Qvq:

Por ppio. de ind. en la estructura de n1 es equivalente demostrar:
    ¿nu2n (n2nu n) = id n?

Caso base, n1 = Z)
    ¿nu2n (n2nu Z) = id Z?
Caso ind, n2 = (S n3))
    H1: ¡nu2n (n2nu n3) = id n3!
    TI: ¿nu2n (n2nu (S n3)) = id (S n3)?

Caso Base:
IZQ:
nu2n (n2nu Z) 
    ----------
=            (n2nu.1)
nu2n []
=            (nu2n.1)             
Z

DER:
id Z
=   (id)
Z

Vale para este caso
Caso ind:

IZQ:
nu2n (n2nu (S n3))
=           (n2nu.2)
nu2n (() : (n2nu n3))
=           (nu2n.2)
S (nu2n (n2nu n3))
=           (HI)
S (id n3)
=           (id)
S n3

DER:
id (S n3)
=           (id)
S n3

Vale para este caso
Vale la propiedad
===========================================================================
    iv. n2nu . nu2n = id

Por ppio. de ext.:
para todo n.
    ¿n2nu . nu2n n = id n?
=                                (.)
    ¿n2nu (nu2n n) = id n?

Sea n1::NU. Qvq:

Por ppio. de ind. en la estructura de n1 es equivalente demostrar:
    ¿nu2n (n2nu n) = id n?

Caso base, n1 = [])
    ¿n2nu (nu2n []) = id []?
Caso ind, n2 = (():n3))
    H1: ¡n2nu (nu2n n3) = id n3!
    TI: ¿n2nu (nu2n (():n3)) = id (():n3)?


Caso Base:
IZQ:
n2nu (nu2n [])
=               (nu2n.1)
n2nu Z
=               (n2nu.1)
[]

DER:
id Z
=          (id)
[]

Vale para este Caso

Caso IND:
IZQ:
n2nu (nu2n (():n3))
=               (nu2m.2)
n2nu (S (nu2n n3))
=               (n2nu.2)
() : (n2nu (nu2n n3))
=                  (HI)
() : id n3
=                (id)
() : n3

DER:
id (():n3)
=               (id)
() : n3

Vale para este caso
Vale la propiedad

===========================================================================
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

-}

data DigBin = O | I
type NBin = [DigBin]

dbAsInt :: DigBin -> Int
dbAsInt I = 1
dbAsInt O = 0

dbAsBool :: DigBin -> Bool
dbAsBool I = True
dbAsBool O = False

dbOfBool :: Bool -> DigBin
dbOfBool True = I
dbOfBool False = O

negDB :: DigBin -> DigBin
negDB O = I
negDB I = O

evalNB :: NBin -> Int
evalNB []     = 0
evalNB (d:ds) = dbAsInt d + 2 * evalNB ds

normalizarNB :: NBin -> NBin
normalizarNB []     = []
normalizarNB (d:ds) = normalizarNB' d (normalizarNB ds)

normalizarNB' :: DigBin -> [DigBin] -> [DigBin]
normalizarNB' O [] = []
normalizarNB' d ds = d:ds

succNB :: NBin -> NBin
succNB []     = [I]
succNB (d:ds) = case d of
  O -> I : ds
  I -> O : succNB ds

-- PREC: n y m están normalizados
addNB :: NBin -> NBin -> NBin    -- Comparar con alguien más
addNB []     m        = m 
addNB n      []       = n
-- addNB (d:ds) (d':ds') = let (d'',c) = addDB d d'
--   in d'' : addNBConCarry (addNB ds ds') c
addNB (d:ds) (d':ds') = f d d' (addNB ds ds')

f :: DigBin -> DigBin -> NBin -> NBin
f O O n = O : n
f I I n = O : succNB n
f _ _ n = I : n

addNBConCarry :: NBin -> DigBin -> NBin
addNBConCarry n O = n
addNBConCarry n I = succNB n

addDB :: DigBin -> DigBin -> (DigBin, DigBin)
addDB I I = (O,I)
addDB O O = (O,O)
addDB _ _ = (I,O)

nb2n :: NBin -> N
nb2n []     = Z
nb2n (d:ds) = db2n d `addN` S (S Z) `prodN` nb2n ds

db2n :: DigBin -> N
db2n O = Z
db2n I = S Z

n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

{-
NO ME SALIO NAAADAAAAAAAAAAAAAAAAA
b. demostrar las siguientes propiedades:
NBin
===========================================================================
¿(evalNB . normalizarNB) n = evalNB n?
¿evalNB (normalizarNB n) = evalNB n?

Dem: Sea m::NBin. Por ppio. de ind. sobre la estructura de n, se verá que:

Caso base, m=[]: evalNB (normalizarNB []) = evalNB []
Caso ind., m=(d:ds):
  HI: ¡evalNB (normalizarNB ds) = evalNB ds!
  TI: ¿evalNB (normalizarNB (d:ds)) = evalNB (d:ds)?

Caso base IZQ:
...

Caso base DER:
...

Antes del Caso ind. cubriremos el caso particular m=(O:[]):
Caso particular m=(O:[]) IZQ:
evalNB (normalizarNB (O:[]))
evalNB (normalizarNB' O (normalizarNB []))
evalNB (normalizarNB' O [])
evalNB []

Caso particular m=(O:[]) DER:
evalNB (O:[])
dbAsInt O + 2 * evalNB []
0 + evalNB []
evalNB []

El caso particular queda demostrado ✅

Ahora sí, vamos al Caso ind. teniendo en cuenta que ya cubrimos el caso m=(O:[])
Caso ind. IZQ:
evalNB (normalizarNB (d:ds))
evalNB (normalizarNB' d (normalizarNB ds))
evalNB (d : normalizarNB ds)
dbAsInt d + 2 * evalNB (normalizarNB ds)
dbAsInt d + 2 * evalNB ds

Caso ind. DER:
evalNB (d:ds)
dbAsInt d + 2 evalNB ds

El caso ind. queda demostrado ✅
===========================================================================


-----------------------------
----------Sección III--------
-----------------------------

Ejercicio 1) Dada la siguiente representación de expresiones aritméticas
-}
    data ExpA = Cte Int
    | Suma ExpA ExpA
    | Prod ExpA ExpA
{-
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
-}
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