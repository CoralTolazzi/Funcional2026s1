{-Ejercicio 1) Demostrar las siguientes propiedades:
    a. doble = \x -> 2 * x
    Por el principio de extensionalidad. Para todo x. doble n = (\x -> 2 * x) n
    Sea n un numero cualquiera. Se verá que que doble e = (\x -> 2 * x) e

        doble e
    =           (def. doble)
        e + e
    =           (aritmetica)
        2 * e
    =           (Beta Red.)
        (\x -> 2 * x ) e

    doble n = (\x -> 2 * x) n son lo mismo porque partiendo de un lado se puede llegar al otro

    b. compose doble doble = cuadruple
    Por el principio de extensionalidad. Para todo x.  compose doble doble n = cuadruple n
    Sea n un numero cualquiera. Se verá que que compose doble doble e = cuadruple e

        compose doble doble e           
    =                            (def. compose)
        doble (doble e) 
    =                            (def. doble)
        doble e + doble e
    =                            (def. doble)
        e + e + e + e
    =                            (aritmetica)
        e * 4
    =                            (def. cuadruple)
        cuadruple e
    
    compose doble doble n = cuadruple n son lo mismo porque partiendo de un lado se puede llegar al otro

-}  
{-
Ejercicio 2) Demostrar las siguientes propiedades:
    a. para todo x. para todo y. x && y = not ((not x) || (not y))
        Caso y = False
        Sea b::Bool, se verá que: b && False = not ((not b) || (not False|))

    Lado der:
        not ((not b) || (not False))
        =                           (def not)
        not ((not b) || True))
        =                           (def Or)
        not True
        =                           (def not)
        False 

    Lado izq:
        b && False
        =                           (def And)
        False

    not ((not b) || (not False|)) son lo mismo porque partiendo de un lado se puede llegar al otro

    Caso y = True
    Sea b::Bool, se verá que: b && True = not ((not b) || (not True|))

    Lado der:
        not ((not b) || (not True))
        =                           (def not)
        not ((not b) || False))
        =                           (def Or)
        not (not b)
        =                           (propiedad involutiva)
        b
    
    Lado izq:
        b && True
        =                           (def and)
        b

   b && True = not ((not b) || (not True|)) son lo mismo porque partiendo de un lado se puede llegar al otro

    b. para todo x. para todo y. not (x || y) = not x && not y

    Caso y = False
    Sea b::Bool, se verá que: not (b || False) = not b && not False
    
    Lado der: 
    not b && not False
    =                  (def de not)
    not b && True 
    =                  (def de and)
    not b

    Lado izq:
    not (b || False)
    =               (def de Or)
    not b

    not (b || False) = not b && not False son lo mismo porque partiendo de un lado se puede llegar al otro
  
    Caso y = True
    Sea b::Bool, se verá que: not (b || True) = not b && not True
    
    Lado der: 
    not b && not True
    =               (def de Not)
    not b && False
    =               (def de And)
    False

    Lado izq:
    not (b || True)
    =               (def de Or)
    not True
    =               (def de Not)
    False
    
    not (b || True) = not b && not True son lo mismo porque partiendo de un lado se puede llegar al otro

-}
{-
Ejercicio 3) Demostrar las siguientes propiedades:
    a. curry suma' = suma
    Por el principio de extensionalidad. Para todo x Para todo y.  curry suma' n1 n2 = cuadruple n1 n2 
    Sea n un numero cualquiera. Se verá que que curry suma' a b = suma a b 

    curry suma' a b
   =                (def de curry)
    suma' (a b)
   =                (def de suma')
    a + b
   =                (def de suma)
    suma a b
    
    curry suma' a b = suma a b son lo mismo porque partiendo de un lado se puede llegar al otro
    
Por el principio de extensionalidad. Para todo x Para todo y.  curry suma' n1 n2 = cuadruple n1 n2 
Sea n un numero cualquiera. Se verá que uncurry suma (a,b) = suma' (a,b)
    
    uncurry suma (a,b) 
   =                (def de uncurry)
   suma a b
   =                (def de suma)
   a + b
   =                (def de suma')
    suma' (a,b)

uncurry suma (a,b) = suma' (a,b) son lo mismo porque partiendo de un lado se puede llegar al otro

-}
{-
Ejercicio 4) Demostrar las siguientes propiedades:

Por el principio de extensionalidad. Para todo x Para todo y.  curry fst x y = const x y
Sea x e y elementos de tipo cualquiera. Se verá que curry fst a b = const a b
 
    curry fst a b 
   =                (def de curry)
   fst (a,b)
   =                (def de fst)
   a
   =                (def de const)
    const a b
    
curry fst a b = const a b son lo mismo porque partiendo de un lado se puede llegar al otro


Por el principio de extensionalidad. Para todo x Para todo y.  uncurry (flip const) (x,y) = snd (x,y) 
 Sea x e y elementos de tipo cualquiera. Se verá que uncurry (flip const) (a,b) = snd (a,b) 
    
    uncurry (flip const) (a,b)
=                (def de uncarry)
    flip const a b
=                (def de flip)
    const b a
=                (def de const)
    b
=                (def de snd)
    snd (a,b)


uncurry (flip const) (a,b) = snd (a,b) son lo mismo porque partiendo de un lado se puede llegar al otro


-}
{-
Ejercicio 5) Demostrar las siguientes propiedades:
    a. para todo f. curry (uncurry f) = f
Por el principio de extensionalidad. Para todo x Para todo y. curry (uncurry f) x y  = f x y
    Sea x e y elementos de tipo cualquiera. Se verá que curry (uncurry f) a b = f a b
    curry (uncurry f) a b
=          (def de curry)
    uncurry f (a,b)
=            (def de uncurry)
    f a b
    
curry (uncurry f) a b = f a b son lo mismo porque partiendo de un lado se puede llegar al otro


    b. para todo f'. uncurry (curry f') = f'
Por el principio de extensionalidad. Para todo x Para todo y. uncurry (curry f') (x,y) = f' (x,y)
Sea x e y elementos de tipo cualquiera. Se verá que uncurry (curry f') (a,b) = f' (a,b)

    uncurry (curry f') (a,b) 
=           (def de uncurry)
    curry f' a b
=          (def de curry)
    f' (a,b)

uncurry (curry f') (a,b) = f' (a,b) son lo mismo porque partiendo de un lado se puede llegar al otro
-}
{-
Ejercicio 6) Dadas las siguientes definiciones
    assoc :: (a,(b,c)) -> ((a,b),c)
    assoc (x,(y,z)) = ((x,y),z)
    appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d
    appAssoc f p = f (assoc p)

demostrar la siguiente propiedad:
para todo f.
appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)
AYUDA: de necesitar el principio de extensionalidad, considerar usar el argumento
en la forma (x,(y,z)).
Sea x y z elementos de tipo cualquiera. Se verá que appAssoc (uncurry (uncurry f)) (a,(b,c)) = uncurry (compose uncurry f) (a,(b,c))
    
    appAssoc (uncurry (uncurry f)) (a,(b,c))
=                                           (def appAssoc)
    (uncurry (uncurry f)) (assoc (a,(b,c))
=                                           (def assoc)
    uncurry (uncurry f) ((a,b) c)
=                                           (def uncurry)
    (uncurry f (a,b)) c
=                                           (def uncurry)
    f a b c

    uncurry (compose uncurry f) (a,(b,c))
=                                           (def uncurry)
    (compose uncurry f a) (b,c)
=                                           (def compose)
    uncurry (f a) (b,c)
=                                           (def uncurry)
    f a b c

appAssoc (uncurry (uncurry f)) (a,(b,c)) = uncurry (compose uncurry f) (a,(b,c)) son lo mismo porque partiendo de un lado se puede llegar al otro


-}
{-
Ejercicio 7) Dada la siguiente definición
(f . g) x = f (g x)
a. definir las siguientes funciones utilizando el operador (.) y la menor
cantidad de parámetros posible:

cuadruple :: Int -> Int
cuadruple = (doble . doble)
cuadruple x = (doble . doble) x
cuadruple x = (compose doble doble) x
cuadruple x = compose doble doble x
cuadruple x = doble (doble x)

doble :: Int -> Int
doble =  (*2) . id

twice :: (a -> a) -> (a -> a)
twice f = f . f

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = f . many (n-1) f

Ayuda: pueden utilizarse como definidas las funciones succ, doble,
const, id, subst, dup, etc.
-}

{-
b. demostrar las siguientes propiedades:
i. para todo f. para todo g. f . g = compose f g
Por el principio de extensionalidad. Para todo x. f . g x = compose f g x
Sea x un elemento de tipo cualquiera. Se verá que  f . g a = compose f g a
    (f . g) a
=              (def (.))
    f (g a)
=              (def compose)
    compose f g a


f . g a = compose f g a son lo mismo porque partiendo de un lado se puede llegar al otro


ii. swap . swap = id
Por el principio de extensionalidad. Para todo x y para todo y. swap . swap (x,y) = id (x,y)
Sea x e y elementos de tipo cualquiera. Se verá que  swap . swap (a,b) = id (a,b)
    (swap . swap) (a,b)
=                      (def de (.))
    swap (swap (a,b))
=                      (def de swap)
    swap (b,a)
=                      (def de swap)
    (a,b)
=                      (def de id)
    id (a,b)

swap . swap (a,b) = id (a,b) son lo mismo porque partiendo de un lado se puede llegar al otro

iii. para todo f. para todo g. para todo h.
    f . (g . h) = (f . g) . h
Por el principio de extensionalidad. Para todo x f . (g . h) x = (f . g) . h x
Sea x un elemento de tipo cualquiera. Se verá que f . (g . h) a = (f . g) . h a
    (f . (g . h)) a
=               (def (.))
    f ((g . h) a)
=               (def (.))
    f (g (h a))
=               (def (.))
    (f . g) (h a)
=               (def (.))
    ((f . g) . h) a

f . (g . h) a = (f . g) . h a son lo mismo porque partiendo de un lado se puede llegar al otro

iv. curry . uncurry = id
Por el principio de extensionalidad. Para Para todo f . (curry . uncurry) f  = id f 
Sea f una funcion . Se verá que (curry . uncurry) g = id g 
    (curry . uncurry) g
=                           (def de (.))
    curry (uncurry g) 
=                           (Lema)       
    g
=                   (def id)
    id g 

(curry . uncurry) g = id g son lo mismo porque partiendo de un lado se puede llegar al otro

v. para todo f. appAssoc f = f . assoc
Por el principio de extensionalidad. Para todo x y. para todo f. P. appAssoc f (x,(y,z)) = f . assoc (x,(y,z)
Sea x y z elementos de tipo cualquiera. Se verá que appAssoc f (a,(b,c)) = f . assoc (a,(b,c)

    appAssoc f (a,(b,c))
=                   (def appAssoc)
    f (assoc (a,(b,c))
=                   (def (.))
    f . assoc (a,(b,c))

appAssoc f (a,(b,c)) = f . assoc (a,(b,c) son lo mismo porque partiendo de un lado se puede llegar al otro

-}
{-
c. demostrar las siguientes propiedades solamente mediante otras propiedades
ya demostradas (sin utilizar el principio de extensionalidad ni las definiciones
de las funciones directamente):
i. doble . doble = cuadruple
    doble . doble 
=               (def (.))
    doble doble
=               (def cuadruple)
    cuadruple

doble . doble = cuadruple son lo mismo porque partiendo de un lado se puede llegar al otro

ii. para todo f'. curry (uncurry (curry f')) = curry f'
    curry (uncurry (curry f'))
    =                           (Lema 7)
    curry f'

iii. para todo f. appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc

    appAssoc (uncurry (uncurry f))
    =                               (Lema)
    (uncurry (uncurry f)) . assoc


iv. para todo f. (uncurry . uncurry) f . assoc = uncurry (uncurry . f)
    (uncurry . uncurry) f . assoc
=                                  (Lema 7 iii)
    appAssoc (uncurry (uncurry f))
=                                   (lema 6)
    uncurry (compose uncurry f)
=                                   (compose = (.))
    uncurry (uncurry . f)

-}    
