{-
Demostrar que
a. Para todo f, subst const f = id
b. curry fst = const

a.
Por el principio de extensionalidad. ¿Para todo y. Para todo f, subst const f y = id y?
    Sea x un elemento de tipo cualquiera y g una funcion cualquiera . subst const g x = id x
 
IZQ:
subst const g x 
=                       (def subst)
(const x) (g x)    
=                       (def const)
g


DER:
id x
=                       (def de id)
x

subst const f y = id y! son lo mismo

b.

Por el principio de extensionalidad. ¿Para todo a para todo b. curry fst a b = const a b?
    Sea x e y elementos de tipo cualquiera. curry fst x y = const x y

izq:
curry fst x y
=             (def curry)
fst(x,y)
=             (def fst)
x


der:
const x y
=             (def const)
x

curry fst a b = const a b son lo mismo

Dar las definiciones iniciales a los siguientes conjuntos

a) El conjunto ABs formado por secuencias no vacias de a y b
ejs: a, b, ab, ba, aa, aab, bb, bba, ...

Regla Base: a tiene que estar en el ABs
            b tiene que estar en el ABs
Regla inductivas:
            si d esta en el ABs, entonces bd tiene que estar
            si d esta en el ABs, entonces ad tiene que estar

b) El conjunto BAs formado por secuencias no vacias de a y b que terminan en a
ejs: a, ba, aa, baa, bba, ...

Regla Base: 
        a tiene que estar en el BAs

Regla inductivas:
            si d esta en el BAs, entonces da tiene que estar
            si d esta en el BAs, entonces bd tiene que estar

c) El conjunto ABBs formado por secuencias no vacias de a y b, tal que cada 
vez que agruegan letra b consecutivas, es un grupo de longitud par de b
ejs: a, abb. bb. bba, bbabbbb, aabbbbaaabb, ...

Regla Base: a tiene que estar en el ABBs
            bb tiene que estar en el ABBs
Regla inductivas:
            si d esta en el ABBs, entonces dbb tiene que estar
            si d esta en el ABBs, entonces ad tiene que estar
            si d esta en el ABBs, entonces da tiene que estar

2) Dar un tipo algebraico en Haskell, que separe la estructura de los ejemplos
del ejercicio 1

a) data ABs = BaseA | BaseB | ConA ABs | ConB ABs

b) data BAs = BaseA | ConA BAs | ConB BAs
         
c) data ABBs = BaseA | BaseBB | ACon ABBs | ConBB ABBs | ConA ABBs   

3) Dar elementos de cada uno de los conjuntos del ejercicio 2, que representan la
estructura de cada uno de los ejemplos del ejercicio 1:


a. a, b, ab, ba, aa, aab, bb, bba  
estrABs_a = BaseA
estrABs_b = BaseB
estrABs_ab = ConA BaseB
estrABs_ba = 

b. a, ba, aa, baa, bba
.
.
.

c. a, abb. bb. bba, bbabbbb, aabbbbaaabb, 
.
.
.

4) Dar funciones que dado un elemento de cada uno de los tipos del g2, describan
el String correspondiente del ejercicio 1

a) 
printABs :: ABs -> String
printABs BaseA = "a"
printABs BaseB = "b"
printABs (ConA r) = "a" ++ r
printABs (ConB r) = "b" ++ r


b) 
printBAs :: BAs -> String
printBAs BaseA = "a"
printBAs ConA r = "a" ++ printBAs r 
printBAs ConB r = printBAs r ++ "b"

c) 
printABBs :: ABBS -> String
printABBs BaseA = "a"
printABBs BaseBB = "bb"
printABBs (ACon r) = "a" ++ r
printABBs (ConA r) = r ++ "a"
printABBs (ConBB r) = r ++ "bb"

Ej: printABs estrABs_a = "a"
    printABBs estrABBs_abb = "abb"
-}