--Ejercicio 1) Dar las reglas que definen el conjunto inductivo correspondiente a la
--parte totalmente definida del tipo algebraico Pizza definido por:
data Pizza = Prepizza | Capa Ingrediente Pizza
                                    deriving Show
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa
                                    deriving Show
{-
Regla Base: Prepizza está en Pizza
Regla inductiva: 
    Si p esta en Pizza
    e i esta en Ingrediente,
    Capa i p esta en Pizza.
-}

--Ejercicio 2) Dar la forma esquemática de una función definida por recursión
--estructural sobre el tipo trabajado en el ejercicio anterior.

--f :: Pizza -> a
--f Prepizza = ...
--f (Capa i p) = ... i ... f p

{-
Ejercicio 3) Dar el tipo de cada una de las siguientes funciones, y definirlas utilizando
recursión estructural.
a. cantidadDeCapas, que describe la cantidad de capas de ingredientes de la
misma.
b. cantidadDeAceitunas, que describe la cantidad de aceitunas que hay en
una pizza dada.
c. duplicarAceitunas, que dada una pizza, describe otra pizza de forma tal
que se cumpla la siguiente propiedad:
            para todo p.
                cantidadDeAceitunas (duplicarAceitunas p)
                    = 2 * cantidadDeAceitunas p
d. sinLactosa, que describe la pizza resultante de remover todas las capas de
queso de una pizza dada.
e. aptaIntolerantesLactosa, que indica si la pizza dada no tiene queso, o
sea se cumple la siguiente propiedad:
    para todo p.
        si aptaIntolerantesLactosa p = True
            entonces p = sinLactosa p
f. conDescripcionMejorada, que toma una pizza y otra que se construyó
con exactamente los mismos ingredientes pero donde no se agregan
aceitunas dos veces seguidas.
-}

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) =  1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = cantDeAceitunas i + cantidadDeAceitunas p

cantDeAceitunas :: Ingrediente -> Int
cantDeAceitunas (Aceitunas n) = n
cantDeAceitunas _ = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarAceitunasSiTiene i) (duplicarAceitunas p)

duplicarAceitunasSiTiene :: Ingrediente -> Ingrediente
duplicarAceitunasSiTiene (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunasSiTiene i = i

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = case i of
        Queso -> sinLactosa p
        _ -> Capa i (sinLactosa p)

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa i p) = case i of 
                        Queso -> False
                        _ -> aptaIntolerantesLactosa p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = unirAceitunas i (conDescripcionMejorada p)

unirAceitunas :: Ingrediente -> Pizza -> Pizza
unirAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
unirAceitunas i p = Capa i p

{-
Ejercicio 4) Demostrar las siguientes propiedades:
a. cantidadDeAceitunas Prepizza
    = cantidadDeAceitunas(conDescripcionMejorada Prepizza)
b. cantidadDeAceitunas (Capa Queso Prepizza)
    = cantidadDeAceitunas(conDescripcionMejorada (Capa Queso Prepizza))
c. cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
                = cantidadDeAceitunas
                            (conDescripcionMejorada
                                    (Capa (Aceitunas 8)
                                            (Capa Queso Prepizza)))
d. cantidadDeAceitunas (Capa (Aceitunas 9)
                            (Capa (Aceitunas 8)
                                (Capa Queso Prepizza)))
= cantidadDeAceitunas
    (conDescripcionMejorada
        (Capa (Aceitunas 9)
            (Capa (Aceitunas 8)
                (Capa Queso Prepizza))))
-}

{-
a. 
cantidadDeAceitunas(conDescripcionMejorada Prepizza)
                                    (def conDescripcionMejorada)
cantidadDeAceitunas Prepizza

b.
cantidadDeAceitunas(conDescripcionMejorada (Capa Queso Prepizza))
                                        (def conDescripcionMejorada)
cantidadDeAceitunas (Capa Queso Prepizza)

c.
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
                                                            (def conDescripcionMejorada)
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))

d.
cantidadDeAceitunas (Capa (Aceitunas 9)(Capa (Aceitunas 8)(Capa Queso Prepizza)))
=                                                                (def cantDeAceitunas)
17


cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
=                                                               (def conDescripcionMejorada)
cantidadDeAceitunas (Capa (Aceitunas 17) (Capa Queso Prepizza))
=
17




-}