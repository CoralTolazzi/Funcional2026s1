{------------------------------------
----------------SECCION I------------
-------------------------------------

Ejercicio 1) Dar las reglas que definen el conjunto inductivo correspondiente a la
parte totalmente definida del tipo algebraico Pizza definido por:

data Pizza = Prepizza | Capa Ingrediente Pizza
                                    deriving Show

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa
                                    deriving Show

Regla Base: Prepizza pertenece a Pizza
Regla inductiva:
    Si p pertenece a Pizza
    Si i pertenece a Ingrediente,
    Capa i p pertenece a Pizza.

Ejercicio 2) Dar la forma esquemática de una función definida por recursión
estructural sobre el tipo trabajado en el ejercicio anterior.

f :: Pizza -> a
f Prepizza = ...
f (Capa i p) = ... i ... f p ...

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

a. 

parto del lado derecho y llego al izquierdo
cantidadDeAceitunas(conDescripcionMejorada Prepizza)
=                                   (def conDescripcionMejorada.1)
cantidadDeAceitunas Prepizza

Conclusion cantidadDeAceitunas(conDescripcionMejorada Prepizza) = cantidadDeAceitunas Prepizza


b.
parto del lado derecho y llego al lado izquierdo
cantidadDeAceitunas(conDescripcionMejorada (Capa Queso Prepizza))
=                                        (def conDescripcionMejorada.2)
cantidadDeAceitunas (unirAceitunas Queso Prepizza)
=                                        (def unirAceitunas.2)
cantidadDeAceitunas (Capa Queso Prepizza)



Conclusion cantidadDeAceitunas(conDescripcionMejorada (Capa Queso Prepizza)) = cantidadDeAceitunas (Capa Queso Prepizza)

c.

parto del lado derecho y llego al lado izquierdo
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
=                                                           (def conDescripcionMejorada.2)
cantidadDeAceitunas (unirAceitunas (Aceitunas 8) (Capa Queso Prepizza))
=                                                            (def unirAceitunas.2)
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))

Conclusion: cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))) = 
    cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))


d.
izq:
cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
=                                                                (def cantidadDeAceitunas.2)
cantDeAceitunas (Aceitunas 9) + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
=                                                                (def cantDeAceitunas.1) 
9  + 8
=                                                                  (aritmetica)
17


der:
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
=                                                               (def conDescripcionMejorada.2)
cantidadDeAceitunas (Capa (Aceitunas 17) (Capa Queso Prepizza))
=
17

Conclusion: cantidadDeAceitunas (Capa (Aceitunas 9)
                            (Capa (Aceitunas 8)
                                (Capa Queso Prepizza)))
= cantidadDeAceitunas
    (conDescripcionMejorada
        (Capa (Aceitunas 9)
            (Capa (Aceitunas 8)
                (Capa Queso Prepizza))))


-}

--------------------------------------
----------------SECCION II------------
--------------------------------------
{-
Ejercicio 1) Dar las reglas que definen el conjunto inductivo correspondiente a la
parte totalmente definida de los tipos algebraicos Planilla y Equipo dados por las
siguientes definiciones:
-}
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla
                                    deriving Show
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo
                                    deriving Show

{-
Regla Base: Fin esta en Planilla
            Becario n esta en Equipo
Regla inductiva:
            Si p está en Planilla
            entonces Registro Nombre p está en Planilla
            Si e1, e2 y e3 están en Equipo
            entonces Investigador Nombre e1 e2 e3 está en Equipo

Ejercicio 2) Dar la forma esquemática de una función definida por recursión
estructural sobre cada uno de los tipos trabajados en el ejercicio anterior. 

        f :: Planilla -> a
        f Fin = ...
        f (Registro n p) = ... f p ...

        f' :: Equipo -> a
        f' (Becario n) = ...
        f' (Investigador n e1 e2 e3) = ... f' e1 ... f' e2 ... f' e3 ...

Ejercicio 3) Dar el tipo de cada una de las siguientes funciones, y definirlas utilizando
recursión estructural.
a. largoDePlanilla, que describe la cantidad de nombres en una planilla dada.
b. esta, que toma un nombre y una planilla e indica si en la planilla dada está el nombre dado.
c. juntarPlanillas, que toma dos planillas y genera una única planilla con los registros de ambas planillas.
d. nivelesJerarquicos, que describe la cantidad de niveles jerárquicos de un equipo dado.
e. cantidadDeIntegrantes, que describe la cantidad de integrantes de un equipo dado.
f. planillaDeIntegrantes, que describe la planilla de integrantes de un equipo dado.
-}
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta _ Fin = False
esta n (Registro n1 p) = n == n1 || esta n p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p = p
juntarPlanillas p Fin = p
juntarPlanillas (Registro n p) p1 = Registro n (juntarPlanillas p p1)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario _) = 1
nivelesJerarquicos (Investigador _ e1 e2 e3) = max (nivelesJerarquicos e1) (max (nivelesJerarquicos e2) (nivelesJerarquicos e3))

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _) = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = juntarPlanillas (Registro n (planillaDeIntegrantes e1))
                                    (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3))
{-
Ejercicio 4) Demostrar las siguientes propiedades:
a. para todo p :: Planilla 
            largoDePlanilla (juntarPlanillas Fin p)
            = largoDePlanilla Fin + largoDePlanilla p
b. para todo p :: Planilla  
                    largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p)
                    = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
c. para todo p :: Planilla  
            largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
            = largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
d. para todo p :: Planilla
            largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
            = largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p

a.
 ¿para todo p :: Planilla. largoDePlanilla (juntarPlanillas Fin p)
            = largoDePlanilla Fin + largoDePlanilla p?
Sea g, se verá que: ¡largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) g)
                    = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla g !

izq:
largoDePlanilla (juntarPlanillas Fin g)
=                                (def juntarPlanillas.1)
largoDePlanilla g

der:
largoDePlanilla Fin + largoDePlanilla p
                                (def de largoDePlanilla.1 para el primer largoDePlanilla)
0 + largoDePlanilla g
                                (aritmetica)
largoDePlanilla g

Conclusion: largoDePlanilla (juntarPlanillas Fin p)
            = largoDePlanilla Fin + largoDePlanilla p

b. 
¿para todo p :: Planilla  
                    largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p)
                    = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p?
Sea g, se verá que: ¡largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) g)
                    = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla g !

izq:
largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) g)
=                                            (def juntarPlanillas.3)
largoDePlanilla (Registro "Edsger" (juntarPlanillas Fin g) 
=                                            (def juntarPlanillas.1)
largoDePlanilla (Registro "Edsger"  g) 
=                                            (def largoDePlanilla.2)
1 + largoDePlanilla g

der:
largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla g
=                                            (def largoDePlanilla en el primero)
1 + largoDePlanilla Fin + largoDePlanilla g
=                                            (def largoDePlanilla.1 en largoDePlanilla Fin)
1 + largoDePlanilla g

Conclusion: largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p)
                    = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p

c.
¿para todo p :: Planilla  
            largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
            = largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p?
Sea g, se verá que: ¡largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) g)
            = largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla g!

izq:
largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) g)
=                                            (def juntarPlanillas.3)
largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) (g)))
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin g)))
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alan" (Registro "Edsger" g))
=                                              (def largoDePlanilla.2)
1 + largoDePlanilla (Registro "Edsger" g)
=                                              (def largoDePlanilla.2)
1 + 1 + largoDePlanilla g
=                                              (def largoDePlanilla.2)
2 + largoDePlanilla g

der:
largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla g
=                                                            (def largoDePlanilla.2)
1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla g
=                                                            (def largoDePlanilla.2)
1 + 1 + largoDePlanilla Fin + largoDePlanilla g
=                                                            (def largoDePlanilla.1)
1 + 1 + 0 + largoDePlanilla g
=                                                            (aritmetica)                                              
2 + largoDePlanilla g

Conclusion: largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
            = largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p


d.
¿para todo p :: Planilla
            largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
            = largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p?
Sea g, se verá que: ¡ largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) g)
            = largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla g!

izq:
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) g)
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) g)
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) g)
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) g))
=                                              (def juntarPlanillas.3)
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin g))))
=                                              (def juntarPlanillas.1)
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" g)))
=                                              (def largoDePlanilla.2)
 1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" g))
=                                              (def largoDePlanilla.2)
 1 + 1 + largoDePlanilla (Registro "Edsger" g)
=                                              (def largoDePlanilla.2)
 1 + 1 + 1 + largoDePlanilla g
=                                              (aritmetica)
3 + largoDePlanilla g


der:
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla g
=                                                                    (def largoDePlanilla.2)
1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla g
=                                                                    (def largoDePlanilla.2)
1 + 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla g
=                                                                    (def largoDePlanilla.2)
1 + 1 + 1 + largoDePlanilla Fin + largoDePlanilla g
=                                                                    (def largoDePlanilla.1)
1 + 1 + 1 + 0 + largoDePlanilla g
=                                                                    (aritmetica)
3 + largoDePlanilla g

Conclusion: largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
            = largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p


Ejercicio 5) Demostrar las siguientes propiedades:
a. largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) = cantidadDeIntegrantes (Becario "Alan")
b. largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) = cantidadDeIntegrantes (Becario "Brian")
c. para todo n :: Nombre . largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)
d. largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) 
   = cantidadDeIntegrantes(Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
e. para todo n :: Nombre . para todo n1 :: Nombre . para todo n2 :: Nombre . para todo n3 :: Nombre .
largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))) = cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
f. largoDePlanilla 
    (planillaDeIntegrantes 
        (Investigador "Oswald" 
            (Investigador "Alonzo" (Becario "Alan") 
                                   (Becario "Alfred") 
                                   (Becario "Stephen"))
            (Investigador "John" (Becario "Brian")
                                    (Becario "Graham")
                                    (Becario "Ioan"))
            (Investigador "Robert" (Becario "Gordon")
                                    (Becario "John")
                                    (Becario "Raymond"))))
    = cantidadDeIntegrantes
        (Investigador "Oswald"
            (Investigador "Alonzo" (Becario "Alan")
                                   (Becario "Alfred")
                                   (Becario "Stephen"))
            (Investigador "John" (Becario "Brian")
                                    (Becario "Graham")
                                    (Becario "Ioan"))
            (Investigador "Robert" (Becario "Gordon")
                                    (Becario "John")
                                    (Becario "Raymond"))))

a. 

izq:
largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) 
=                                            (def planillaDeIntegrantes.1)
largoDePlanilla (Registro "Alan" Fin)
=                                            (def  largoDePlanilla.2)
1 + largoDePlanilla fin
=                                            (def  largoDePlanilla.1)
1 + 0
=                                            (aritmetica)
1


der:
cantidadDeIntegrantes (Becario "Alan")
=                                            (def cantidadDeIntegrantes)
1

Conclusion: largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) = cantidadDeIntegrantes (Becario "Alan")
 
b. 
izq:
largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) 
=                                            (def planillaDeIntegrantes.1)
largoDePlanilla (Registro "Brian" Fin)
=                                            (def  largoDePlanilla.2)
1 + largoDePlanilla Fin
=                                            (def  largoDePlanilla.1)
1 + 0
=                                            (aritmetica)
1

der:
cantidadDeIntegrantes (Becario "Brian")
=                                            (def cantidadDeIntegrantes.1)
1

Conclusion: largoDePlanilla (planillaDeIntegrantes (Becario "Brian"))
 = cantidadDeIntegrantes (Becario "Brian")


c. 
¿para todo n :: Nombre . largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)?
Sea g, se verá que: ¡largoDePlanilla (planillaDeIntegrantes (Becario g)) = cantidadDeIntegrantes (Becario g)!

izq:
largoDePlanilla (planillaDeIntegrantes (Becario g)) 
=                                    (def planillaDeIntegrantes.1)
largoDePlanilla (Registro n Fin)
=                                    (def largoDePlanilla.2)
1 + largoDePlanilla Fin
=                                    (def largoDePlanilla.1)
1 + 0
=                                    (aritmetica)
1

der:
cantidadDeIntegrantes (Becario g)
=                                            (def cantidadDeIntegrantes.1)
1

Conclusion: largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)

d. largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) 
   = cantidadDeIntegrantes(Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))

Parto de la izq:
largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) 
=                                                                                   (def planillaDeIntegrantes.2)
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (planillaDeIntegrantes (Becario "Alan")) (juntarPlanillas (planillaDeIntegrantes (Becario "Alfred")) (planillaDeIntegrantes (Becario "Stephen"))) 
=                                                                                   (def planillaDeIntegrantes.1)
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" Fin)) (juntarPlanillas (Registro "Alfred" Fin) (Registro "Stephen" Fin))) 
=                                                                                   (def juntarPlanillas.3)
largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" Fin)) (Registro "Alfred" (juntarPlanillas(Registro "Stephen" Fin) Fin)))
=                                                                                   (def juntarPlanillas.2)
largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" Fin) (Registro "Alfred" (Registro "Stephen" Fin)))
=                                                                                   (def juntarPlanillas.3)
largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin))))
=                                                                                   (def largoDePlanilla.2)
1 + largoDePlanilla (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
=                                                                                   (def largoDePlanilla.2)
1 + 1 + largoDePlanilla (Registro "Alfred" (Registro "Stephen" Fin))
=                                                                                   (def largoDePlanilla.2)
1 + 1 + 1 + largoDePlanilla (Registro "Stephen" Fin)
=                                                                                   (def largoDePlanilla.2)
1 + 1 + 1 + 1 largoDePlanilla  Fin
=                                                                                   (def largoDePlanilla.1)
1 + 1 + 1 + 1 + 0
=                                                                                   (aritmetica)
4


der:
cantidadDeIntegrantes(Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
=                                                                                   (def cantidadDeIntegrantes.3)
1 + cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
=                                                                                   (def cantidadDeIntegrantes.3)
1 + cantidadDeIntegrantes (Becario "Alan")  + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
=                                                                                   (def cantidadDeIntegrantes.1)
1 + 1 + 1 + 1
=                                                                                   (aritmetica)
4

Conclusion: largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) 
   = cantidadDeIntegrantes(Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))


f. 
largoDePlanilla 
    (planillaDeIntegrantes 
        (Investigador "Oswald" 
            (Investigador "Alonzo" (Becario "Alan") 
                                   (Becario "Alfred") 
                                   (Becario "Stephen"))
            (Investigador "John" (Becario "Brian")
                                    (Becario "Graham")
                                    (Becario "Ioan"))
            (Investigador "Robert" (Becario "Gordon")
                                    (Becario "John")
                                    (Becario "Raymond"))))

=                                                (def planillaDeIntegrantes)
largoDePlanilla  
        Registro "Oswald" 
        (Registro "Alonzo" 
        (Registro "Alan" 
        (Registro "Alfred" 
        (Registro "Stephen" 
        (Registro "John" 
        (Registro "Brian" 
        (Registro "Graham" 
        (Registro "Ioan" 
        (Registro "Robert" 
        (Registro "Gordon" 
        (Registro "John" 
        (Registro "Raymond" Fin))))))))))))
=                                                (def largoDePlanilla)
13

der:
cantidadDeIntegrantes
        (Investigador "Oswald"
            (Investigador "Alonzo" (Becario "Alan")
                                   (Becario "Alfred")
                                   (Becario "Stephen"))
            (Investigador "John" (Becario "Brian")
                                    (Becario "Graham")
                                    (Becario "Ioan"))
            (Investigador "Robert" (Becario "Gordon")
                                    (Becario "John")
                                    (Becario "Raymond"))))

=                                                    (def cantidadDeIntegrantes)
13


--------------------------------------
----------------SECCION III------------
--------------------------------------

Ejercicio 1) Dar las reglas que definen el conjunto inductivo correspondiente a la
parte totalmente definida del tipo algebraico Dungeon a definido por:
-}
data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)
                                                                                                deriving Show

{-
regla base: Si x :: a
            Habitacion x esta Dungeon a
regla inductiva 1:
                Si m esta en Maybe
                Si d esta en Dungeon,
                Pasaje m d esta en Dungeon

regla inductiva 2:
                Si m esta en Maybe
                Si d1 d2 esta en Dungeon
                Bifurcacion m d1 d2 esta en Dungeon

Ejercicio 2) Dar la forma esquemática de una función definida por recursión
estructural sobre el tipo trabajado en el ejercicio anterior.

f :: Dungeon -> a
f (Habitacion x) = ... 
f (Pasaje m d) = ... m ... f d ...
f (Bifurcacion m d1 d2) ... m ... f d1 ... f d2 ...

Ejercicio 3) Dar el tipo de cada una de las siguientes funciones, y definirlas utilizando
recursión estructural.
a. cantidadDeBifurcaciones, que describe la cantidad de bifurcaciones de
un dungeon dado.
b. cantidadDePuntosInteresantes, que describe la cantidad de puntos
interesantes de un dungeon dado. Los puntos interesantes son los lugares
donde puede aparecer un elemento.
c. cantidadDePuntosVacios, que describe la cantidad de puntos
interesantes del dungeon dado en las que no hay ningún elemento.
d. cantidadDePuntosCon, que dado un elemento y un dungeon, describe la
cantidad de puntos interesantes del dungeon en las que se encuentra el
elemento dado.
e. esLineal, que indica si no hay bifurcaciones en un dungeon dado.
f. llenoDe, que dado un elemento y un dungeon, indica si el elemento se
encuentra en todas las posiciones del dungeon.
-}

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion x) = 1
cantidadDeBifurcaciones (Pasaje m d) = 1 + cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion x) = 1
cantidadDePuntosInteresantes (Pasaje m d) = case m of
                                        Just x -> 1 + cantidadDePuntosInteresantes d
                                        Nothing -> cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = case m of
                                        Just x -> 1 + cantidadDePuntosInteresantes d1 + 
                                                        cantidadDePuntosInteresantes d2
                                        Nothing -> cantidadDePuntosInteresantes d1 + 
                                                    cantidadDePuntosInteresantes d2

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion x) = 0
cantidadDePuntosVacios (Pasaje m d) = case m of
                                        Just x -> cantidadDePuntosVacios d
                                        Nothing -> 1 + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = case m of
                                        Just x -> cantidadDePuntosVacios d1 + 
                                                        cantidadDePuntosVacios d2
                                        Nothing -> 1 + cantidadDePuntosVacios d1 + 
                                                    cantidadDePuntosVacios d2



cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x (Habitacion y) = if x == y then 1 else 0
cantidadDePuntosCon x (Pasaje m d) = case m of
                                Just y -> unoSiIguales x y + cantidadDePuntosCon x d
                                Nothing -> cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion m d d2) = case m of
                                Just y -> unoSiIguales x y + cantidadDePuntosCon x d1 + cantidadDePuntosCon x d2
                                Nothing -> cantidadDePuntosCon x d1 + cantidadDePuntosCon x d2

unoSiIguales :: Eq a => a -> a -> Bool
unoSiIguales x y = if x == y then 1 else 0

esLineal ::  Dungeon a -> Bool
esLineal (Habitacion _) = True
esLineal (Pasaje _ d) = esLineal d
esLineal (Bifurcacion _ _ _) = False

llenoDe :: a -> Dungeon -> Bool
llenoDe x (Habitacion y) = x == y
llenoDe x (Pasaje m d) = case m of
                    Just y  -> x == y && llenoDe x d
                    Nothing -> False
llenoDe x (Bifurcacion m d1 d2) = case m of
                    Just y -> x == y && llenoDe x d1 && llenoDe x d2
                    Nothing -> False

{-
Ejercicio 4) Dada la siguiente definición:
    data Tesoro = Cofre | Oro | Joyas
demostrar las siguientes propiedades:   
a. para todo x :: a 
        cantidadDePuntosVacios (Habitacion x) = 0
b. cantidadDePuntosVacios (Pasaje Nothing
        (Habitacion Joyas)) = 1
c. para todo y :: a . para todo x :: a .
    cantidadDePuntosVacios (Pasaje (Just y)(Habitacion x)) = 0
d. cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas))
                             (Pasaje (Just Oro) (Habitacion Cofre))) = 2
e. para todo z :: a . para todo y :: a . para todo x :: a .
cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z))
                            (Pasaje (Just y) (Habitacion x))) = 2
f. cantidadDePuntosVacios
(Bifurcacion (Just Cofre) (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre)))
    (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro) (Pasaje Nothing (Habitacion Joyas)))) = 4

a.
¿para todo x :: a. cantidadDePuntosVacios (Habitacion x) = 0?
Sea g, se verá que: ¡cantidadDePuntosVacios (Habitacion g) = 0!

cantidadDePuntosVacios (Habitacion g) 
=                                    (def cantidadDePuntosVacios.1)
0

Conclusion: cantidadDePuntosVacios (Habitacion x) = 0

b.
cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) = 1

Parto de la izquierda para llegar a la derecha
cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
=                                             (def de cantidadDePuntosVacios.2)
1 + cantidadDePuntosVacios (Habitacion Joyas)
=                                             (def de cantidadDePuntosVacios.1)
1 + 0
=                                             (aritmetica)
1
llego a la derecha

Conclusion : cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) = 1

c. 
 ¿para todo y :: a . para todo x :: a . cantidadDePuntosVacios (Pasaje (Just y)(Habitacion x)) = 0?
 Sea g, y sea h se verá que: ¡cantidadDePuntosVacios (Pasaje (Just g)(Habitacion h)) = 0!

    cantidadDePuntosVacios (Pasaje (Just g)(Habitacion h))
    =                                               (def de cantidadDePuntosVacios.2)
    cantidadDePuntosVacios (Habitacion h)
    =                                               (def de cantidadDePuntosVacios.1)
    0

Conclusion: cantidadDePuntosVacios (Pasaje (Just g)(Habitacion h)) = 0

d.
parto del lado izquierdo para llegar al lado derecho
cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas))
                             (Pasaje (Just Oro) (Habitacion Cofre))) 
=                                                                   (def cantidadDePuntosVacios.3)
1 + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
=                                                                   (def cantidadDePuntosVacios.2)
1 + 1 + cantidadDePuntosVacios  (Habitacion Joyas) +  cantidadDePuntosVacios (Habitacion Cofre)                  
=                                                                   (def cantidadDePuntosVacios.1)
1 + 1 + 0 + 0                  
=                                                                   (aritmetica)
2

Conclusion: cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas))
                             (Pasaje (Just Oro) (Habitacion Cofre))) = 2

e.
para todo z :: a . para todo y :: a . para todo x :: a .
cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z))
                            (Pasaje (Just y) (Habitacion x))) = 2


¿para todo z :: a . para todo y :: a . para todo x :: a . cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z))
                            (Pasaje (Just y) (Habitacion x))) = 2?
 Sea h, sea g y sea i se verá que: ¡cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion h))
                            (Pasaje (Just g) (Habitacion i))) = 2!


cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion h))
                            (Pasaje (Just g) (Habitacion i))) 
=                                                                       (def cantidadDePuntosVacios.3) 
1 + cantidadDePuntosVacios (Pasaje Nothing (Habitacion h)) + cantidadDePuntosVacios (Pasaje (Just g) (Habitacion i)))
=                                                                       (def cantidadDePuntosVacios.2) 
1 + 1 + cantidadDePuntosVacios (Habitacion h) + cantidadDePuntosVacios (Habitacion i)
=                                                                       (def cantidadDePuntosVacios.1) 
1 + 1 + 0 + 0
=                                                                       (aritmetica) 
2

Conclusion: cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion z)) (Pasaje (Just y) (Habitacion x)))

f.
cantidadDePuntosVacios
    (Bifurcacion (Just Cofre) (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre)))
    (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro) (Pasaje Nothing (Habitacion Joyas)))) 
    =
     4

Conclusion: cantidadDePuntosVacios
    (Bifurcacion (Just Cofre) (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre)))
    (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro) (Pasaje Nothing (Habitacion Joyas)))) = 4

Ejercicio 5) Dadas las siguientes definiciones:

data VariasCosas a b = Objeto a | Criatura b
data Monstruo = Gargola | Dragon | Troll

demostrar las siguientes propiedades:
a. cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)) = 0

Parto del lado izquierdo para llegar al lado deerecho
cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
=                                                (def cantidadDePuntosCon.1)                   
0

Conclusion: cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)) = 0

b. cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll))
            (Habitacion (Objeto Oro)))) = 1

Parto del lado izquierdo para llegar al lado derecho
cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll))
            (Habitacion (Objeto Oro))))
=                                                (def cantidadDePuntosCon.2)                   
unoSiIguales (Criatura Troll) (Criatura Troll) + cantidadDePuntosCon  (Criatura Troll) (Habitacion (Objeto Oro))
=                                                (def unoSiIguales) 
1 + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
=                                                (def cantidadDePuntosCon.1) 
1+0
=                                                (aritmetica) 
1

Conclusion: cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll))
            (Habitacion (Objeto Oro)))) = 1

c. cantidadDePuntosCon (Criatura Troll)
    (Pasaje (Just (Criatura Troll))
        (Habitacion (Criatura Troll)))) = 2 


cantidadDePuntosCon (Criatura Troll)
    (Pasaje (Just (Criatura Troll))
        (Habitacion (Criatura Troll)))) 
=                            (def cantidadDePuntosCon.2)
unoSiIguales (Criatura Troll) (Criatura Troll) + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
=                            (def unoSiIguales)
1 + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
=                            (def cantidadDePuntosCon.1)
1 + 1
=                            (aritmetica)
2

Conclusion: cantidadDePuntosCon (Criatura Troll)
    (Pasaje (Just (Criatura Troll))
        (Habitacion (Criatura Troll)))) = 2 

d. cantidadDePuntosCon (Criatura Troll)
    (Bifurcacion (Just (Criatura Troll))
    (Pasaje (Just (Criatura Troll))
    (Habitacion (Objeto Oro)))
    (Pasaje (Just (Criatura Troll))
    (Habitacion (Criatura Troll)))) 
    =                               def cantidadDePuntosCon 
    4

e. cantidadDePuntosCon (Criatura Troll)
    (Pasaje (Just (Criatura Troll))
    (Bifurcacion (Just (Criatura Troll))
    (Pasaje (Just (Criatura Troll))
    (Habitacion (Objeto Oro)))
    (Pasaje (Just (Criatura Troll))
    (Habitacion (Criatura Troll)))))
    =                               def cantidadDePuntosCon 
    5



-}