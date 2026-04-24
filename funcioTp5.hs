
--Ejercicio 1) Dadas las siguientes definiciones

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
                                                deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto
                                                deriving Show

chocoHelate consH = consH Chocolate


{-
determinar el tipo de las siguientes expresiones:
a. Vasito :: Gusto -> Helado
b. Chocolate :: Gusto
c. Cucurucho :: Gusto -> Gusto -> Helado
d. Sambayon :: Gusto
e. Pote :: Gusto -> Gusto -> Gusto -> Helado
f. chocoHelate ::  (Gusto -> a) -> a
g. chocoHelate Vasito :: Helado
h. chocoHelate Cucurucho :: Gusto -> Helado
i. chocoHelate (Cucurucho Sambayon) :: Helado
j. chocoHelate (chocoHelate Cucurucho) :: Helado
k. chocoHelate (Vasito DulceDeLeche) :: No tipa muajaja
l. chocoHelate Pote :: Gusto -> Gusto -> Helado
m. chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

a) Es un constructor parametrizado para el tipo Helado. 
b) Es una constante no funcional de tipo Gusto.
c) Es un constructor parametrizado para el tipo Helado.
d) Es una constante no funcional de tipo Gusto.
e) Es un constructor parametrizado para el tipo Helado. 
f) Es una función de orden superior que retorna elementos de tipo arbitrario.
g) Es un elemento del tipo Helado.
h) Es una función que retorna elementos del tipo Helado. 
i) Es un elemento del tipo Helado.
j) Es un elemento del tipo Helado. 
k) No tiene tipo. 
l) Es una función que retorna elementos del tipo Helado.
m) Es un elemento del tipo Helado.

-- chocoHelate :: (Gusto -> a) -> a
-- consH :: Gusto -> a
-- -----------------------------
-- chocoHelate consH :: a

-- consH :: Gusto -> a
-- Chocolate :: Gusto
-- --------------------------------
-- consH Chocolate :: a

--Ejercicio 2) Dado el siguiente tipo que pretende representar dígitos binarios
data DigBin = O | I

definir las siguientes funciones:
a. dbAsInt :: DigBin -> Int, que dado un símbolo que representa un
dígito binario lo transforma en su significado como número.
b. dbAsBool :: DigBin -> Bool, que dado un símbolo que representa un
dígito binario lo transforma en su significado como booleano.
c. dbOfBool :: Bool -> DigBin, que dado un booleano lo transforma en el
símbolo que representa a ese booleano.
d. negDB :: DigBin -> DigBin, que dado un dígito binario lo transforma en
el otro.
-}

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

{-
Ejercicio 3) Dado el siguiente tipo que pretende representar dígitos decimales
data DigDec = D0 | D1 | D2 | D3 | D4
| D5 | D6 | D7 | D8 | D9
definir las siguientes funciones:
a. ddAsInt :: DigDec -> Int, que dado un símbolo que representa un
dígito decimal lo transforma en su significado como número.
b. ddOfInt :: Int -> DigDec, que dado un número entre 0 y 9 lo transforma
en el símbolo que representa a ese dígito.
c. nextDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma
en el siguiente según el orden circular dado en la definición.
d. prevDD :: DigDec -> DigDec, que dado un dígito decimal lo transforma
en el anterior según el orden circular dado en la definición.
-}
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

ddAsInt :: DigDec -> Int
ddAsInt D0  = 0
ddAsInt D1 = 1
ddAsInt D2  = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9
ddOfInt _ = error "No definido" 


nextDD :: DigDec -> DigDec
nextDD D9 = D0
nextDD d = ddOfInt (succ (ddAsInt d))

{-
nextDD :: DigDec -> Int
nextDD D0  = D1
nextDD D1 = D2
nextDD D2  = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0
-}

prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD d = ddOfInt ((ddAsInt d) - 1)

{-
prevDD :: DigDec -> DigDec
prevDD D0  = D9
prevDD D1 = D0
prevDD D2  = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8

-}

{-
Ejercicio 4) Dado el siguiente tipo que representa medidas en un software de dibujo
como LibreOffice Draw.
data Medida = Mm Float | Cm Float
| Inch Float | Foot Float

escribir las siguientes funciones:
a. asMm :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en milímetros que aproxima la dada según la
conversión establecida.
b. asCm :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en centímetros que aproxima la dada según la
conversión establecida.
c. asInch :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en pulgadas que aproxima la dada según la
conversión establecida.
d. asFoot :: Medida -> Medida, que dada una medida cualquiera la
transforma en una medida en pies que aproxima la dada según la conversión

-}
data Medida = Mm Float | Cm Float | Inch Float | Foot Float

asMm :: Medida -> Medida 
asMm (Mm n) = Mm n
asMm (Cm n) = Mm (n*10)
asMm (Inch n) = Mm (n* 25.4)
asMm (Foot n) = Mm (n*30.8)

asCm ::  Medida -> Medida 
asCm (Inch n) = Cm (n * 2.54)
asCm (Cm n) = Cm n
--asCm (Mm n) = Cm (div n 10)
asCm (Foot n) = Cm (n * 30.48)

asInch ::  Medida -> Medida 
asInch (Inch n) = Inch n
--asInch (Cm n) = Inch (div n 2.54)
--asInch (Mm n) = Inch (div n 25.4)
asInch (Foot n) = Inch (n * 12)

asFoot ::  Medida -> Medida 
--asFoot (Inch n) = Foot (div n 12)
--asFoot (Cm n) = Foot (div n 30.48)
--asFoot (Mm n) = Foot (n/304.8)
asFoot (Foot n) = Foot n

{-
Ejercicio 5) Dadas las siguientes definiciones:
data Shape = Circle Float | Rect Float Float
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0
Determinar el tipo de las siguientes expresiones:
a. uncurry Rect
b. construyeShNormal (flip Rect 5.0)
c. compose (uncurry Rect) swap
d. uncurry Cucurucho
e. uncurry Rect swap
f. compose uncurry Pote
g. compose Just
h. compose uncurry (Pote Chocolate)
-}

data Shape = Circle Float | Rect Float Float
                deriving Show

data Set a = S (a -> Bool)
               

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

compose :: (b -> c) -> (a -> b) -> a -> c   
compose f g x = f (g x)

{-
a)
uncurry Rect

uncurry :: (a->b->c) -> (a,b)-> c
Rect :: Float -> Float -> Shape 
-----------------
uncurry Rect 

a = Float
b = Float
c = Shape

uncurry :: (a->b->c) -> (a,b)-> c
Rect :: Float -> Float -> Shape 
-----------------
uncurry Rect :: (Float, Float) -> Shape

b)
construyeShNormal (flip Rect 5.0)

construyeShNormal :: (Float -> Shape) -> Shape
flip Rect 5.0 :: Float -> Shape
----------------------------------------------
construyeShNormal (flip Rect 5.0) :: Shape

c)
compose (uncurry Rect) swap

compose :: (b -> c) -> (a -> b) -> a -> c 
(uncurry Rect) :: (Float, Float) -> Shape
------------------------------------
compose (uncurry Rect) :: (a -> (Float,Float)) -> a -> Shape
swap :: (a,b) -> (b,a)
---------------------------------------
compose (uncurry Rect) swap :: (Float,Float) -> Shape

a = (e,b)
(Float, Float) = (b,e)

d)
uncurry Cucurucho 

uncurry :: (a->b->c) -> (a,b)-> c
Cucurucho :: Gusto -> Gusto -> Helado
------------------------------------------
uncurry Cucurucho :: (Gusto,Gusto) -> Helado

e)
uncurry Rect :: (Float, Float) -> Shape
swap :: (a,b) -> (b,a)
---------------------------
uncurry Rect swap :: ----

(a,b) -> (b,a) != (Float, Float)

f)
compose :: (b -> c) -> (a -> b) -> a -> c
uncurry :: (d->h->e) -> (d,h)-> e
---------------
compose uncurry :: (a -> d -> h -> e) -> a -> (d,h)-> e
Pote :: Gusto -> Gusto -> Gusto -> Helado
-----------------------
compose uncurry Pote :: Gusto -> (Gusto,Gusto) -> Helado


(b -> c) = (d->h->e) -> (d,h)-> e

b = (d->h->e)
c = (d,h)-> e

(a -> d -> h -> e) -> a -> (d,h)-> e

g)
compose :: (b -> c) -> (a -> b) -> a -> c
Just :: d -> Maybe d
------------------------------------
compose Just ::  (a -> b) -> a -> Maybe b 

h)
compose :: (b -> c) -> (a -> b) -> a -> c
uncurry :: (d->h->e) -> (d,h)-> e
---------------------
compose uncurry :: (a -> d -> h -> e) -> a -> (d,h)-> e
(Pote Chocolate) :: Gusto -> Gusto -> Helado
---------------------------------
compose uncurry (Pote Chocolate) :: NO TIENE TIPO

a -> d -> h -> e =  Gusto -> Gusto -> Helado

Ejercicio 6) Para cada una de las expresiones del ejercicio anterior que denoten
funciones, construir una expresión aplicándola.

uncurry Rect :: (Float, Float) -> Shape
uncurry Rect (2.0,1.0)
Rect 2.0 1.0

construyeShNormal (flip Rect 5.0) :: Shape
Rect 1.0 5.0

compose (uncurry Rect) swap :: (Float,Float) -> Shape
compose (uncurry Rect) swap (2.0,1.0)
Rect 1.0 2.0

uncurry Cucurucho :: (Gusto,Gusto) -> Helado
uncurry Cucurucho (Chocolate, Chocolate)
Cucurucho Chocolate Chocolate

compose uncurry Pote :: Gusto -> (Gusto,Gusto) -> Helado
compose uncurry Pote Chocolate (Chocolate,Chocolate)    
Pote Chocolate Chocolate Chocolate

Ejercicio 7) Dado el tipo Set definido en la clase teórica
data Set a = S (a -> Bool)
definir las siguientes funciones sobre él:
a. belongs :: Set a -> a -> Bool, que dado un conjunto, describe la
función que indica si un elemento dado pertenece a ese conjunto.
b. empty :: Set a, que describe el conjunto vacío.
c. singleton :: a -> Set a, que dado un elemento describe un conjunto
que contiene a ese único elemento.
d. union :: Set a -> Set a -> Set a, que dados dos conjuntos,
describe al conjunto que resulta de la unión de ambos.
e. intersection :: Set a -> Set a -> Set a, que dados dos
conjuntos, describe al conjunto que resulta de la intersección de ambos.

Ejercicio 7) 
a)
belongs :: Set a -> a -> Bool
belongs (S f) x = f x

b)
empty :: Set a
empty = S (\_ -> False)

c)
singleton :: a -> Set a
singleton x = S (\y -> y == x )

d)
union :: Set a -> Set a -> Set a
union (S f) (S g) = S (\x -> f x || g x)

e)
intersection :: Set a -> Set a -> Set a
intersection (S x) (S y) = S (\x -> f x && g x)

Ejercicio 8) Dados los siguientes tipos que modelan los posibles resultados de
computar con excepciones
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer
| Other String
type ExHandler a = Exception -> a
definir la función
tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
que dada una computación que puede fallar, una función que indica cómo continuar si
no falla, y un manejador de los casos de falla, expresa la computación completa.
Un ejemplo que utiliza esta función sería el siguiente:

sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
sueldoGUIE nombre empleados =
    tryCatch (lookupE nombre empleados)
            mostrarInt
            (\e -> case e of
                    NotFound -> ventanaError msgNotEmployee
                            _ -> error msgUnexpected)
    where msgNotEmployee = "No es empleado de la empresa"
        msgUnexpected = "Error inesperado"

sabiendo que

mostrarInt :: Int -> GUI Int
ventanaError :: String -> GUI a
lookupE :: Nombre -> [Empleado] -> MayFail Int
-}

{-
mults3 :: Set Int
mults3 = S (x -> mod x 3 == 0)

pares :: Set Int
pares = S esPar

belongs :: Set a -> a -> Bool
belongs (S f) x = f x

esPar n = mod n 2 == 0

union :: Set a -> Set a -> Set a
union (S f) (S g) = S (\x -> f x || g x)

intersection :: Set a -> Set a -> Set a
intersection (S x) (S y) = S (\x -> f x && g x)


--Ejercicio 8) Dados los siguientes tipos que modelan los posibles resultados de
--computar con excepciones
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise e) f h = h e 
tryCatch (Ok x) f h = f x 
que dada una computación que puede fallar, una función que indica cómo continuar si
no falla, y un manejador de los casos de falla, expresa la computación completa.

Un ejemplo que utiliza esta función sería el siguiente:
sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
sueldoGUIE nombre empleados =
    tryCatch (lookupE nombre empleados)
            mostrarInt
            (\e -> case e of
                    NotFound -> ventanaError msgNotEmployee
                            _ -> error msgUnexpected)
    where msgNotEmployee = "No es empleado de la empresa"
        msgUnexpected = "Error inesperado"

sabiendo que

mostrarInt :: Int -> GUI Int
ventanaError :: String -> GUI a
lookupE :: Nombre -> [Empleado] -> MayFail Int

-}


