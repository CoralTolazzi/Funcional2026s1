data Shape = Circle Float | Rect Float Float
                    deriving Show
data Par a = DosCosas a a
            deriving Show
data Aveces a = Nada | AcaEsta a
                    deriving Show
cuadrado n = Rect n n
shapeNormal c = c 1.0
chocoHelate c = c Chocolate
string2Gusto "DDL" = DDL
string2Gusto "Chocolate" = Chocolate 
string2Gusto "Frutilla" = Frutilla
armarHeladoCon sg = Vasito (sg "Chocolate")
data Gusto = Chocolate | DDL | Frutilla | Sambayon
                                                deriving Show
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto
                                                deriving Show


{-
1) Dar el tipo de
cuadrado :: Float -> Shape 
shapeNormal :: (Float -> a) -> a
chocoHelate :: (Gusto -> a) -> a
string2Gusto :: String -> Gusto
armarHeladoCon :: (String -> Gusto) -> Helado

2) Dar el tipo y reducir
shapeNormal Circle :: Shape
Circle 1.0

shapeNormal cuadrado :: Shape
cuadrado 1.0
Rect 1.0 1.0

shapeNormal Rect :: Float -> Shape
Rect 1.0 

shapeNormal Rect 2 :: Shape
Rect 1.0 2

shapeNormal (Rect 2) :: Shape
Rect 2 1.0

chocoHelate Vasito :: Helado
Vasito Chocolate

chocoHelate Cucurucho :: Gusto -> Helado
Cucurucho Chocolate

chocoHelate Cucurucho Sambayon :: Helado
Cucurucho Chocolate Sambayon

chocoHelate (Cucurucho Sambayon) :: Helado
Cucurucho Sambayon Chocolate

chocoHelate (flip Cucurucho Sambayon) :: Helado
Cucurucho Chocolate Sambayon

chocoHelate (\g -> Cucurucho g Sambayon) :: Helado 
(\g -> Cucurucho g Sambayon) = Gusto -> Helado
Cucurucho Chocolate Sambayon

chocoHelate (chocoHelate Cucurucho) :: Helado
Cucurucho Chocolate Chocolate

chocoHelate Pote :: Gusto -> Gusto -> Helado
Pote Chocolate

chocoHelate poteDeUnGusto :: Helado
Pote Chocolate Chocolate Chocolate

armarHeladoCon string2Gusto :: Helado
Vasito Chocolate

armarHeladoCon error :: Helado
Vasito error

esVasito (armarHeladoCon error) :: Bool
True

3) Dar algunos elementos de los siguientes tipos
Par Bool 
Par Helado
Par (Gusto -> Helado)

4)Dar el tipo de
DosCosas (Cucurucho DDL)

DosCosas :: a -> a -> Par a
Cucurucho DDL :: Gusto -> Helado
-------------------------------------
DosCosas (Cucurucho DDL) :: Gusto -> Helado -> Par (Gusto -> Helado)

5) Escribir las funciones

esCuadrado :: Shape -> Bool
esCuadrado (Rect  f1 f2) = f1 == f2
esCuadrado _ = False 

armarCuadradoAlRecibir :: Float -> (Float -> Shape) -> Bool
armarCuadradoAlRecibir f1 ffs = esCuadrado (ffs f1)
que verifica si la funcion arma un cuadrado al recibir el numero dado  

armarCuadradoDeTamanioCuandoRecibe :: Float -> Float -> (Float -> Shape) -> Bool
armarCuadradoDeTamanioCuandoRecibe f1 f2 ffs = esCuadrado (armarCuadradoAlRecibir f2 (fss f1))
que verifica si la funcion dada arma un cuadrado del tamanio del primer numero al 
recibir el segundo numero 

siempreArmaCuadrado :: (Float -> Shape) -> Bool 
siempreArmaCuadrado ffs = esCuadrado (\x -> ffs x)
que verifica si la funcion dada arma un cuadrado de cualquier tamanio cuando recibe cualquier otro numero.
Para comprobar si esta funcion hace lo esperado, aplicar la funcion definido a
cuadrado
Rect 2
shapeNormal Rect

\n ... if n < 10
    then cuadrado (n+1)
    else Circle n

-}

