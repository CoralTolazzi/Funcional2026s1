twice :: (a -> a) -> (a -> a)
twice f x = f (f x)

swap :: (a,b) -> (b,a)
swap (x, y) = (y, x)

doble :: Int -> Int
doble x = x + x

apply :: (a -> b) -> a -> b
apply f x = f x

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
{-
Ejercicio 1) Definir funciones
curry :: ((a,b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a,b) -> c

de tal manera que: 
● para toda función f :: a -> b -> c, se cumple
curry (uncurry f) = f
● y para toda función f’ :: (a,b) -> c, se cumple
uncurry (curry f') = f'
f’ :: (a,b) -> c
f :: a -> b -> c
curry :: ((a,b)->c) -> (a->(b->c))
uncurry :: (a->(b->c)) -> (a,b) -> c
curry (uncurry f) = f
curry ((a->(b->c)) -> ((a,b)->c) f)= f

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)
curry f = g where g x = h where h y = f (x,y)

uncurry :: (a->(b->c)) -> ((a,b)->c)
uncurry f = g where g (x,y) = (f x) y
uncurry f (x,y) = (f x) y


-}
{-
Ejercicio 2) Reescribir las siguientes definiciones sin utilizar where, let o lambdas, y
utilizando la menor cantidad de paréntesis posible.
a. apply f = g where g x = f x
apply f x = f x

b. twice f = g where g x = f (f x)
twice f x = f (f x)

c. id = \x -> x
id x = x

d. flip f = g where g x = h h y = (f y) x
flip f x y = (f y) x

e. uflip f = g where g p = f (swap p)
uflip f p = f (swap p)

f. const = \x -> (\y -> x)
const x y = x

g. compose = \f -> (\g -> (\x -> f (g x)))
compose f g x = f (g x)

-}

{-
Ejercicio 3) Indicar el tipo de cada una de las funciones del ejercicio anterior,
utilizando también la menor cantidad posible de paréntesis.


apply :: (a -> b) -> a -> b

twice :: (a -> a) -> a -> a

id :: a -> a

flip :: (a -> b -> c) -> b -> a -> c
d. flip f = g where g x = h h y = (f y) x
flip f x y = (f y) x


uflip :: ((b,a) -> c) -> (a,b) -> c

const :: b -> a -> b

compose :: (... -> ...) -> (... -> ...) -> ... -> ...
compose :: (... -> ...) -> (a -> ...) -> a -> ...
compose :: (b -> ...) -> (a -> b) -> a -> ...
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-}
{-
Ejercicio 4) En las expresiones que siguen, colocar los paréntesis que están
implícitos, manteniendo el significado de cada una de las expresiones, y dar el tipo
de cada una de ellas, suponiendo dadas las definiciones de los ejercicios anteriores.

a. apply apply apply
(apply apply)apply
apply apply apply :: (a -> b) -> a -> b


b. twice doble 2
(twice doble) 2

twice :: (Int -> Int) -> (Int -> Int)
doble :: Int -> Int
-----------------------------------
twice doble :: (Int -> Int) 2

c. twice twice twice swap
((twice twice) twice) swap
-}
{-
paso 1 
twice :: (c -> c) -> (c -> c)
swap :: (a,b) -> (b,a)
-----------------------
twice swap :: 

c = (a,b)
c = (b,a)
b = a

paso 2
twice :: ((a,a) -> (a,a)) -> ((a,a) -> (a,a))
swap :: (a,a) -> (a,a)
-----------------------
twice twice twice swap :: (a,a) -> (a,a)
-}

{-
d. ((flip twice) 1) doble

paso 1:

flip :: (e -> b -> c) -> b -> e -> c
twice :: (a -> a) -> (a -> a)
---------------------------------
flip twice :: ...

(e -> b -> c) = (a -> a) -> (a -> a)

e = a -> a
b -> c = a -> a
b = a
c = a

((a -> a) -> (a -> a)) -> b -> a -> a -> c

paso 2:

flip :: ((a -> a) -> (a -> a)) -> a -> (a -> a) -> a
twice :: (a -> a) -> (a -> a)
---------------------------------
flip twice :: a -> (a -> a) -> a


flip twice :: a -> (a -> a) -> a
1 :: Int
----------------------------------------
flip twice 1 :: (Int -> Int) -> Int 

Int = a

flip twice 1 :: (Int -> Int) -> Int 
doble :: Int -> Int
--------------------------------------
flip twice 1 doble :: Int 
-}

{-
Ejercicio 5) Reescribir las siguientes definiciones utilizando sólo lambdas (sin where ni let).

a. appDup f = g where g x = f (x, x)
appDup f = g where g x = f (x, x)
appDup f x = f (x, x) =                     \f x -> f (x, x)


b. appFork (f, g) = h where h x = (f x, g x)
appFork (f, g) x = (f x, g x) =             \(f, g) x -> (f x, g x)


c. appPar (f, g) = h where h (x, y) = (f x, g y)
appPar (f, g) (x, y) = (f x, g y) =           \(f, g) (x, y) -> (f x, g y)



d. appDist f = g where g (x, y) = (f x, f y)
appDist f (x, y) = (f x, f y) =                \f (x, y) -> (f x, f y)    


e. subst f = h where h g = k where k x = (f x) (g x)
subst f g x = (f x) (g x) =                     \f g x -> (f x) (g x)

-}
{-
Ejercicio 6) Indicar cuáles de las siguientes expresiones tienen tipo según el sistema
de tipos de Hindley Milner. En el caso de que alguna sea incorrecta, ¿existirá una
expresión que utilice las mismas partes, pero asociadas de forma diferente y que sí
tenga significado? En el caso de que sí, escribir tal variante.

a. compose (fst snd)

compose :: 
(fst snd) ::
----------------------------
compose (fst snd) ::

fst :: (a, b) -> a
snd :: (a, b) -> b
-------------------------
fst snd ::





b. uncurry curry snd

Alternativa con uncurry (curry snd) ya que uncurry curry snd no tipa

paso 1
uncurry :: (a->(b->c)) -> (a,b) -> c 
curry snd :: d -> e -> e
-------------------------
uncurry (curry snd) ::

a -> b -> c = d -> e -> e  
a = d
b = e
c = e 
Entonces:
b = c = e

paso 2
uncurry :: (d->(e->e)) -> (d,e) -> e 
curry snd :: d -> e -> e
-------------------------
uncurry (curry snd) :: (d,e) -> e 

paso 1:
curry ::((a,b) -> c) -> a -> b -> c
snd :: (d,e) -> e
--------------------
curry snd ::  

((a,b) -> c) = (d,e) -> e

(a,b) = (d,e)
a = d
b = e
entonces: 
c = e = b

paso 2:
curry ::((d,e) -> e) -> d -> e -> e
snd :: (d,e) -> e
--------------------
curry snd :: d -> e -> e

c. (apply id) ((id apply) apply)

(apply id) ((id apply) apply)

apply id :: ((a -> b) -> (a -> b)) -> ((a -> b) -> (a -> b))
(id apply) apply :: (a -> b) -> (a -> b)
------------------------------------
apply id ((id apply) apply) :: (a -> b) -> (a -> b)

apply :: (a -> a) -> (a -> a)
id :: a -> a
--------------------------------
apply id ::: a -> a

id :: (a -> b) -> (a -> b) -> (a -> b) -> (a -> b)
apply :: (a -> b) -> (a -> b)
-----------------------------
id apply :: (a -> b) -> (a -> b)

id apply :: ((a -> b) -> (a -> b)) -> (a -> b) -> (a -> b)
apply :: (a -> b) -> (a -> b)
----------------------------
(id apply) apply :: (a -> b) -> (a -> b)

 id apply :: (a -> b) -> a -> b
apply :: (a -> b) -> (a -> b)

(a -> b) -> (a -> b) = c -> d

a -> b = c
a -> b = d


d. compose ((compose doble) doble)

compose :: (Int -> Int) -> (a -> Int) -> a -> Int
doble :: Int -> Int 
-------------------------------
compose doble :: (a -> Int) -> a -> Int

Int -> Int = b -> c
b = Int
c = Int


compose doble :: (Int -> Int) -> Int -> Int
doble :: Int -> Int 
-------------------------------------
(compose doble) doble :: Int -> Int

a -> Int = Int -> Int  
a = Int
Int = Int

compose :: (Int -> Int) -> (a -> Int) -> a -> Int
(compose doble) doble :: Int -> Int
-----------------------------------------
compose ((compose doble) doble) :: (a -> Int) -> a -> Int

e. (compose compose) doble doble

No se Puede

doble :: Int -> Int
doble :: Int -> Int 
-------------------------------
doble doble :: 

Int -> Int  = Int No son comparables 

-}
{-
Ejercicio 7) Dada la siguiente definición, indicar cómo podría reescribirse 
usando compose y id:
many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = compose f (many (n-1) f)

many 2 doble 2
compose doble (many (2-1) doble) 2

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

id x = x
id = \x -> x

compose doble doble 2 = doble (doble 2)
-}
{-
Ejercicio 8) Quitar de los siguientes tipos la mayor cantidad de paréntesis posible, sin
cambiar su significado. En cada caso, escribir en castellano cómo debería leerse el
tipo obtenido de forma correcta, y cómo con la convención de leerla como si
estuviera no-currificada.
Por ejemplo, para Int -> (Int -> Int) las respuestas serían: Int -> Int ->
Int, en castellano en forma correcta “es una función que toma un entero y
devuelve una función que toma otro entero y devuelve un entero”, y en castellano
usando la convención “es una función que toma dos enteros y devuelve un entero”.

a. (Int -> Int) -> (Int -> Int)
"Es una funcion que toma una funcion que toma un Entero y devuelve otro entero y devuelve una 
funcion que toma otro entero y devuelve otro entero"
a. (Int -> Int) -> Int -> Int
"Es una funcion que toma una funcion que toma un Entero y devuelve otro entero y toma otro
entero que devuelve entero" 
--------------------------------------------------------------------------------

b. (a -> (b -> c)) -> (a -> b) -> c
   (a -> b -> c) -> (a -> b) -> c

c. (a -> b, c -> d) -> ((a, c) -> (b, d))   
   (a -> b, c -> d) -> (a, c) -> (b, d)

d. ((a, a) -> b) -> (a -> b)   
   ((a, a) -> b) -> a -> b

e. (a -> (b -> c)) -> (b -> (a -> c))
   (a -> b -> c) -> b -> a -> c

f. (a -> b) -> ((a, a) -> (b, b))
   (a -> b) -> (a, a) -> (b, b)

g. (a -> b, a -> c) -> (a -> (b, c))
   (a -> b, a -> c) -> a -> (b, c)

h. (a -> (b -> c)) -> ((a -> b) -> (a -> c))
   (a -> b -> c) -> (a -> b) -> a -> c

i. a -> (b -> a)
   a -> b -> a

h         ::(a -> (b -> c)) -> ((a -> b) -> (a -> c))
compose flip uncurry ::  (a -> b1 -> b2 -> c) -> b2 -> (a, b1) -> c

-}
{-
Ejercicio 9) Dar expresiones equivalentes a las funciones definidas a continuación
utilizando funciones como compose, flip, etc. (dadas en los ejercicios anteriores) y
sin utilizar lambas.

a. cuadruple x = doble (doble x)
cuadruple = compose doble doble

b. timesTwoPlusThree x = suma (doble x) 3
timesTwoPlusThree x = flip suma 3 (doble x) 

c. fourTimes f x = f (f (f (f x)))
fourTimes = twice twice
fourTimes = many 4
-}

{-
Ejercicio 10) Investigar la notación de sección de operadores.

-- Notación     | Significado                 | Equivalente en lambda
-- -------------|-----------------------------|--------------------------
-- (+3)         | Sumar 3 al argumento        | \x -> x + 3
-- (3+)         | Sumar argumento a 3         | \x -> 3 + x
-- (*2)         | Multiplicar por 2           | \x -> x * 2
-- (2*)         | 2 por argumento             | \x -> 2 * x
-- (/2)         | Dividir por 2               | \x -> x / 2
-- (2/)         | 2 dividido por argumento    | \x -> 2 / x
-- (==3)        | ¿Es igual a 3?              | \x -> x == 3
-- (3==)        | ¿3 es igual a argumento?    | \x -> 3 == x
-- (>5)         | ¿Mayor que 5?               | \x -> x > 5
-- (5>)         | ¿5 mayor que argumento?     | \x -> 5 > x

-}