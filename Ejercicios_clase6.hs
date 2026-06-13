data BST23 a = Cero 
             | Dos Int Int (BST23 a) a (BST23 a) 
             | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)

bst23 = Dos 3 4 (Dos 1 2 (Dos 0 1 Cero 20 Cero ) 21 Cero ) 30 (Dos 0 1 Cero 31 Cero)

bst23' = Tres 3 4 (Dos 1 2 Cero 32 Cero) 44 (Dos 1 2 Cero 33 Cero) 55 (Dos 0 1 Cero 3 Cero)

-- Invariante de Representacion
-- en t = Dos h n t1 x t2
            -- h es la altura del t
            -- n es el size del t
            -- t1 y t2 tienen la misma altura
            -- todos los elementos de t1 son < x
            -- todos los elementos de t2 son > x
            -- t1 y t2 cumplen la inv. rep.

-- en t = Tres h n t1 x t2 y t3
            -- h es la altura del t
            -- n es el size de t
            -- t1, t2, t3 tienen la misma altura
            -- todos los elementos de t1 son < x
            -- todos los elementos de t2 son > x ; < y
            -- x < y
            -- t1, t2 y t3 cumplen la inv. rep.

-- 1. Dar el tipo y definicion de fold23 y rec23

fold23 :: b -> (Int -> Int -> b -> a -> b -> b) -> (Int -> Int -> b -> a -> b -> a -> b -> b) -> BST23 a -> b
fold23 c d t Cero = c
fold23 c d t (Dos n1 n2 bst1 x bst2) = d n1 n2 (fold23 c d t bst1) x (fold23 c d t bst2)
fold23 c d t (Tres n1 n2 bst1 x1 bst2 x2 bst3) = t n1 n2 (fold23 c d t bst1) x1 (fold23 c d t bst2) x2 (fold23 c d t bst3)

rec23 :: b -> (Int -> Int -> BST23 a -> BST23 a -> b -> a -> b -> b) -> (Int -> Int -> BST23 a -> BST23 a -> BST23 a -> b -> a -> b -> a -> b -> b)  -> BST23 a -> b
rec23 c d t Cero = c
rec23 c d t (Dos n1 n2 bst1 x bst2) = d n1 n2 bst1 bst2 (rec23 c d t bst1) x (rec23 c d t bst2) 
rec23 c d t (Tres n1 n2 bst1 x1 bst2 x2 bst3) = t n1 n2 bst1 bst2 bst3 (rec23 c d t bst1) x1 (rec23 c d t bst2) x2 (rec23 c d t bst3)

-- 2. Definir las siguientes funciones sin recursion explicita

--fold23 :: [a] -> (Int -> Int -> [a] -> a -> [a] -> [a]) -> (Int -> Int -> [a] -> a -> [a] -> a -> [a] -> [a]) -> BST23 a -> [a]
inOrder :: BST23 a -> [a]
inOrder = fold23 [] (\n1 n2 xs1 x xs2 -> xs1 ++ [x] ++ xs2) (\n1 n2 xs1 x1 xs2 x2 xs3 -> xs1 ++ [x1] ++ xs2 ++ [x2] ++ xs3)

--fold23 :: Int -> (Int -> Int -> Int -> a -> Int -> Int) -> (Int -> Int -> Int -> a -> Int -> a -> Int -> Int) -> BST23 a -> Int
cantElem :: BST23 a -> Int
cantElem = fold23 0 (\_ n _ _ _ -> n) (\_ n _ _ _ _ _ -> n)

altura :: BST23 a -> Int
altura = fold23 0 (\h n r1 x r2 -> h) (\h n r1 x1 r2 x2 r3 -> h)


minElem :: BST23 a -> a
minElem = rec23 (error "árbol vacío")
                (\n1 n2 bst1 bst2 r1 x r2 -> case bst1 of
                    Cero -> x
                    _    -> r1)
                (\n1 n2 bst1 bst2 bst3 r1 x1 r2 x2 r3 -> case bst1 of
                    Cero -> x1
                    _    -> r1)

maxElem :: BST23 a -> a
maxElem = rec23 (error "árbol vacío")
                (\n1 n2 bst1 bst2 r1 x r2 -> case bst2 of
                    Cero -> x
                    _    -> r2)
                (\n1 n2 bst1 bst2 bst3 r1 x1 r2 x2 r3 -> case bst3 of
                    Cero -> x2
                    _    -> r3)

search :: Ord a => a -> BST23 a -> Bool
search e = fold23 False
    (\h n r1 x r2 ->
        if e == x      then True
        else if e < x  then r1
        else                r2)
    (\h n r1 x1 r2 x2 r3 ->
        if e == x1 || e == x2 then True
        else if e < x1        then r1
        else if e < x2        then r2
        else                       r3)