module Futoshiki where


-- Bidimensional Matrix --
type Matrix a = [[a]]

-- Relation between cells --
data Relation = Ind | RGT | RLT
  deriving (Eq, Read, Show)

-- Futoshiki definition --
data Futoshiki = Futoshiki {
  size  :: Int,
  cells :: Matrix Int,
  hRel  :: Matrix Relation,
  vRel  :: Matrix Relation
}
  deriving (Eq, Read, Show)

{-
existeHuecos: Se encarga de verificar si en la matriz hay huecos (ceros)
mediante el uso de la recursividad y elem.
-}

existeHuecos :: (Foldable t, Eq a, Num a) => [t a] -> Bool
existeHuecos [] = False
existeHuecos (fila: filas) 
  | 0 `elem` fila = True
  | otherwise = existeHuecos filas

{-
cumpleReglas: Verifica si el futoshiki actual cumple las reglas, que no haya repetidos en las filas y en las columnas
y que se cumplan las restricciones RGT y RLT, a modo de aclaración se puede transponer las matrices tanto de numeros
como de restricciones para hacer el mismo proceso para la parte vertical del futoshiki como en la horizontal.
-}

cumpleReglas :: Futoshiki -> Bool
cumpleReglas futoshiki =
 not(repetidosFilasMatriz (cells futoshiki)) && --Verificar si hay elementos repetidos en una misma fila
 not(repetidosFilasMatriz (transponerMatriz (cells futoshiki))) && --Verificar si hay elementos repetidos en una misma columna transponiendo la matriz
 verificarRestriccionesMatrizHorizontal (cells futoshiki) (hRel futoshiki) &&
 verificarRestriccionesMatrizHorizontal (transponerMatriz(cells futoshiki)) (transponerMatriz(vRel futoshiki))

{-
verificarRestriccionesMatriz: Verifica si se cumple las restricciones horizontales por la matriz.
-}

verificarRestriccionesMatrizHorizontal :: (Num a, Ord a) => [[a]] -> [[Relation]] -> Bool
verificarRestriccionesMatrizHorizontal [] [] = True
verificarRestriccionesMatrizHorizontal (fe:rfe) (frel: rfrel)
  | verificarRestriccionesFila fe frel = verificarRestriccionesMatrizHorizontal rfe rfrel
  | otherwise = False

{-
verificarRestriccionesFila: Verifica si en una fila se cumple las restricciones.
-}

verificarRestriccionesFila :: (Num a, Ord a) => [a] -> [Relation] -> Bool
verificarRestriccionesFila _ [] = True -- Si las filas de las resticciones está vacía significa que se han cumplido todas las restricciones
verificarRestriccionesFila (e1:e2:er) (rel1: relr)
  | verificarComparacion e1 e2 rel1 = verificarRestriccionesFila (e2:er) relr
  | otherwise = False

verificarRestriccionesFila _ _ = True

{-
verificarComparacion: Verifica si se cumple una restriccion entre dos casillas de la celda.
-}

verificarComparacion :: (Num a, Ord a) => a -> a -> Relation -> Bool
--Si alguno de los elementos tiene 0 se ignora debido a que son huecos, para comparar ambos elementos
--no deben de ser huecos
verificarComparacion 0 _ _ = True
verificarComparacion _ 0 _ = True
--Caso normal, segun rel (restricción entre casillas) hará una comparación u otra entre 2 elementos
verificarComparacion e1 e2 rel =
  case rel of
    RGT ->  e1 > e2
    RLT ->  e1 < e2
    Ind ->  True

{-
repetidosFilasMatriz: Verifica que no haya numeros repetidos en las filas de la matriz
-}

repetidosFilasMatriz :: (Eq a, Num a) => [[a]] -> Bool
repetidosFilasMatriz [] = False
repetidosFilasMatriz (fila:resto)
  | repetidosFila fila = True
  | otherwise = repetidosFilasMatriz resto

{-
repetidosFila: Verifica que no haya ningún número repetido en la misma fila de la matriz
del Futoshiki.
-}

repetidosFila :: (Eq a, Num a) => [a] -> Bool
repetidosFila [] = False
repetidosFila (elemento:resto)
  | elemento `elem` resto && elemento /= 0 = True
  | otherwise = repetidosFila resto

{-
transponerMatriz: Hace la traspuesta de la matriz que se recibe por parametro y la devuelve,
servirá para verificar las columnas del Futoshiki, va cogiendo las cabezas de las filas y
las va metiendo en una simple fila, así recursivamente hasta que se quede vacía la matriz,
que ahí ya se añade de nuevo a la matriz y da lugar a la matriz traspuesta.
-}

transponerMatriz :: [[a]] -> [[a]]
transponerMatriz [] = []
transponerMatriz ([]:_) = []
transponerMatriz matriz = (map head matriz) : transponerMatriz( map tail matriz)

{- 
esSol: Verifica si el futoshiki que hay por entrada no tiene huecos y cumple las normas
que pide el propio enunciado.
-}

esSol :: Futoshiki -> Bool
esSol futoshiki
  | not(existeHuecos (cells futoshiki)) && cumpleReglas futoshiki = True
  | otherwise = False

{-
reemplazar: Reemplaza una celda de la matriz dada una fila [0..n-1] y columna en especifico [0..n-1]
-}

reemplazar :: Int -> Int -> a -> [[a]] -> [[a]]
reemplazar fila columna valor matriz =
  let
    --Calculamos la fila elegida y la fila con el valor reemplazado
    filaElegida = matriz !! fila
    filaReemplazada = take columna filaElegida ++ [valor] ++ drop (columna + 1) filaElegida
  in take fila matriz ++ [filaReemplazada] ++ drop (fila + 1) matriz

{-
buscarHueco: Se encarga de encontrar en que fila y columna está el primer hueco ([0..n-1],[0..n-1]).
-}

buscarHueco :: (Enum a1, Enum b, Eq a2, Num a1, Num b, Num a2) => [[a2]] -> (a1, b)
buscarHueco matriz = 
  head ([ (x,y) | (x,fila) <- zip [0..] matriz, (y,elemento) <- zip [0..] fila, elemento == 0])
  
{- 
suc: Obtiene los hijos o sucesores de un nodo futoshiki, he cambiado el nombre debido a que me daba error,
al parecer succ era una función definida en el prelude.
-}

suc :: Futoshiki -> [Futoshiki]
suc futoshiki = 
    let --Uso de creación de listas por compresión para encontrar los tableros que cumplen las reglas con los diferentes valores que existen
      (fila, columna) = buscarHueco(cells futoshiki)
      futoshikisPosibles = [futoshiki{ cells = reemplazar fila columna valor (cells futoshiki)} | valor <- [1.. size futoshiki]]
    --Calculamos y añadimos a la lista mediante compresión aquellos futoshikis que cumplan las reglas.
    in [sucesor | sucesor <- futoshikisPosibles , cumpleReglas (sucesor)]

-- Función de backtracking genérica --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    esSol          suc          n
  | esSol n                           = [n]
  | otherwise                         = concat (map (bt esSol suc) (suc n))

-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]
solve f = map cells (bt esSol suc f)