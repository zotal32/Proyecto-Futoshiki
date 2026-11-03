module Main where

import Control.Exception (catch, evaluate, IOException)
import System.CPUTime
import System.IO
import Text.Printf
import Text.Read (readMaybe)

import Futoshiki

data Sol = None | Multiple | Single (Matrix Int)
  deriving (Eq, Read, Show)
type TestFutoshiki = (String,Futoshiki,Sol)

-- FUNCIÓN QUE LEE EL FICHERO DE TESTS --
readTests :: FilePath -> IO [String]
readTests    f         = do putStr "Cargando tests... "
                            m <- readMaybeFile f
                            case m of
                              Nothing      -> do putStrLn "No existe el fichero."
                                                 return []
                              Just content -> let output = lines content in do
                                              putStrLn (show(length output)++" tests cargados.")
                                              return (output)

-- FUNCIÓN QUE INTENTA LEER UN FICHERO Y CAPTURA POSIBLES ERRORES --
readMaybeFile :: FilePath -> IO (Maybe String)
readMaybeFile    path      = catch (Just <$> readFile path) handler
  where
    handler :: IOException -> IO (Maybe String)
    handler    _            = return Nothing

-- FUNCIÓN QUE EJECUTA LOS TESTS LEÍDOS (SI HAY TESTS) --
executeTests :: [String] -> Bool   -> IO ()
executeTests    []          result  = do putStr "Fin de los tests, "
                                         if result then putStrLn "todos los tests han sido superados."
                                         else putStrLn "ha habido tests con fallos."
executeTests    (x:xs)      result  = do r <- checkTest x
                                         executeTests xs r

-- FUNCIÓN QUE COMPRUEBA UN TEST --
checkTest :: String -> IO Bool
checkTest    s       = do case readMaybe s :: Maybe TestFutoshiki of
                            Nothing          -> do putStrLn "Error de formato del test."
                                                   return True
                            Just (id,f,eSol) -> do putStr ( "Test \"" ++ id ++ "\" " )
                                                   start <- getCPUTime
                                                   result <- evaluate ( checkSolution f eSol )
                                                   end <- getCPUTime
                                                   let diffMs  = fromIntegral (end-start) / 1.0e9 :: Double
                                                   if result then do
                                                     printf "correcto (%.3f ms).\n" diffMs
                                                     return True
                                                   else do printf "fallido (%.3f ms).\n" diffMs
                                                           return False

-- FUNCIÓN QUE COMPRUEBA SI LA RESPUESTA DADA POR EL PROGRAMA SE CORRESPONDE CON LA ESPERADA --
checkSolution :: Futoshiki -> Sol       -> Bool
checkSolution    f            None       = (solve f) == []
checkSolution    f            (Single m) = (solve f) == [m]
checkSolution    f            Multiple   = checkMultiple (solve f)
  where checkMultiple :: [Matrix Int] -> Bool
        checkMultiple    (x:y:zs)      = True
        checkMultiple    _             = False

-- "BUCLE" PRINCIPAL --
loop :: IO ()
loop  = do putStr "Teclee el nombre del fichero de tests a cargar: "
           hFlush stdout
           fileName <- getLine
           if ( fileName /= "" )
              then do tests <- readTests fileName
                      executeTests tests True
                      loop
              else do putStr "Fin del testeo.\n"

-- FUNCIÓN PRINCIPAL --
main :: IO ()
main  = do hSetBuffering stdin LineBuffering
           loop
           

