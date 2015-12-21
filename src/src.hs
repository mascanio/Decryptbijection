import Data.Char

--El programa sirve para, dado un texto codificado con una biyeccion entre caracteres,
--dar informacion sobre el texto y herramientas para modificarlo. Se puede tanto codificar
--como decodificar, intercambiando caracteres en un texto normal. Se pueden tanto intercambiar
--caracteres existentes en el texto como por caracteres que no están. Por ejemplo, si en el
--texto no hay j, se puede sustituir por esa letra. Para empezar el programa llamar a main.


--Llamada inicial, lee el fichero original e inicia el bucle principal.
main = do 
    putStrLn "Inserta el nombre del archivo."
    filePath <- getLine
    texto <- readFile filePath
    procesaTexto texto texto
    return ()
    
--Bucle principal. Da una serie de opciones para analizar y modificar el texto.
procesaTexto textoOriginal textoProcesado = do
    imprimeMenu
    op <- getLine
    putStrLn ""
    case op of
        --Mostrar texto original.
        "1" -> do
            putStrLn textoOriginal
            putStrLn ""
            procesaTexto textoOriginal textoProcesado
        --Mostrar texto procesado.
        "2" -> do
            putStrLn textoProcesado
            putStrLn ""
            procesaTexto textoOriginal textoProcesado  
        --Mostrar posible biyección sobre el texto original.
        "3" -> do
            posibleBiyeccion textoOriginal
            procesaTexto textoOriginal textoProcesado
		--Mostrar posible biyección sobre el texto procesado.
        "4" -> do
            posibleBiyeccion textoProcesado
            procesaTexto textoOriginal textoProcesado
        --Frecuencia de caracteres en el texto original.
        "5" -> do
            let frecuencias = calculaFrecuenciaCaracter textoOriginal
            putStrLn (imprimeFrecuenciasCaracter frecuencias)
            procesaTexto textoOriginal textoProcesado
        --Frecuencia de palabras en el texto original.
        "6" -> do
            let frecuencias = calculaFrecuenciaPalabras textoOriginal
            putStrLn (imprimeFrecuenciasPalabra frecuencias)
            procesaTexto textoOriginal textoProcesado
        --Frecuencia de caracteres en el texto procesado.
        "7" -> do
            let frecuencias = calculaFrecuenciaCaracter textoProcesado
            putStrLn (imprimeFrecuenciasCaracter frecuencias)
            procesaTexto textoOriginal textoProcesado
        --Frecuencia de palabras en el texto procesado.
        "8" -> do
            let frecuencias = calculaFrecuenciaPalabras textoProcesado
            putStrLn (imprimeFrecuenciasPalabra frecuencias)
            procesaTexto textoOriginal textoProcesado
        --Cambiar caracter por otro manualmente
        "9" -> do
            putStrLn "Inserta el caracter que quieres cambiar."
            i <- getChar
            getLine
            putStrLn "Inserta el caracter por el que quieres cambiarlo."
            j <- getChar
            getLine
            case (elem j textoProcesado) of
                True -> do
                    putStrLn "El caracter por el que quieres cambiarlo ya está en el texto, sustituye por otro. \n"
                    procesaTexto textoOriginal textoProcesado
                False -> do
                    let textoProcesadoAux = sustituyeTexto textoProcesado i j
                    putStrLn textoProcesadoAux
                    putStrLn ""
                    procesaTexto textoOriginal textoProcesadoAux
        --Intercambia dos caracteres que aparezcan en el texto.
        "10" -> do
            putStrLn "Inserta el caracter que quieres intercambiar."
            i <- getChar
            getLine
            putStrLn "Inserta el caracter por el que quieres intercambiarlo."
            j <- getChar
            getLine
            case (((elem j textoProcesado) && (elem i textoProcesado)) && ((i /= '@') && (j /= '@'))) of
                True -> do
                    let textoProcesadoAux = intercambia i j textoProcesado
                    putStrLn textoProcesadoAux
                    putStrLn ""
                    procesaTexto textoOriginal textoProcesadoAux
                False -> do
                    putStrLn "Uno de los caracteres no está en el texto o no es un caracter válido. \n"
                    procesaTexto textoOriginal textoProcesado
        --Reiniciar texto
        "11" -> do
            procesaTexto textoOriginal textoOriginal
        --Salir
        "0" -> do
            return ()
        _ -> do
            putStrLn "La opcion no es valida. \n"
            procesaTexto textoOriginal textoProcesado

---------------------------------------
--Imprimimos el menú con esta función--
---------------------------------------
imprimeMenu:: IO ()
imprimeMenu = do
 putStrLn "Inserta una opcion:"
 putStrLn "1.  Mostrar texto original."
 putStrLn "2.  Mostrar texto procesado."
 putStrLn "3.  Mostrar posible biyección sobre el texto original."
 putStrLn "4.  Mostrar posible biyección sobre el texto procesado." 
 putStrLn "5.  Frecuencia de caracteres en el texto original." 
 putStrLn "6.  Frecuencia de palabras en el texto original."
 putStrLn "7.  Frecuencia de caracteres en el texto procesado."
 putStrLn "8.  Frecuencia de palabras en el texto procesado."
 putStrLn "9. Cambiar caracter manualmente."
 putStrLn "10. Intercambia dos caracteres que aparezcan en el texto."
 putStrLn "11. Reiniciar texto."
 putStrLn "0. Salir."
 return ()

----------------------------
--Funciones de sustitución--
----------------------------
 
--Cambiamos en el texto el caracter 'i' por el caracter 'j' 
sustituyeTexto:: String -> Char -> Char -> String
sustituyeTexto [] i j = []
sustituyeTexto (x:xs) i j = (map ((changeLetter i j)) (x:xs)) where
 changeLetter m n r = if comparaCaracter m r then esMayus r n  else r
 esMayus i j = if isUpper i then toUpper j else toLower j 
 comparaCaracter i j = if toUpper i == toUpper j then True else False

 --Intercambia el caracter i por el caracter j
intercambia:: Char -> Char -> String -> String
intercambia i j []  = []
intercambia i j (x:xs) 
 | x == i = [j] ++ intercambia i j xs
 | x == j = [i] ++ intercambia i j xs
 |otherwise = [x] ++ intercambia i j xs
-------------------------------------------
--Funciones para frecuencia de caracteres--
-------------------------------------------

--Calculamos las frecuencias para cada caracter:
calculaFrecuenciaCaracter:: String -> [(Char, Integer)]
calculaFrecuenciaCaracter (x:xs) = qsort [(y, frecuenciaLetra y (x:xs))| y <- ['a'..'z']] where 
 --Calcula la frecuencia sobre una letra.
 frecuenciaLetra:: Char -> String -> Integer
 frecuenciaLetra i (x:xs)
  | (length xs) == 0 = if ((toLower x == i) || (toUpper x == i)) then 1 else 0 
  | (toLower x == i) || (toUpper x == i) = 1 + frecuenciaLetra i xs
  | otherwise = frecuenciaLetra i xs

--Imprime las frecuencias de los caracteres:
imprimeFrecuenciasCaracter:: [(Char, Integer)] -> String
imprimeFrecuenciasCaracter [] = ""
imprimeFrecuenciasCaracter (x:xs) = "La frecuencia de " ++ show (fst x) ++ " es: " ++ show (snd x) ++ "\n" ++ imprimeFrecuenciasCaracter xs

-----------------------------------------
--Funciones para frecuencia de palabras--
-----------------------------------------

--Imprime las frecuencias de las palabras:
imprimeFrecuenciasPalabra:: [(String, Integer)] -> String
imprimeFrecuenciasPalabra [] = ""
imprimeFrecuenciasPalabra (x:xs) = "La frecuencia de " ++ show (fst x) ++ " es: " ++ show (snd x) ++ "\n" ++ imprimeFrecuenciasPalabra xs

--Calculamos las frecuencias para cada palabra y las devuelve en una lista.:
calculaFrecuenciaPalabras (x:xs) = qsort (listaFrecuenciaPalabras (listaPalabras (toLowerString (x:xs)))) where
 listaFrecuenciaPalabras:: [String] -> [(String, Integer)]
 listaFrecuenciaPalabras (x:xs) = [(y, frecuenciaPalabra y (x:xs))| y <- eliminaRepetidos (x:xs)]
 --Esta función devuelve una lista con las palabras contenidas en el texto.
 listaPalabras:: String -> [String]
 listaPalabras (x:xs) = words (quitaCaracteresNoAlfabéticos (x:xs)) where
  --Esta función elimina los caracteres no afabéticos en un texto.
  quitaCaracteresNoAlfabéticos:: String -> String
  quitaCaracteresNoAlfabéticos [] = "" 
  quitaCaracteresNoAlfabéticos (x:xs)
   | (elem x (['a'..'z'] ++ " ") || (elem x (['A'..'Z'] ++ " "))) = [x] ++ quitaCaracteresNoAlfabéticos xs
   | otherwise = quitaCaracteresNoAlfabéticos xs
 --Esta función calcula la frecuencia de apariciones de una palabra en el texto.
 frecuenciaPalabra:: String -> [String] -> Integer
 frecuenciaPalabra palabra (x:xs)
  | (length xs) == 0 = if palabra == x then 1 else 0
  | palabra == x = 1 + frecuenciaPalabra palabra xs
  | palabra /= x = frecuenciaPalabra palabra xs
 --Esta función hace que todas las letras de un string sean minúsculas.
 toLowerString:: String -> String
 toLowerString [] = ""
 toLowerString (x:xs)
  | isUpper x = [(toLower x)] ++ toLowerString xs
  | otherwise = [x] ++ toLowerString xs
 --Esta función elimina los elementos repetidos en una lista.
 eliminaRepetidos:: Eq a => [a] -> [a]
 eliminaRepetidos [] = []
 eliminaRepetidos (x:xs) 
  | elem x xs = eliminaRepetidos xs
  | not(elem x xs) = [x] ++ eliminaRepetidos xs

-------------------------
--Función de ordenación--
-------------------------
--Ordena listas de tuplas en función del segundo elemento.
qsort:: Ord b => [(a, b)] -> [(a, b)]
qsort [] = []
qsort (x:l) = qsort [y | y <- l, snd y > snd x] ++ [x] ++ qsort [y | y <- l, snd y <= snd x]

-----------------------------
--Mostrar posible biyección--
-----------------------------
--Devuelve la posible biyección aplicada en el texto.
posibleBiyeccion (x:xs) = do
 let frec1 = calculaFrecuenciaCaracter (x:xs)
 putStrLn (imprimePosibleBiyección (qsort (frec1)) listaFrecuenciasEspañol)
 return () where 
 --Devyelve un string con la posible biyeccion del texto.
 imprimePosibleBiyección:: [(Char, Integer)] -> [(Char, Double)] -> String
 imprimePosibleBiyección [] [] = ""
 imprimePosibleBiyección (x:xs) (y:ys) = show (fst x) ++ " --> " ++ show (fst y) ++ "\n" ++ imprimePosibleBiyección xs ys

 --Contiene la lista con las frecuencias de las letras en español.
listaFrecuenciasEspañol:: [(Char, Double)]
listaFrecuenciasEspañol = qsort [('a', 12.53), ('b', 1.42), ('c', 4.68), ('d', 5.86), ('e', 13.68), ('f', 0.69), ('g', 1.01), ('h', 0.70), ('i', 6.25), ('j', 0.44), ('k', 0.01), ('l', 4.97), ('m', 3.15), ('n', 6.71), ('o', 8.68), ('p', 2.51), ('q', 0.88), ('r', 6.87), ('s', 7.98), ('t', 4.63), ('u', 3.93), ('v', 0.90), ('w', 0.02), ('x', 0.22), ('y', 0.90), ('z', 0.52)]
