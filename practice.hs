import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Monad (when)
import Data.Maybe (isNothing, isJust)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> Either String [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    if isJust (buscarVehiculo placaVehiculo parqueadero)
        then Left "El vehículo ya se encuentra en el parqueadero."
        else Right (Vehiculo placaVehiculo tiempo Nothing : parqueadero)

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> Either String [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    if isNothing (buscarVehiculo placaVehiculo parqueadero)
        then Left "El vehículo no se encuentra en el parqueadero."
        else Right (map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero)

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

-- Función para serializar un vehículo a una cadena de texto
serializarVehiculo :: Vehiculo -> String
serializarVehiculo (Vehiculo placa entrada salida) =
    placa ++ "," ++ show entrada ++ "," ++ show salida

-- Función para deserializar una cadena de texto a un vehículo
deserializarVehiculo :: String -> Vehiculo
deserializarVehiculo linea = case splitOn ',' linea of
    [p, e, s] -> Vehiculo p (read e) (read s)
    _         -> error $ "Error de parsing: " ++ linea

-- Función para dividir una cadena en una lista de cadenas utilizando un delimitador
splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
    where
        f c l@(x:xs)
            | c == delimiter = []:l
            | otherwise = (c:x):xs

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    let contenido = unlines (map serializarVehiculo parqueadero)
    catch (bracket (openFile "parqueadero.txt" WriteMode) hClose $ \handle -> do
        hPutStr handle contenido
        putStrLn "Parqueadero guardado en el archivo parqueadero.txt.") manejarExcepciones
    where
        manejarExcepciones :: IOError -> IO ()
        manejarExcepciones e = putStrLn "Error al escribir en el archivo parqueadero.txt. Asegúrese de que el archivo no está en uso."

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    contenido <- catch (readFile "parqueadero.txt") manejarExcepciones
    let lineas = lines contenido
    return (map deserializarVehiculo lineas)
    where
        manejarExcepciones :: IOError -> IO String
        manejarExcepciones e = do
            putStrLn "Error al leer el archivo parqueadero.txt. Asegúrese de que el archivo existe y está accesible."
            return ""

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo (Vehiculo p e s) =
    "Vehículo (Placa = " ++ p ++ ", Hora entrada = " ++ show e ++ ", Hora salida = " ++ show s ++ ")"

-- Función para listar los vehículos en el parqueadero
listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos parqueadero = do
    putStrLn "Lista de vehículos en el parqueadero:"
    mapM_ (putStrLn . mostrarVehiculo) parqueadero

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos en el parqueadero"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case registrarEntrada placaVehiculo tiempoActual parqueadero of
                Left mensaje -> do
                    putStrLn mensaje
                    cicloPrincipal parqueadero
                Right parqueaderoActualizado -> do
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            case registrarSalida placaVehiculo tiempoActual parqueadero of
                Left mensaje -> do
                    putStrLn mensaje
                    cicloPrincipal parqueadero
                Right parqueaderoActualizado -> do
                    putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
                    guardarParqueadero parqueaderoActualizado
                    cicloPrincipal parqueaderoActualizado

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero

        "4" -> do
            listarVehiculos parqueadero
            cicloPrincipal parqueadero

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero
