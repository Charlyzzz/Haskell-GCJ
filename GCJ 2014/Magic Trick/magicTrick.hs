import Data.List.Split
import Data.List



main = do
	texto <- getContents		
	(escribir.output.solve.sanitizeInput) texto	
   where sanitizeInput = map (map asInt.splitOn " ").splitOn "\n"


escribir = mapM_ (appendFile "results.out".(++"\n"))


output :: [String] -> [String]
output = zipWith (++) (map ((++ ": ").("Case #" ++).show) [1..] )
	


solve :: [[Int]] -> [String]
solve list = solveR cantidadVeces (tail list)
       where cantidadVeces = (head.head) list

solveR :: Int -> [[Int]] -> [String]
solveR 1 lista = [truco  (primerEleccion lista) (segundaEleccion lista) (grilla1 lista) (grilla2 lista)]    

solveR cantidadFaltante lista = truco  (primerEleccion lista) (segundaEleccion lista) (grilla1 lista) (grilla2 lista) : solveR (cantidadFaltante-1) (drop 10 lista)

primerEleccion :: [[Int]] -> Int
primerEleccion (x:xs) = head x

segundaEleccion :: [[Int]] -> Int
segundaEleccion lista = (head.at lista)  6

grilla1 :: [[Int]] -> [[Int]]
grilla1 = grilla 1

grilla2 :: [[Int]] -> [[Int]]
grilla2 = grilla 6

grilla :: Int -> [[Int]] -> [[Int]]
grilla drops  = take 4.drop drops 


truco :: Int -> Int -> [[Int]] -> [[Int]] -> String
truco primerEleccion segundaEleccion grilla1 grilla2 = (resultado.intersect primerFila) segundaFila
    where resultado [x] = show x
          resultado [] = "Volunteer cheated!"
          resultado (x:xs) = "Bad magician!"
          primerFila = grilla1 `at` primerEleccion
          segundaFila = grilla2 `at` segundaEleccion



asInt :: String -> Int
asInt = read

at :: [a] -> Int -> a
at lista a=  lista !! (a-1 )
