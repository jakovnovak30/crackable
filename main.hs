import Text.HTML.Scalpel

provjeriStatus :: String -> Bool
provjeriStatus "jedi survivor" = False
provjeriStatus _               = True

poruka :: Bool -> String
poruka moguce 
  | moguce    = "Igrica se da crackati"
  | otherwise = "EA te opet sjebal, kurve proklete"

main :: IO()
main = do
  putStrLn "Napisite ime igrice koju zelite crackati"
  searchFor <- getLine
  putStrLn $ poruka $ provjeriStatus searchFor
