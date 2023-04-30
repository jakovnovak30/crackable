{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel

data Status = Status { naslov :: String, crackanost :: String } deriving (Show)
type Naslov = String
type Crackanost = String
type Url = String

 -- provjeriStatus :: Trazimo -> Bool
 -- provjeriStatus searchFor
 --   | nadeno == "Cracked" = True
 --   | otherwise           = False
 --   where nadeno = najdi searchFor
 
najdi :: String -> IO(Maybe [Status])
najdi searchFor = scrapeURL url statusi
 where
  url = buildUrl searchFor

  statusi :: Scraper String [Status]
  statusi = chroots ("div" @: [hasClass "elementor-post__card"]) status 

  status :: Scraper String Status
  status = do
   crackanost <- chroot ("div" @: [hasClass "elementor-post__badge"]) $ text "div"
   naslov     <- chroot ("h3"  @: [hasClass "elementor-post__title"]) $ text "a"
   return $ Status naslov crackanost

buildUrl :: String -> Url
buildUrl searchFor =
 "https://cwwatch.net/?s=" ++ query
 where query = prviDio ++ drugiDio
       prviDio = head polje
       drugiDio = concat $ map ("+" ++) $ tail  polje
       polje = words searchFor

poruka :: Bool -> String
poruka moguce
 | moguce    = "Igrica se da crackati"
 | otherwise = "EA te opet sjebal, kurve proklete"

main :: IO()
main = do
 putStrLn "Napisite ime igrice koju zelite crackati"
 searchFor <- getLine
 putStrLn "Work in progress"
 -- putStrLn $ poruka $ provjeriStatus searchFor
