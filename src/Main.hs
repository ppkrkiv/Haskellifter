module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List ( partition )
import Aritmetiikka
import Talot
import Pelastettavat
import Kopteri
import Puut

alkutilanne :: PeliTila
alkutilanne =
    GameOn
      (Peli
       0
       (luoKopteri (0,0))
       [Talo 800 500 700,
        Talo 450 400 (-500),
        Talo 700 600 (-1500),
        Talo 400 350 1200]
       [Pelastettava (700, 800),
       Pelastettava (900, 800),
       Pelastettava (-1600, 700),
       Pelastettava (1300, 400),
       Pelastettava (-600, 450)]
       [Puu 300 70 300,
        Puu 540 85 (-900),
        Puu 400 80 1600,
        Puu 450 82 1820,
        Puu 390 78 2100,
        Puu 290 50 (-2100)]
      )

main :: IO ()
main = play
        (InWindow "Choplifter" (1500,700) (200,200))
        (light blue)
        60 -- ruudunpäivitys kertaa/s
        alkutilanne
        piirräPeliTila
        reagoiPeliTila
        päivitäPeliTila

reagoiPeliTila :: Event -> PeliTila -> PeliTila
reagoiPeliTila tapahtuma pelitila
    = case pelitila of
        GameOver cl -> GameOver cl
        GameOn cl   -> GameOn (reagoi tapahtuma cl)

reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli
    = case tapahtuma of
        EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa (1.8)) peli
        EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-1.8)) peli
        EventKey (Char 'a') Down _ _ -> kopterille (kallista (-6)) peli
        EventKey (Char 'd') Down _ _ -> kopterille (kallista (6)) peli
        _ -> peli

päivitäPeliTila :: Float -> PeliTila -> PeliTila
päivitäPeliTila aikaEdellisestä pelitila
    = case pelitila of
        GameOver peli -> GameOver peli
        GameOn   peli -> case törmääköTaloon (kopteriTörmäysviivat (cl_kopteri peli)) (cl_talot peli) of
                        Nothing          -> GameOn (päivitäPeliä aikaEdellisestä peli)
                        Just Roottori    -> GameOver peli
                        Just Laskuteline
                            | onkoHyväLaskeutuminen (cl_kopteri peli)
                              -> GameOn (päivitäPeliä aikaEdellisestä (kopterille laskeudu peli))
                            | otherwise -> GameOver peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)}

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
  = case edellinenTila of
      Peli aika kopteri talot pelastettavat puut
          -> let
                paikka = kop_paikka kopteri
                nouseekoKyytiin pelastettava = magV (pelastettava_sijainti pelastettava #- (kop_paikka kopteri)) < 50
                (pelastettavatKopteriin, pelastettavatUlkona) = partition nouseekoKyytiin pelastettavat
             in Peli (aika + aikaEdellisestä)
                      (päivitäKopteria aikaEdellisestä (genericLength pelastettavatKopteriin) kopteri)
                      talot
                      (map (päivitäPelastettavaa (flip korkeusKohdassa edellinenTila) paikka) pelastettavatUlkona)
                      puut

törmääköTaloon :: ((Point, Point), (Point, Point)) -> [Talo] -> Maybe Törmäyskohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
    where
      törmääköYhteen talo
        = let
            ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat
            (va,oy) = nurkkaPisteet talo
          in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
                (True,False)  -> Just Laskuteline
                (False,False) -> Nothing
                _             -> Just Roottori

piirräPeliTila :: PeliTila -> Picture
piirräPeliTila pelitilanne
    = case pelitilanne of
        GameOver cl -> piirräPeli cl <> translate (-200) 0 (color red (text "CRASH!"))
        GameOn cl -> piirräPeli cl

piirräPeli :: Choplifter -> Picture
piirräPeli peli = let
                   aika = cl_aika peli
                   talot = cl_talot peli
                   puut = cl_puut peli

                   kopterikuva = piirräKopteri aika (cl_kopteri peli)

                   pelastettavaKuvat = map (piirraPelastettava aika) (cl_pelastettavat peli)
                   taloKuvat = map piirräTalo talot
                   puuKuvat = map piirräPuu puut
                   peliKuva = kopterikuva
                                        <> maa
                                        <> pictures taloKuvat
                                        <> pictures pelastettavaKuvat
                                        <> laskeutumisAlusta
                                        <> pictures puuKuvat

                  in scale 0.3 0.3 (translate 0 (-180) peliKuva)
-- translate (x - kopterinX) (y - kopterinY)
data PeliTila = GameOver Choplifter | GameOn Choplifter

data Choplifter
  = Peli
    {
       cl_aika   :: Float                   -- aika peli alusta
      ,cl_kopteri :: Kopteri                -- Kopterin tiedot
      ,cl_talot :: [Talo]                   -- Talot pelissä
      ,cl_pelastettavat :: [Pelastettava]   -- pelastettavat hahmot
      ,cl_puut :: [Puu]
    }

korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli =
   maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli

maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

laskeutumisAlusta :: Picture
laskeutumisAlusta = color (greyN 0.4) (translate 0 (-5) (rectangleSolid 300 10))
                    <> translate 150 4 (color red (circleSolid 6))
                    <> translate (-150) 4 (color red (circleSolid 6))

