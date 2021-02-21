module Kopteri where
import Graphics.Gloss
import Aritmetiikka
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle


data Kopteri = Kopteri {
       kop_paikka :: (Float, Float)        -- Kopterin sijainti 
      ,kop_nopeus :: (Float, Float)        -- Kuinka nopeasti?
      ,kop_teho   :: Float                 -- Teho
      ,kop_kulma  :: Float                 -- Missä kulmassa?
      ,kop_pelastettujaKyydissä :: Natural -- Kuinka monta pelastettu?
}

data Törmäyskohta = Laskuteline | Roottori deriving (Eq, Ord)

luoKopteri :: Point -> Kopteri
luoKopteri paikka
    = Kopteri
       paikka
       (0,0) -- sijainti
       0 -- teho
       0 -- kulma
       0 -- pelastettavaa 

päivitäKopteria :: Float -> Natural -> Kopteri -> Kopteri
päivitäKopteria aikaEdellisestä lisääPelastettavia kopteri
  = kopteri{
            kop_paikka = (kopteriX + aikaEdellisestä * vX
                         , max 0 (kopteriY + aikaEdellisestä * vY))
            , kop_nopeus = ((vX + dX) * 0.97 , (vY + dY - 10) * 0.97)
            , kop_pelastettujaKyydissä = (kop_pelastettujaKyydissä kopteri + lisääPelastettavia)
            }
    where
      (kopteriX, kopteriY) = kop_paikka kopteri
      (vX, vY) = kop_nopeus kopteri
      (dX, dY) = kulmaJaTehoNopeudeksi (kop_teho kopteri) (kop_kulma kopteri)

kallista :: Float -> Kopteri -> Kopteri
kallista muutos kopteri = kopteri{kop_kulma = muutos + kop_kulma kopteri}

muutaTehoa :: Float -> Kopteri -> Kopteri
muutaTehoa muutos kopteri = kopteri{kop_teho = muutos + kop_teho kopteri}

laskeudu :: Kopteri -> Kopteri
laskeudu kopteri = kopteri{kop_kulma=0, kop_nopeus=pysäytäPystyssä (kop_nopeus kopteri)}

onkoHyväLaskeutuminen :: Kopteri -> Bool
onkoHyväLaskeutuminen Kopteri{kop_nopeus = nopeus, kop_kulma = kulma}
    | magV nopeus < 80 && abs kulma <= 30 = True
    | otherwise = False


kopteriTörmäysviivat :: Kopteri -> ((Point, Point) , (Point, Point))
kopteriTörmäysviivat kopteri = 
  let
    paikka = kop_paikka kopteri
    kulma = kop_kulma kopteri
    vasen = -170
    oikea = 100
    kääntö = rotateV (-degToRad kulma)
  in ( (kääntö (vasen,0) #+ paikka
       ,kääntö (oikea,0) #+ paikka)
       ,
       (kääntö (vasen,120) #+ paikka
       ,kääntö (oikea,120) #+ paikka)
     )

kulmaJaTehoNopeudeksi :: Float -> Float -> (Float, Float)
kulmaJaTehoNopeudeksi teho kulma
  = rotateV (- degToRad kulma) (0,teho)

piirräKopteri :: Float -> Kopteri -> Picture
piirräKopteri aika Kopteri{kop_teho = teho, kop_kulma = kulma, kop_paikka = (kopteriX, kopteriY)} 
    = translate kopteriX kopteriY 
      . rotate kulma
      . scale 0.4 0.4
      . translate 0 (150)
      . color white
      $ runko
  where
    runko = rectangleSolid 180 170 -- pohja kopterille
            <> translate (-200) 0 (rectangleSolid 300 40) --pyrstö
            <> translate (-350) 0 (rotate (-30) (rectangleSolid 50 100)) --pyrstön "pää"
            <> lapa
            <> lapa2 
            <> translate 0 90     (rectangleSolid 20 120) -- lavan "jalka"
            
            -- kopterin keula
            -- ei ihan täydellinen mutta kelvannee
            <> translate 110 43 (rotate 120 (rectangleSolid 40 100)) 
            <> translate 110 (-43) (rotate 240 (rectangleSolid 40 100))
            <> translate 120 0 (rectangleSolid 100 55)
            
            <> translate (-110) 43 (rotate (-120) (rectangleSolid 40 100))
            <> translate (-110) (-43) (rotate (120) (rectangleSolid 40 100))
            <> translate (-120) 0 (rectangleSolid 100 55)

            -- laskuteline
            <> translate (-50) (-90)   (rectangleSolid 10 120)
            <> translate (50) (-90)    (rectangleSolid 10 120)
            <> translate 0 (-150)      (rectangleSolid 240 15)
    lapa = translate 0 150 (rectangleSolid (500 * sin (aika * teho)) 10)
    lapa2 = translate (-350) 0 (rotate (200 * (aika*teho)) (rectangleSolid 200 8)) --pyrstön lapa