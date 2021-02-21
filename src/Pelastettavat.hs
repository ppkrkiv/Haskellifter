module Pelastettavat where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Aritmetiikka

data Pelastettava = Pelastettava {pelastettava_sijainti :: Point}

--                    x        y
haluaakoLiikkua :: (Float -> Float) -> Point -> Pelastettava -> Bool
haluaakoLiikkua korkeusKohdassa kopterinPaikka pelastettava = haluaaLiikkua && not putoaako
  where
    putoaako = abs (korkeusEdessä - snd (pelastettava_sijainti pelastettava)) > 50
    korkeusEdessä = korkeusKohdassa (fst (pelastettava_sijainti pelastettava) + suunta * 2)
    haluaaLiikkua = magV (kopterinPaikka #- pelastettava_sijainti pelastettava) < 600
    suunta = minneMenisi kopterinPaikka pelastettava

minneMenisi :: Point -> Pelastettava -> Float
minneMenisi kopterinPaikka pelastettava
        | fst kopterinPaikka < fst (pelastettava_sijainti pelastettava)
            = -15
        | otherwise
            = 15

päivitäPelastettavaa :: (Float -> Float) -> Point -> Pelastettava -> Pelastettava
päivitäPelastettavaa korkeusKohdassa kopterinPaikka pelastettava 
        | haluaakoLiikkua korkeusKohdassa kopterinPaikka pelastettava
            = pelastettava{pelastettava_sijainti = pelastettava_sijainti pelastettava #+ (suunta,0)}
        | otherwise
            = pelastettava
    where
      suunta = minneMenisi kopterinPaikka pelastettava

piirraPelastettava :: Float -> Pelastettava -> Picture
piirraPelastettava aika pelastettava = let 
                                  (x,y) = pelastettava_sijainti pelastettava
                                  lantio = (15, 40)
                                  vasenJalka = 9+sin (20*aika) * 7
                                  oikeaJalka = 5+cos (20*aika) * 7
                                  pelastKuva = color white
                                    (translate 0 110 (circleSolid 20) --pää
                                      <> line [(0,100), lantio] -- selkä
                                      <> line [(-45,90 + cos (20*aika+3) * 40),(-30,90), (30,90)
                                              , (40,90 + cos (20*aika) * 40)] -- kädet
                                      <> line [(-25,vasenJalka), (-20,vasenJalka) 
                                              , lantio
                                              , (30,oikeaJalka), (35,oikeaJalka)] --jalat
                                    )
                                  in translate x y pelastKuva