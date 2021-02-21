module Talot where
import Graphics.Gloss
    ( greyN,
      yellow,
      color,
      rectangleSolid,
      translate,
      Picture,
      Point )

data Talo = Talo {talo_korkeus :: Float, talo_leveys :: Float, talo_sijainti :: Float}

piirräTalo :: Talo -> Picture
piirräTalo talo = let
                    paikoillaan = translate (talo_sijainti talo) (talo_korkeus talo /2) talonKuva
                    talonKuva = color (greyN 0.5)
                                  (rectangleSolid (talo_leveys talo) (talo_korkeus talo))
                                  -- ovi
                                  <> translate 0 ((- talo_korkeus talo / 2)+40) (rectangleSolid 50 80)
                                  -- ikkunat 6kpl
                                  <> translate (talo_leveys talo / 3) (talo_korkeus talo / 3) (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5))
                                  <> translate (-talo_leveys talo / 3) (talo_korkeus talo / 3) (color yellow (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5)))
                                  <> translate 0 (talo_korkeus talo / 3) (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5))
                                  <> translate (talo_leveys talo / 3) 0 (color yellow (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5)))
                                  <> translate (-talo_leveys talo / 3) 0 (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5))
                                  <> translate 0 0 (rectangleSolid (talo_leveys talo / 5) (talo_korkeus talo / 5))


                    --((vax,vay),(oyx, oyy)) = nurkkaPisteet talo
                    --apupisteet = translate vax vay (color red (circleSolid 10))
                    --              <>  translate oyx oyy (color red (circleSolid 10))
                  in paikoillaan

-- type point = (Float, Float)
nurkkaPisteet :: Talo -> (Point, Point)
nurkkaPisteet talo =
    let
        vasenAla = (talo_sijainti talo - (talo_leveys talo / 2), 0)
        oikeaYlä = (talo_sijainti talo + (talo_leveys talo / 2)     , talo_korkeus talo)
    in (vasenAla, oikeaYlä)

osuukoTaloon :: Float -> Talo -> Float
osuukoTaloon kohta talo
  | abs (talo_sijainti talo - kohta) < (talo_leveys talo / 2)
        = talo_korkeus talo
  | otherwise
        = 0