module Puut where
import Graphics.Gloss


data Puu = Puu {puu_korkeus :: Float, puu_leveys :: Float, puu_sijainti :: Float}

piirräPuu :: Puu -> Picture
piirräPuu puu = let 
                  paikallaan = translate (puu_sijainti puu) (puu_korkeus puu / 2) puunKuva
                  puunKuva = color (makeColor 0.7 0.3 0.2 1)
                              (rectangleSolid (puu_leveys puu) (puu_korkeus puu)) --puun runko
                              <> translate 0 ((puu_korkeus puu / 2)+50) (color (makeColor 0.3 0.8 0.3 1) (circleSolid 100))
                              <> translate (puu_leveys puu) ((puu_korkeus puu / 2)+10) (color (makeColor 0.3 0.8 0.3 1) (circleSolid 100))
                              <> translate (-puu_leveys puu) ((puu_korkeus puu / 2)+10) (color (makeColor 0.3 0.8 0.3 1) (circleSolid 100))
                              <> translate 0 ((puu_korkeus puu / 2)-50) (color (makeColor 0.3 0.8 0.3 1) (circleSolid 100))
                in paikallaan

