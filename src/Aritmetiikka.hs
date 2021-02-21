module Aritmetiikka where
import Graphics.Gloss.Data.Vector ( Vector )
import Graphics.Gloss.Data.Point ( Point )

(#+) :: Point -> Vector -> Point
(a,b) #+ (x,y) = (a+x,b+y)

(#-) :: Point -> Point -> Vector
(a,b) #- (x,y) = (a-x,b-y)

pysäytäPystyssä :: Vector -> Vector
pysäytäPystyssä (vx, vy) = (vx, max 0 vy)