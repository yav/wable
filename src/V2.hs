module V2 where

type Radians  = Float
type Scalar   = Float
data V2       = V2 Scalar Scalar deriving Show

pointwise :: (Scalar -> Scalar -> Scalar) -> V2 -> V2 -> V2
pointwise f (V2 x y) (V2 a b) = V2 (f x a) (f y b)

add :: V2 -> V2 -> V2
add = pointwise (+)

sub :: V2 -> V2 -> V2
sub = pointwise (-)

rotate :: Radians -> V2 -> V2
rotate a (V2 x y) = V2 (ca * x - sa * y) (sa * x + ca * y)
  where ca = cos a
        sa = sin a

len :: V2 -> Scalar
len (V2 x y) = sqrt (x * x + y * y)

asPair :: V2 -> (Scalar,Scalar)
asPair (V2 x y) = (x,y)


