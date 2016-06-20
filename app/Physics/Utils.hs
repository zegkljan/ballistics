module Physics.Utils where

import           Physics

homogScalarField :: Double -> ScalarField
homogScalarField = const

homogVectorField :: Vec3D -> VectorField
homogVectorField = const
