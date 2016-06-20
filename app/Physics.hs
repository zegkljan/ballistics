module Physics where

import           Data.Vec as V

{-|
  Interval around zero that is considered to be zero.
-}
doubleZero :: Double
doubleZero = 1e-6

{-|
  A scalar field.
  An alias for a function from 3D vector to a scalar value.
-}
type ScalarField = Vec3D -> Double

{-|
  A vector field.
  An alias for a function from 3D vector to 3D vector.
-}
type VectorField = Vec3D -> Vec3D

{-|
  Description of the projectile.
-}
data Projectile = Projectile {
  radius   :: Double, -- ^ radius of the projectile ball [meter]
  mass     :: Double, -- ^ mass of the projectile [kilogram]
  dragCoef :: Double  -- ^ aerodynamic drag coefficient of the projectile
} deriving (Show)

{-|
  Position and velocity coupled together.
  It is essentialy a state of a flying projectile.
-}
data PosVel = PosVel {
  position :: Vec3D, -- ^ position of the projectile [meter]
  velocity :: Vec3D  -- ^ velocity of the projectile [meter / second]
} deriving (Show)

{-|
  Description of the environment.
-}
data World = World {
  gravity      :: VectorField, -- ^ gravitational field of the environment [meter / second^2]
  aeroDensity  :: Double,      -- ^ density of the air [kilogram / meter^3]
  aeroVelocity :: VectorField  -- ^ velocity field of the air (wind velocity field) [meter / second^2]
}

{-|
  Settins of the simulation.
-}
data SimSettings = SimSettings {
  sampleTime :: Double, -- ^ length of a simulation time step [second]
  maxSamples :: Int     -- ^ maximum number of simulation samples
} deriving (Show)

type StoppingCondition = World -> Projectile -> PosVel -> Bool

simulate :: SimSettings -> World -> StoppingCondition -> Projectile -> PosVel -> [PosVel]
simulate s w cond proj pv = undefined

step :: SimSettings -> World -> StoppingCondition -> Projectile -> PosVel -> Maybe PosVel
step ss w scond proj pv = let acc = acceleration proj pv w
                              ts = pack (vec (sampleTime ss))
                              nv = (ts * acc) + velocity pv
                              np = (ts * velocity pv) + position pv
                              npv = PosVel { position = np,
                                             velocity = nv }
                           in if scond w proj npv
                              then Nothing
                              else Just npv

{-|
  Computes the total acceleration of the projectile.
-}
acceleration :: Projectile
             -> PosVel     -- ^ position + velocity
             -> World      -- ^ world
             -> Vec3D      -- ^ total acceleration
acceleration proj pv w = let m = mass proj
                             cs = crossSection proj
                             dc = dragCoef proj
                             pos = position pv
                             vel = velocity pv
                             aeroVel = aeroVelocity w pos
                             g = gravity w pos
                             aeroDrag = aeroDragForce (aeroDensity w) aeroVel vel cs dc
                          in g + (aeroDrag / pack (vec m))

{-|
  Computes the force of aerodynamic drag acting on the projectile.
-}
aeroDragForce :: Double -- ^ air density
              -> Vec3D  -- ^ velocity vector of the air (wind) [meter / second]
              -> Vec3D  -- ^ projectile velocity vector [meter / second]
              -> Double -- ^ projectile effective cross-section [meter^2]
              -> Double -- ^ projectile aerodynamic drag coefficient [-]
              -> Vec3D  -- ^ aerodynamic force vector [Newton]
aeroDragForce density aeroV projV crossSection dragCoef =
  let relV     = unpack (aeroV - projV)
      relVsq   = normSq relV
      unitRelV = normalize relV
      res      = 1.0 / 2.0 * density * crossSection * dragCoef * relVsq
   in if norm relV <= doubleZero
      then Vec3D 0 0 0
      else pack (vec res * unitRelV)


{-|
  Computes the cross-section of the projectile.
-}
crossSection :: Projectile
             -> Double -- ^ [meter^2]
crossSection p = pi * (radius p ** 2)
