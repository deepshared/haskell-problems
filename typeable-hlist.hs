{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import Data.Typeable
import Data.Kind
import Data.SOP.Constraint
import Generics.SOP
import Generics.SOP.Dict

data
 TP :: (k -> Type) -> [k] -> Type where
  Til  :: TP f '[]
  (:#) :: (Typeable x, Typeable xs, Typeable (x ': xs))
       => f x -> TP f xs -> TP f (x ': xs)

infixr 5 :#

data T (xs :: [*]) where
  TC ::
    { tcxs :: TP I xs
    } -> T xs

data N (xs :: [*]) where
  NC ::
    { tcxs :: NP I xs
    } -> N xs

data WT (xs :: [*]) where
  WTC ::
    { wct  :: T xs
    } -> WT xs

instance Show (WT (xs :: [*])) where
  show (WTC (TC x@(Til :: TP I xs))) = show $ typeOf x
  show (WTC (TC (x :# xs)))
    =  show (typeOf x)  -- this is available through normal "All Typeable xs"
    <> show (typeOf xs) -- ..this is the problem.

spineConstraint :: forall k (xs :: [k]) . (Typeable k, All Typeable xs) => Dict Typeable xs
spineConstraint = cpara_SList (Proxy @Typeable) Dict (\ Dict -> Dict)

data WN (xs :: [*]) where
  WNC :: All Typeable xs =>
    { wct  :: N xs
    } -> WN xs

instance Show (WN (xs :: [*])) where
  show (WNC (NC x@(Nil :: NP I xs))) = show $ typeOf x
  show (WNC (NC (x :* (xs :: NP I xs'))))
    =  show (typeOf x)  -- this is available through normal "All Typeable xs"
    <> case spineConstraint of
         (Dict :: Dict Typeable xs') -> show (typeOf xs)
