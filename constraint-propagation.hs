{-# LANGUAGE
    AllowAmbiguousTypes,
    BangPatterns,
    ConstraintKinds,
    DataKinds,
    DeriveFunctor,
    DeriveGeneric,
    DeriveTraversable,
    DerivingVia,
    DuplicateRecordFields,
    EmptyCase,
    ExplicitNamespaces,
    FlexibleContexts,
    FlexibleInstances,
    GADTs,
    GeneralisedNewtypeDeriving,
    KindSignatures,
    LambdaCase,
    MultiParamTypeClasses,
    NamedFieldPuns,
    NoMonomorphismRestriction,
    OverloadedLabels,
    OverloadedStrings,
    PackageImports,
    PartialTypeSignatures,
    PatternSynonyms,
    QuantifiedConstraints,
    RankNTypes,
    RecordWildCards,
    RecursiveDo,
    ScopedTypeVariables,
    StandaloneDeriving,
    TupleSections,
    TypeApplications,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    TypeSynonymInstances,
    UndecidableInstances,
    UndecidableSuperClasses,
    ViewPatterns
 #-}

import           Data.Typeable
import           Data.Kind (Constraint)

-- | Dependency: All
--
-- Nearly verbatim from Data.SOP.Constraint:
class Top x
instance Top x
type SListI = All Top
class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k]) where
instance All c '[] where
instance (c x, All c xs) => All c (x ': xs) where
type family
  AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF _c '[]       = ()
  AllF  c (x ': xs) = (c x, All c xs)

-- | Actual problematic code
--
data GADT (xs :: [*]) where
  GADT :: All Typeable xs
    => GADT xs

data Some where
  Some :: forall xs. All Typeable xs
    => GADT xs -> Some

mapSome
  :: All Typeable xs'
  => Proxy xs'
  -> (forall (xs :: [*])
      . (All Typeable xs, All Typeable xs')
      => Proxy xs' -> GADT xs -> GADT xs')
  -> Some
  -> Some
mapSome p f some =
  case some of
    Some x -> Some (f p x)

mapSome'
  :: All Typeable xs'
  => (forall (xs :: [*])
      . (All Typeable xs)
      => GADT xs -> GADT xs')
  -> Some
  -> Some
mapSome' f some =
  case some of
    Some x -> Some (f x)

mapSome''
  :: (forall (xs :: [*]) (xs' :: [*])
      . (All Typeable xs, All Typeable xs')
      => GADT xs -> GADT xs')
  -> Some
  -> Some
mapSome'' f some =
  case some of
    Some x -> Some (f x)

-- mapSome
  -- (fe :: forall xss xss'. (All Typeable xss, All Typeable xss') => GADT xss -> GADT xss')
  -- (Some (x :: GADT xs))
  -- fe
  -- some
  -- = case some of
  --     Some x -> Some (fe x)

  -- ((Some-- :: (All Typeable xs') => GADT xs' -> Some
  --    )
  --    ((fe
  --      (x :: (All Typeable xs) => GADT xs))
  --     ::     (All Typeable xs') => GADT xs'))
