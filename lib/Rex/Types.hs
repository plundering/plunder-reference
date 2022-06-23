{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Types
    ( RuneShape(..)
    , TextShape(..)
    , GRex(..)
    , Leaf
    , Rex
    )
where

import PlunderPrelude

data RuneShape
    = OPEN
    | NEST_PREFIX
    | NEST_INFIX
    | SHUT_PREFIX
    | SHUT_INFIX
  deriving (Eq, Ord, Show, Generic, NFData)

data TextShape
    = BARE_WORD  --  foo
    | THIC_CORD  --  "foo"
    | THIN_CORD  --  'foo'
    | THIC_LINE  --  """foo
    | THIN_LINE  --  '''foo
  deriving (Eq, Ord, Show, Generic, NFData)

type Leaf = (TextShape, Text)

data GRex v
    = N RuneShape Text [GRex v] (Maybe (GRex v))
    | T TextShape Text (Maybe (GRex v))
    | C v (Maybe (GRex v))
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

type Rex = GRex Void
