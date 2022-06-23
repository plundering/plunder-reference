{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Rex
    ( module Rex
    , RuneShape(..)
    , TextShape(..)
    , Leaf
    , GRex(..)
    , Rex
    , rexFile
    , rexFileColor
    , rexLine
    , rexLineColor
    , boldColoring
    , noColoring
    )
where

import Rex.Block as Rex
import Rex.Print
import Rex.ReadT as Rex
import Rex.Types
