{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{-

  TODO Escape control characters in line strings

  TODO Handle invalid shapes information in input.

       This expression with every node in shut mode is invalid, for example

       ```
       (+ x y*y z)
       ```

-}

module Rex.Print
    ( blocksFile
    , rexFile
    , rexLine
    )
where

import PlunderPrelude
import Rex.Types

import qualified Data.Text    as T
import qualified Text.Builder as TB


-- Expression ------------------------------------------------------------------

wideLeaf :: TextShape -> Text -> TB.Builder
wideLeaf = f
  where
    f BARE_WORD t = TB.text t
    f THIN_CORD t = TB.char '\'' <> TB.text t <> TB.char '\''
    f THIC_CORD t = TB.char '"'  <> TB.text t <> TB.char '"'
    f THIN_LINE t = f THIN_CORD t -- Coerce to wide form.
    f THIC_LINE t = f THIC_CORD t -- Coerce to wide form.

isShut :: Rex -> Bool
isShut (N SHUT_PREFIX  _ _ _) = True
isShut (N SHUT_INFIX   _ _ _) = True
isShut (C (AS _ _) _)         = True
isShut _                      = False

rexLine :: Rex -> Text
rexLine = TB.run . rexLine'

{-
  TODO Some combinations of shut forms do not need to be wrapped.
-}
wrapRex :: Rex -> TB.Builder
wrapRex x | isShut x = "(" <> rexLine' x <> ")"
wrapRex x            = rexLine' x

rexLine' :: Rex -> TB.Builder
rexLine' = go
 where

  asGo acc []         = acc
  asGo acc ((r,x):is) = asGo (acc <> TB.text r <> wrapRex x) is

  anGo :: [TB.Builder] -> [(Text, Rex)] -> [TB.Builder]
  anGo acc []         = reverse acc
  anGo acc ((r,x):is) = anGo (infixApp x : TB.text r : acc) is

  go :: Rex -> TB.Builder
  go = \case
    T s t Nothing         -> wideLeaf s t
    T s t (Just k)        -> wideLeaf s t <> go k -- TODO Wrap if necessary.
    N OPEN  r ps k        -> go (N NEST_PREFIX r ps k)
    N s     r ps (Just k) -> wrapRex (N s r ps Nothing) <> go k
    C (AS h tl) (Just k)  -> wrapRex (C (AS h tl) Nothing) <> go k
    C (AS h tl) Nothing   -> asGo (wrapRex h) tl
    C (AN h tl) (Just k)  -> wrapRex (C (AN h tl) Nothing) <> go k
    C (AN h tl) Nothing   -> parens $ anGo [infixApp h] tl
    N s     r ps Nothing  ->
      case s of
        SHUT_PREFIX -> TB.text r <> wrapRex (unsafeHead ps)
        SHUT_INFIX  -> intercalate (TB.text r) $ fmap wrapRex ps
        NEST_INFIX  -> parens $ intersperse (TB.text r) (infixApp <$> ps)
        NEST_PREFIX -> case r of
          "|" -> brackets (fancyTail ps)
          "," -> curlies (go <$> ps)
          _   -> brackets (TB.text r : fmap go ps)

  fancyTail []                              = []
  fancyTail [x]                             = [go x]
  fancyTail [x, N NEST_PREFIX r ps Nothing] = go x : TB.text r : fancyTail ps
  fancyTail (x:y:z)                         = go x : fancyTail (y:z)

  parens :: [TB.Builder] -> TB.Builder
  parens [] = "[]"
  parens xs = "(" <> intercalate " " xs <> ")"

  brackets :: [TB.Builder] -> TB.Builder
  brackets xs = "[" <> intercalate " " xs <> "]"

  curlies :: [TB.Builder] -> TB.Builder
  curlies xs = "{" <> intercalate " " xs <> "}"

  infixApp :: Rex -> TB.Builder
  infixApp x@T{}   = go x
  infixApp x@(C{}) = go x
  infixApp x@(N t r ps k) =
    if isApp
    then TB.intercalate " " (go <$> params)
    else go x
   where
    params = (ps <> toList k)
    isApp = case (params, r, t) of
              ([],     _,   _          ) -> False
              (_:_:_,  "|", NEST_PREFIX) -> True
              _                          -> False

{-

""" x
""" y
| x y z
| x y z
""" x
""" y
| x y z
| x y z
9

-}

openLeaf :: Int -> TextShape -> Text -> Maybe Rex -> TB.Builder
openLeaf = end
 where
  end :: Int -> TextShape -> Text -> Maybe Rex -> TB.Builder
  end d THIN_LINE t (Just k) = end d THIN_LINE t Nothing
                            <> ("\n" <> rexFileGo d k)
  end d THIC_LINE t (Just k) = end d THIC_LINE t Nothing
                            <> ("\n" <> rexFileGo d k)
  end d s         t (Just k) = end d s t Nothing <> wrapRexLine (s,t) k

  end _ THIC_LINE t Nothing = TB.text "\"\"\"" <> TB.text t
  end _ THIN_LINE t Nothing = TB.text "'''"    <> TB.text t
  end _ s         t Nothing = wideLeaf s t

  wrapRexLine :: Leaf -> Rex -> TB.Builder
  wrapRexLine l r = if safeJuxtapose l r
                    then rexLine' r
                    else "(" <> rexLine' r <> ")"

  safeJuxtapose :: Leaf -> Rex -> Bool
  safeJuxtapose (BARE_WORD,_) (T BARE_WORD _ _)     = False
  safeJuxtapose _             (N SHUT_PREFIX _ _ _) = False
  safeJuxtapose _             (N NEST_PREFIX _ _ _) = False
  safeJuxtapose _             C{}                   = False
  safeJuxtapose _             _                     = True

indent :: Int -> TB.Builder
indent depth = TB.text (T.replicate depth " ")

fat :: TextShape -> Bool
fat THIC_LINE = True
fat THIN_LINE = True
fat _         = False

lean :: TextShape -> Bool
lean = not . fat

open :: Rex -> Bool
open (N OPEN _ _ _) = True
open _              = False

-- TODO Don't always need the extra newline.
blocksFile :: [Rex] -> Text
blocksFile = loop ""
 where
  loop acc []     = TB.run acc
  loop acc [r]    = loop (acc <> rexFileBuilder r) []
  loop acc (r:rs) = loop (acc <> rexFileBuilder r <> "\n") rs

rexFile :: Rex -> Text
rexFile = TB.run . rexFileBuilder

rexFileBuilder :: Rex -> TB.Builder
rexFileBuilder rex = rexFileGo 0 rex <> "\n"

rexFileGo :: Int -> Rex -> TB.Builder
rexFileGo = go
 where
  go :: Int -> Rex -> TB.Builder
  go d (T s t k)              = indent d <> openLeaf d s t k
  go d (N OPEN r ps Nothing)  = indent d <> TB.text r <> args False (d+2) ps
  go d (N OPEN r ps (Just k)) = indent d <> TB.text r <> args True d (ps<>[k])
  go d x@N{}                  = indent d <> rexLine' x
  go d x@C{}                  = indent d <> rexLine' x

  cont :: Int -> Int -> [Rex] -> TB.Builder
  cont _   _   []     = ""
  cont dep len (x:xs) = concat [ "\n"
                               , go (dep + 2*len) x
                               , cont dep (len-1) xs
                               ]


  stackClosed :: Int -> [Rex] -> TB.Builder
  stackClosed _     []    = ""
  stackClosed depth (x:xs) = concat [ "\n"
                                    , go depth x
                                    , stackClosed depth xs
                                    ]

  -- TODO Rewrite this mess.
  args :: Bool -> Int -> [Rex] -> TB.Builder
  args hasCont depth children =
    case children of
      []              -> arg hasCont depth children
      [_]             -> arg hasCont depth children
      _:_:_ | hasCont ->
        let Just (childs@(c:cs), k) = unsnoc children
            thisWidth = length cs + sum (TB.length . rexLine' <$> childs)
            thisNarrow = thisWidth <= (60-(depth+2))
        in
            case (any isOpen childs, thisNarrow) of
              (True, _) ->
                arg hasCont depth children
              (_, True) ->
                 let hed = arg False (depth+2) childs
                 in hed <> "\n" <> go depth k
              (_, False) ->
                 let stk = " " <> rexLine' c <> stackClosed (depth+2) cs
                 in stk <> "\n" <> go depth k
      _ | narrow  -> arg hasCont depth children
      _ | anyOpen -> arg hasCont depth children
      c:cs        -> " " <> rexLine' c <> stackClosed depth cs
   where
    lineWidth = length children + sum (TB.length . rexLine' <$> children)
    narrow    = lineWidth <= (60-(depth+2))

    anyOpen = any isOpen children

    isOpen (N OPEN _ _ _) = True
    isOpen _              = False

  arg :: Bool -> Int -> [Rex] -> TB.Builder
  arg hasCont depth children =
    r hasCont depth children
   where
    r :: Bool -> Int -> [Rex] -> TB.Builder
    r _     _ []                        = ""
    r True  d [a]                       = "\n" <> go d a
    r False _ [T s t Nothing]  | lean s = " " <> wideLeaf s t
    r True  d [T s t _,k]      | lean s = " " <> wideLeaf s t <> "\n" <> go d k
    r _     d (a@N{} : as)     | open a = cont d (length as) (a:as)
    r _     d (a@(T s _ _):as) | fat s  = cont d (length as) (a:as)
    r f     d (a:as)                    = " " <> rexLine' a <> r f d as

{-
  What formatting decisions do we make?

  At this level?  Only these:

################################################################################
# - x y z
################################################################################
# - longlonglong
#   longlonglong
#   longlonglong
################################################################################
# - f | g :it-fits-aligned
#   | y
# | z
################################################################################
# - f | :it-fits-jagged
# | z
################################################################################
# - f
#   | :it-does-not-fit
# | z
################################################################################
# - f | How about this one?
#         | Idk man.  I mean...
#         | It *can* be done?
#       | Why not?
#       | Maybe this should be restricted to single-line parameters?
#     | Or maybe there should be a maximum width for jagged stuff?
# | z
################################################################################

That we need to provide to higher levels?

Basically just wide-and-tall?

And these decisons need to be made at multi levels at once.

Something can fit wide in one place but, based on layout decisions
outside of it, it can not fit.

Yeah, this requires some sort of decision tree with a grading system.
I have not implemented such a thing before.

################################################################################
(f x)
################################################################################
| f x
################################################################################
| f
| x
################################################################################

Every node tracks (width,height)

There are limits!
  Width should not exceed 60.
    Unless some leaf is extremely large.
    Need to special-case that somehow.

x|y   -> {TIE,3,1}
[x|y] -> {NES,5,1}
(x y) -> {NES,5,1}
| x y -> {OPN , 5 , 1}
         {, OPN 5 1}

= [a= b= c=]
    [a=3 b=4 c=5]
= [a b c]
    [3 4 5]

| f x|y
| x y     -> (OPN,7,2)

| f | x y
| x y     -> (OPN,9,2)

| f
  | x y
| x y     -> (OPN,7,3)
-}
