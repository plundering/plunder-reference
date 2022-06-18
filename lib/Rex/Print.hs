{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE ImplicitParams #-}

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
    , rexFileColor
    , rexLine
    , rexLineColor
    , RexColoring(..)
    , boldColoring
    , noColoring
    )
where

import PlunderPrelude
import Rex.Types

import qualified Data.Text    as T
import qualified Text.Builder as TB

--------------------------------------------------------------------------------

data RexColoring = RC
    { rcOpen :: Text -> TB.Builder
    , rcRune :: Text -> TB.Builder
    , rcText :: TB.Builder -> TB.Builder
    , rcBare :: TB.Builder -> TB.Builder
    , rcNest :: TB.Builder -> TB.Builder
    }

noColoring :: RexColoring
noColoring = RC
    { rcOpen = TB.text
    , rcRune = TB.text
    , rcText = id
    , rcBare = id
    , rcNest = id
    }

boldColoring :: RexColoring
boldColoring = RC
    { rcOpen = boldYellow . TB.text
    , rcRune = \case
        rt | lightRune rt -> boldYellow (TB.text rt)
        rt                -> yellow (TB.text rt)
    , rcText = green
    , rcBare = id
    , rcNest = boldMagenta
    }
  where
    esc code = "\x001b[" <> code <> "m"

    green t       = esc "32"            <> t <> esc "0"
    yellow t      = esc "33"            <> t <> esc "0"
    boldYellow  t = esc "33" <> esc "1" <> t <> esc "0"
    boldMagenta t = esc "35" <> esc "1" <> t <> esc "0"

    lightRune "-" = True
    lightRune "`" = True
    lightRune "." = True
    lightRune _   = False

cNest :: (?color :: RexColoring) => TB.Builder -> TB.Builder
cNest = rcNest where RC{..} = ?color

cRune :: (?color :: RexColoring) => Text -> TB.Builder
cRune rune = rcRune rune where RC{..} = ?color

cOpen :: (?color :: RexColoring) => Text -> TB.Builder
cOpen rune = rcOpen rune where RC{..} = ?color

cText :: (?color :: RexColoring) => TB.Builder -> TB.Builder
cText = rcText where RC{..} = ?color

cBare :: (?color :: RexColoring) => TB.Builder -> TB.Builder
cBare = rcBare where RC{..} = ?color

-- Expression ------------------------------------------------------------------

wideLeaf :: (?color :: RexColoring) => TextShape -> Text -> TB.Builder
wideLeaf = f
  where
    f BARE_WORD t = cBare (TB.text t)
    f THIN_CORD t = cText (TB.char '\'' <> TB.text t <> TB.char '\'')
    f THIC_CORD t = cText (TB.char '"'  <> TB.text t <> TB.char '"')
    f THIN_LINE t = f THIN_CORD t -- Coerce to wide form.
    f THIC_LINE t = f THIC_CORD t -- Coerce to wide form.

isShut :: Rex -> Bool
isShut (N SHUT_PREFIX  _ _ _) = True
isShut (N SHUT_INFIX   _ _ _) = True
isShut (C (AS _ _) _)         = True
isShut _                      = False

rexLine :: Rex -> Text
rexLine = rexLineColor noColoring

rexLineColor :: RexColoring -> Rex -> Text
rexLineColor color =
  let ?color = color
  in TB.run . rexLine'

{-
  TODO Some combinations of shut forms do not need to be wrapped.
-}
wrapRex :: (?color :: RexColoring) => Rex -> TB.Builder
wrapRex x | isShut x = cNest "(" <> rexLine' x <> cNest ")"
wrapRex x            = rexLine' x

rexLine' :: (?color :: RexColoring) => Rex -> TB.Builder
rexLine' = go
 where

  asGo acc []         = acc
  asGo acc ((r,x):is) = asGo (acc <> cRune r <> wrapRex x) is

  anGo :: [TB.Builder] -> [(Text, Rex)] -> [TB.Builder]
  anGo acc []         = reverse acc
  anGo acc ((r,x):is) = anGo (infixApp x : cRune r : acc) is

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
        SHUT_PREFIX -> cRune r <> wrapRex (unsafeHead ps)
        SHUT_INFIX  -> intercalate (cRune r) (wrapRex <$> ps)
        NEST_INFIX  -> parens $ intersperse (cRune r) (infixApp <$> ps)
        NEST_PREFIX -> case r of
          "|" -> brackets (fancyTail ps)
          "," -> curlies (go <$> ps)
          _   -> brackets (cRune r : fmap go ps)

  fancyTail []                              = []
  fancyTail [x]                             = [go x]
  fancyTail [x, N NEST_PREFIX r ps Nothing] = go x : cRune r : fancyTail ps
  fancyTail (x:y:z)                         = go x : fancyTail (y:z)

  parens :: [TB.Builder] -> TB.Builder
  parens [] = cNest "[]"
  parens xs = cNest "(" <> intercalate " " xs <> cNest ")"

  brackets :: [TB.Builder] -> TB.Builder
  brackets xs = cNest "[" <> intercalate " " xs <> cNest "]"

  curlies :: [TB.Builder] -> TB.Builder
  curlies xs = cNest "{" <> intercalate " " xs <> cNest "}"

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

openLeaf :: (?color :: RexColoring) => Int -> TextShape -> Text -> Maybe Rex -> TB.Builder
openLeaf = end
 where
  end :: Int -> TextShape -> Text -> Maybe Rex -> TB.Builder
  end d THIN_LINE t (Just k) = end d THIN_LINE t Nothing
                            <> ("\n" <> rexFileGo d k)
  end d THIC_LINE t (Just k) = end d THIC_LINE t Nothing
                            <> ("\n" <> rexFileGo d k)
  end d s         t (Just k) = end d s t Nothing <> wrapRexLine (s,t) k

  end _ THIC_LINE t Nothing = cText (TB.text "\"\"\"" <> TB.text t)
  end _ THIN_LINE t Nothing = cText (TB.text "'''"    <> TB.text t)
  end _ s         t Nothing = wideLeaf s t

  wrapRexLine :: Leaf -> Rex -> TB.Builder
  wrapRexLine l r = if safeJuxtapose l r
                    then rexLine' r
                    else cNest "(" <> rexLine' r <> cNest ")"

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
blocksFile :: (?color :: RexColoring) => [Rex] -> Text
blocksFile = loop ""
 where
  loop acc []     = TB.run acc
  loop acc [x]    = loop (acc <> rexFileBuilder x) []
  loop acc (x:xs) = loop (acc <> rexFileBuilder x <> "\n") xs

rexFile :: Rex -> Text
rexFile = rexFileColor noColoring

rexFileColor :: RexColoring -> Rex -> Text
rexFileColor rc rex = TB.run (rexFileBuilder rex)
                                where ?color = rc

rexFileBuilder :: (?color :: RexColoring) => Rex -> TB.Builder
rexFileBuilder rex = rexFileGo 0 rex <> "\n"

rexFileGo :: (?color :: RexColoring) => Int -> Rex -> TB.Builder
rexFileGo = igo
 where
  igo :: Int -> Rex -> TB.Builder
  igo d x                      = indent d <> go d x

  go :: Int -> Rex -> TB.Builder
  go d (T s t k)              = openLeaf d s t k

  -- TODO Generalize (I want this behavior now, but code is too complex,
  -- so I'm hacking it)
  go d (N OPEN r [p,q] mK)       | isOpen p && length r < 3
                                 = cOpen r
                                <> TB.text (T.replicate (6 - length r) " ")
                                <> go (d+6) p
                                <> "\n"
                                <> igo (d+3) q
                                <> case mK of
                                     Nothing -> ""
                                     Just k  -> "\n" <> igo d k

  go d (N OPEN r [p] mK)       | isOpen p && length r < 3
                                = cOpen r
                               <> TB.text (T.replicate (3 - length r) " ")
                               <> go (d+3) p
                               <> case mK of
                                      Nothing -> ""
                                      Just k  -> "\n" <> igo d k
  go d (N OPEN r ps Nothing)  = cOpen r <> args False (d+3) ps
  go d (N OPEN r ps (Just k)) = cOpen r <> args True d (ps<>[k])
  go _ x@N{}                  = rexLine' x
  go _ x@C{}                  = rexLine' x

  cont :: Int -> Int -> [Rex] -> TB.Builder
  cont _   _   []     = ""
  cont dep len (x:xs) = concat [ "\n"
                               , igo (dep + 3*len) x
                               , cont dep (len-1) xs
                               ]


  stackClosed :: Int -> [Rex] -> TB.Builder
  stackClosed _     []    = ""
  stackClosed depth (x:xs) = concat [ "\n"
                                    , igo depth x
                                    , stackClosed depth xs
                                    ]

  isOpen (N OPEN _ _ _) = True
  isOpen _              = False

  -- TODO Rewrite this mess.
  args ::  Bool -> Int -> [Rex] -> TB.Builder
  args hasCont depth children =
    case children of
      []              -> arg hasCont depth children
      [_]             -> arg hasCont depth children
      _:_:_ | hasCont ->
        let Just (childs@(c:cs), k) = unsnoc children
            thisWidth = length cs + sum (TB.length . rexLine' <$> childs)
            thisNarrow = thisWidth <= (60-(depth+3))
        in
            case (any isOpen childs, thisNarrow) of
              (True, _) ->
                arg hasCont depth children
              (_, True) ->
                 let hed = arg False (depth+3) childs
                 in hed <> "\n" <> igo depth k
              (_, False) ->
                 let stk = " " <> rexLine' c <> stackClosed (depth+3) cs
                 in stk <> "\n" <> igo depth k
      _ | narrow  -> arg hasCont depth children
      _ | anyOpen -> arg hasCont depth children
      c:cs        -> " " <> rexLine' c <> stackClosed depth cs
   where
    lineWidth = length children + sum (TB.length . rexLine' <$> children)
    narrow    = lineWidth <= (60-(depth+3))

    anyOpen = any isOpen children

  arg :: Bool -> Int -> [Rex] -> TB.Builder
  arg hasCont depth children =
    r hasCont depth children
   where
    r :: Bool -> Int -> [Rex] -> TB.Builder
    r _     _ []                        = ""
    r True  d [a]                       = "\n" <> igo d a
    r False _ [T s t Nothing]  | lean s = " " <> wideLeaf s t
    r True  d [T s t _,k]      | lean s = " " <> wideLeaf s t <> "\n" <> igo d k
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

{-------------------------------------------------------------------------------

:= [foldl a b c]
|
               ? [loop a b c d e]
               | if gte-e-c d
               @ f
                  [a d get-b-e]
               [seq f | loop a b c f 3-e]
            a
         c
      len-c
   b
0

>>> := [foldl a b c]
>>> |     ? [loop a b c d e]
>>>       | if gte-e-c d
>>>       @ f [a d get-b-e]
>>>       | seq f
>>>       | loop a b c f 3-e
>>>    a
>>>    c
>>>    len-c
>>>    b
>>>    0

:= [idx a b]
@ c
   |  ? [loop a b]
      | if isHed-b 0:0
      @ c [loop a appHead-b]
      | if appHead-c c
      @ d appTail-c
      | if eql-a-d [1 appTail-b] [0 3-d]
   a
   b
[if appHead-c appTail-c 0]

>>> := [idx a b]
>>> @ c
>>>    |  ? [loop a b]
>>>       | if isHed-b 0:0
>>>       @ c [loop a appHead-b]
>>>       | if appHead-c c
>>>       @ d appTail-c
>>>       | if eql-a-d [1 appTail-b] [0 3-d]
>>>    a
>>>    b
>>> | if appHead-c appTail-c 0



-------------------------------------------------------------------------------}
