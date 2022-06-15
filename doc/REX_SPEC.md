The Rex Grammar
===============

```
namechar = [a-zA-Z_]
runechar = [$!#%&*+,-./:<=>?@\\^`|~]

/"""(.*)/     -> PAGE
/'''(.*)/     -> PAGE
/;(.*)/       -> NOTE
/(namechar+)/ -> NAME
/(runechar+)/ -> RUNE
/( +)/        -> WHYT
/'([^']*)'/   -> TEXT
/"([^"]*)"/   -> TEXT

leaf = PAGE | TEXT | NAME
shut = (nest | leaf)+
shin = shut (RUNE shut)*
form = RUNE shin | shin
frag = RUNE shin | RUNE | shin

faro = ')'
     | RUNE WHYT form faro
     | WHYT ')'
     | WHYT RUNE WHYT form faro
     | WHYT form faro

para = '(' WHYT? form faro

brok = ']' | WHYT ']' | WHYT frag brok
brak = '[' WHYT? (( ']' || frag brok ))

carl = form (( '}' || WHYT (( '}' || carl )) ))
curl = '{' WHYT? (( '}' || carl ))

nest = para | brak | curl

whyt = NOTE | (WHYT NOTE?)

loan = EOF | whyt EOF | whyt frag loan
line = whyt? (EOF | frag loan)
```
