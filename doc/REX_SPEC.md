The Rex Grammar
===============

```
namechar = [a-zA-Z_]
runechar = [$!#%&*+,-./:<=>?@\\^`|~]

/"""(.*)/     -> P # Page
/'''(.*)/     -> P # Page
/;(.*)/       -> C # Comment
/(namechar+)/ -> N # Name
/(runechar+)/ -> R # Rune
/( +)/        -> S # Whitespace
/'([^']*)'/   -> T # Text
/"([^"]*)"/   -> T # Text

leaf = P | T | N
shut = (nest | leaf)+
shin = shut (R shut)*
form = R shin | shin
frag = R shin | R | shin

# In infix mode, always occurs after `form`.
plix = ')'
     | R S form plix
     | S ')'
     | S R S form plix
     | S form plix

# In prefix mode, always occurs after `R` or `form`.
pree = ')'
     | S ')'
     | S R pree
     | S form pree

# Body of parenthesis, decides if infix or prefix
prest = ')'
      | form plix
      | R pree

# Body of [bracket expression]
broke = ']'
      | S ']'
      | S frag broke

# Body of {curly expression}
carl = '}'
     | S '}'
     | S frag carl

# [Any] (nested) {expression}
nest = '(' prest
     | '(' S prest
     | '[' broke
     | '[' frag broke
     | '{' carl
     | '{' frag carl

# Open space (can include line-comment)
open = C
     | S C
     | S

loan = EOF
     | open EOF
     | open frag loan

lean = EOF
     | frag loan

line = open lean
     | lean
```
