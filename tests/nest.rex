= 'Sugar for functions and tuples'
, (add 3 4)
, [add 3 4]
, [| add 3 4]
, {3 4}
, [, 3 4]
, [~]

= 'Grouped Infix'
, (nat*nat > nat > nat)

= 'Flexible spacing for parens'
, (3, 4, 5)
, ( 3, 4, 5)
, ( 3, 4, 5 )
, ( 3 , 4, 5 )
, ( 3 , 4 , 5 )
, (  3  ,  4  ,  5  )

= 'Flexible spacing for brackets'
, [f x]
, [ f x]
, [f x ]
, [ f x ]
, [ f  x ]
, [| f x]
, [|  f x]
, [| f x ]
, [|  f x ]
, [|  f  x ]
, [ | f x]
, [ |  f x]
, [ | f x ]
, [ |  f x ]
, [ |  f  x ]

= 'Some edge-cases'
, [#[V3 1] a b]
, (+3 + +4)
, (=x, =y)
