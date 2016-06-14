module Aaron.Hardy.Pairing.Foo (..) where

{-|
@docs foo, bar
-}

import Aaron.Hardy.James.Pairing.Bar exposing (foo)


{-|
-}
bar : Int
bar =
    12


baz : Int
baz =
    foo + bar
