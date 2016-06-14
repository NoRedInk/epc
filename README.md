# epc - elm program coverage

PoC that provides program coverage for elm. Built upon hpc.

# Installing

You will need haskell to build this project. You can install with [stack](1)

Clone the repo with `--recursive` to install [submodules](2):

```
git clone --recursive git@github.com:NoRedInk/epc.git
```

If you forget to add `--recursive`, you can still install submodules manually from the top level of the repo:

```
git submodule init
git submodule update
```

Once everything is installed, you can build the repo:

```
stack build
```

## Usage

After the build finishes, you can run the program:

```
stack exec epc elm_test/Aaron/Hardy/Pairing/Foo.elm
```

Note that currently the program works with a single module.

The output will be:

```
.epc
├── Aaron.Hardy.Pairing.Foo.mix
├── Aaron.Hardy.Pairing.Foo.tix
└── Instrumented
    └── Aaron
        └── Hardy
            └── Pairing
                └── Foo.elm
```

The source module is rewritten to compile in elm with the function calls wrapped in the `tick` function:

```elm
module Instrumented.Aaron.Hardy.Pairing.Foo (..) where

{-|
@docs foo, bar
-}

import Coverage


{-|
-}
foo : Int
foo =
    3


{-|
-}
bar : Int
bar =
    12


baz : Int
baz =
    (Coverage.tick "Aaron.Hardy.Pairing.Foo" 0 foo) + (Coverage.tick "Aaron.Hardy.Pairing.Foo" 1 bar)

```

[1]: http://docs.haskellstack.org/en/stable/README/#how-to-install
[2]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
