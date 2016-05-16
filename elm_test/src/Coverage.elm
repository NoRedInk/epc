module Coverage (tick) where

{-|
@docs tick
-}

import Native.Coverage

import Combine exposing (parse)

import Coverage.Parser exposing (tix)
import Coverage.Types exposing (Tix(Tix), updateTixModule)

{-|-}
tick : String -> Int -> a -> a
tick str n x =
  Native.Coverage.readTix str
    |> parse tix
    |> fst
    |> \result -> case result of
      Combine.Done (Tix tixModules) ->
        Tix (List.map (updateTixModule str n) tixModules)
          |> toString
          |> Native.Coverage.tick x str
      Combine.Fail _ ->
        x
