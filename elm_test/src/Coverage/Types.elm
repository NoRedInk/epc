module Coverage.Types where

type Tix
  = Tix (List TixModule)

type TixModule
  = TixModule String Int Int (List Int)

updateTick : Int -> List number -> List number
updateTick n =
  List.indexedMap (\i x -> if i == n then x + 1 else x)

updateTixModule : String -> Int -> TixModule -> TixModule
updateTixModule str n (TixModule m hash len ticks) =
  if str == m then
    TixModule m hash len (updateTick n ticks)
  else
    TixModule m hash len ticks
