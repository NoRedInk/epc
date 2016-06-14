module Trace.Epc.Mix where

import AST.Declaration (Decl(Decl), Declaration(Definition))
import AST.Module (Header(Header), Module(Module), body, header, name)
import AST.V0_16 (Commented(Commented))
import AST.Variable (Ref(VarRef))
import Control.Monad ((>=>))
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime)
import Reporting.Region (Position(Position), Region(Region), column, end, line, start)
import Trace.Hpc.Mix (BoxLabel(TopLevelBox), Mix(Mix), MixEntry)
import Trace.Hpc.Util (HpcPos, toHash, toHpcPos)

import qualified AST.Pattern as P
import qualified Reporting.Annotation as A

mkMix :: FilePath -> UTCTime -> [MixEntry] -> Mix
mkMix filepath time entries = Mix filepath time (toHash entries) 1 entries

regionToHpcPos :: Region -> HpcPos
regionToHpcPos Region
    { start = Position {line = l1, column = c1}
    , end = Position {line = l2, column = c2}
    } = toHpcPos (l1, c1, l2, c2)

moduleName :: Module -> [String]
moduleName Module {header = Header {name = Commented _ strs _}} = strs

countTopLevel :: Module -> Int
countTopLevel = length . topLevels

topLevels :: Module -> [A.Located Declaration]
topLevels Module {body = decls} = mapMaybe declToDefinition decls

mixEntries :: Module -> [MixEntry]
mixEntries Module {body = decls} = mapMaybe declToMixEntry decls

declToMixEntry :: Decl -> Maybe MixEntry
declToMixEntry = declToDefinition >=> declarationToMixEntry

declarationToMixEntry :: A.Located Declaration -> Maybe MixEntry
declarationToMixEntry (A.A region (Definition (A.A _ (P.Var (VarRef str))) _ _ _)) = Just (regionToHpcPos region, TopLevelBox [str])
declarationToMixEntry _ = Nothing

declToDefinition :: Decl -> Maybe (A.Located Declaration)
declToDefinition (Decl located) = isDefinition located
declToDefinition _ = Nothing

isDefinition :: A.Located Declaration -> Maybe (A.Located Declaration)
isDefinition def@(A.A _ Definition {}) = Just def
isDefinition _ = Nothing
