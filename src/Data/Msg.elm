module Data.Msg exposing (Msg(..))

import Data.Generator exposing (GeneratorFields(..))
import Data.Installment exposing (Fields(..))


type Msg
    = AddInstallment
    | DelInstallment Int
    | UpdateInstallment Int Fields String
    | UpdateGenerator GeneratorFields String
    | Generate
