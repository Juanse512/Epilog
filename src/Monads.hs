module Monads where

import           Common

-- Clases de mónadas que proveen las operaciones necesarias
-- para implementar los evaluadores.

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Name -> [VarT] -> m Exp -- puede ser Ver en vez de Bool
    -- Cambia el valor de una variable
    update :: Name -> [VarT] -> Exp -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

