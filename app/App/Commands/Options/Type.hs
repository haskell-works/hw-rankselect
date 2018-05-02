module App.Commands.Options.Type where

import Data.Word

data IndexType = CsPoppy | Poppy512 deriving (Eq, Read, Show)

data BuildOptions = BuildOptions
  { _buildOptionsIndexType :: IndexType
  } deriving (Eq, Show)

data SelectAllOptions = SelectAllOptions
  { _selectAllOptionsIndexType :: IndexType
  } deriving (Eq, Show)

newtype UnitTestOptions = UnitTestOptions
  { _unitTestOptionsName :: String
  } deriving (Eq, Show)

data ValidateOptions = ValidateOptions
  { _validateOptionsIndexType :: IndexType
  , _validateOptionsFile      :: FilePath
  } deriving (Eq, Show)

data ValidateState = ValidateState
  { _validateStateCumulativePopCount :: Word64
  } deriving (Eq, Show)

emptyValidateState :: ValidateState
emptyValidateState = ValidateState 0
