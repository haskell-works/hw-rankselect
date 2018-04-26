module App.Commands.Options.Type where

data IndexType = CsPoppy | Poppy512 deriving (Eq, Read, Show)

data BuildOptions = BuildOptions
  { _buildOptionsIndexType :: IndexType
  } deriving (Eq, Show)

data SelectAllOptions = SelectAllOptions
  { _selectAllOptionsIndexType :: IndexType
  } deriving (Eq, Show)
