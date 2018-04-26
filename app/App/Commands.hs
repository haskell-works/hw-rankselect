module App.Commands where

import App.Commands.Build
import App.Commands.SelectAll
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdBuild
  <>  cmdSelectAll
