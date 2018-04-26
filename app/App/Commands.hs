module App.Commands where

import App.Commands.Build
import App.Commands.SelectAll
import App.Commands.Validate
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdBuild
  <>  cmdSelectAll
  <>  cmdValidate
