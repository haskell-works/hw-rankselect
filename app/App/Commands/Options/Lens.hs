{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module App.Commands.Options.Lens where

import App.Commands.Options.Type
import Control.Lens

makeFields ''BuildOptions
makeFields ''SelectAllOptions
makeFields ''UnitTestOptions
makeFields ''ValidateOptions
makeFields ''ValidateState
