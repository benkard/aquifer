{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Model
    ( Drink(..)
    , drinkId
    , drinkDescription
    , drinkTimestamp
    , drinkLiters
    , DrinkDB(..)
    , Model(..)
    , drinkDB
    , drinkDialogActive
    ) where

import ClassyPrelude
import Control.Category.Unicode ((∘))
import Control.Lens
import qualified Data.Aeson.TH as AesonTH
import qualified Data.Scientific as Scientific
import qualified Data.Time.Clock as Clock

data Drink = Drink
  { _drinkId          ∷ Int32
  , _drinkDescription ∷ Text
  , _drinkTimestamp   ∷ Clock.UTCTime
  , _drinkLiters      ∷ Scientific.Scientific
  }
$(makeLenses ''Drink)
$(AesonTH.deriveJSON (AesonTH.defaultOptions{AesonTH.fieldLabelModifier = toLower ∘ (drop 6)}) ''Drink)

data DrinkDB = DrinkDB { unDrinkDB ∷ [Drink] }
$(makeLenses ''DrinkDB)
$(AesonTH.deriveJSON AesonTH.defaultOptions ''DrinkDB)

data Model = Model
  { _drinkDB           ∷ DrinkDB
  , _drinkDialogActive ∷ Bool
  }
$(makeLenses ''Model)
