{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecursiveDo, ScopedTypeVariables, ConstraintKinds, MultiWayIf, LambdaCase #-}
module Lib
    ( main
    ) where

import ClassyPrelude
import GHCJS.DOM.Types (toJSString, fromJSString)
import GHCJS.Types (JSString)
import GHCJS.Foreign ()
import Data.JSString ()
import Reflex
import Reflex.Dom
import Reflex.Host.Class (HostFrame)
import Prelude.Unicode ((≡), (≥))
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Format as TimeFormat
import qualified Data.Map
import qualified GHCJS.DOM.Storage as DOMStorage
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as DOMWindow
import qualified GHCJS.DOM.Element as DOMElement
import Data.String.Here.Uninterpolated (here)

import Model (Drink(..), drinkId, drinkDescription, drinkTimestamp, drinkLiters, DrinkDB(..))

foreign import javascript unsafe "console.log($1)" js_log :: JSString -> IO ()

numberInput ∷ MonadWidget t m ⇒ Event t () → m (Event t (), Dynamic t (Maybe Scientific.Scientific))
numberInput clearEvent = do
    input ← elAttr "span" (Data.Map.fromList [("class", "ui large fluid right labeled left icon input")]) $ do
      input ← textInput $ def & textInputConfig_inputType    .~ "number"
                              & textInputConfig_attributes   .~ (constDyn (Data.Map.fromList [("placeholder", "amount"), ("pattern", "[0-9]*")]))
                              & setValue                     .~ fmap (const "") clearEvent
      elAttr "div" (Data.Map.fromList [("class", "ui label")]) $ do
        text "ml"
      elAttr "i" (Data.Map.fromList [("class", "coffee icon")]) $ do
        return ()
      return input
    n ← mapDyn readMay $ _textInput_value input
    let ev = textInputGetEnter input
    return (ev, n)

bigPlusButton ∷ MonadWidget t m ⇒ m (Event t ())
bigPlusButton = do
  (e, _) ← elAttr "span" (Data.Map.fromList [("class", "ui fluid input")]) $ do
    elAttr' "button" (Data.Map.fromList [("class", "ui large fluid huge green labeled icon button")]) $ do
      elAttr "i" (Data.Map.fromList [("class", "add icon")]) $ return ()
      text "add drink"
  return $ domEvent Click e

uiButton ∷ MonadWidget t m ⇒ String → m a → m (Event t ())
uiButton extraClasses inner = do
  (e, _) ← elAttr' "button" (Data.Map.fromList [("class", "ui " ++ extraClasses ++ " button")]) $
    inner
  return $ domEvent Click e

combineDyn3 ∷ (Reflex t, MonadHold t m) ⇒ (a → b → c → d) → Dynamic t a → Dynamic t b → Dynamic t c → m (Dynamic t d)
combineDyn3 f d1 d2 d3 = do
    d1d2 ← combineDyn ((,)) d1 d2
    combineDyn (\(d1',d2') d3' → f d1' d2' d3') d1d2 d3

drinkInput ∷ MonadWidget t m ⇒ Event t () → Event t () → m (Event t (), Dynamic t (Maybe (Text, Scientific.Scientific, UTCTime)))
drinkInput focusEvent clearEvent = elAttr "div" (Data.Map.fromList [("class", "ui green segment")]) $ do
    -- time
    startTime ← liftIO $ Clock.getCurrentTime
    tick ← tickLossy 1.0 startTime
    currentTime ← holdDyn startTime (map _tickInfo_lastUTC tick)
    -- description input
    descInput ← elAttr "span" (Data.Map.fromList [("class", "ui large fluid left icon input")]) $ do
      input ← textInput $ def & textInputConfig_attributes .~ (constDyn (Data.Map.fromList [("placeholder", "drink")]))
                              & setValue                   .~ fmap (const "") clearEvent
      elAttr "i" (Data.Map.fromList [("class", "book icon")]) $ return ()
      return input
    let descEv = textInputGetEnter descInput
    let desc = _textInput_value descInput
    performEvent_ $ fmap (const $ liftIO $ DOMElement.focus $ _textInput_element descInput) focusEvent
    --
    el "br" $ return ()
    -- amount input
    (amountEv, amount) ← numberInput clearEvent
    -- output
    drinkComponents ← combineDyn3
      (\desc' amount' currentTime' →
         case amount' of
           Just amount'' → Just (pack desc', amount'', currentTime')
           Nothing       → Nothing)
      desc amount currentTime
    let ev = descEv `appendEvents` amountEv
    return (ev, drinkComponents)

css ∷ String
css = [here|
@import url(https://fonts.googleapis.com/css?family=Lato:400,400italic,300italic,300,700italic,700,100,100italic,900italic,900&subset=latin,latin-ext);
@viewport {
  width: device-width;
}
@-ms-viewport {
  width: device-width;
}
@-o-viewport {
  width: device-width;
}
html {
  font-family: 'Lato', sans-serif; 
}
.fullwidth {
  width: 100%;
}
|]

js ∷ String
js = [here|
document.addEventListener("DOMContentLoaded", function (event) {
  jQuery(function($) {
    $('.ui.modal')
      .modal('setting', 'closable', false);
  });
});
|]

buildHead ∷ Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) ()
buildHead = do
    elMeta "viewport" "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    elMeta "apple-mobile-web-app-capable"          "yes"
    elMeta "apple-mobile-web-app-status-bar-style" "black"
    elMeta "mobile-web-app-capable"                "yes"
    elMeta "msapplication-tap-highlight"           "no"
    elAttr "script" (Data.Map.fromList [("type", "text/javascript"),
                                        ("src",  "https://code.jquery.com/jquery-2.2.0.min.js")]) $ return ()
    elAttr "script" (Data.Map.fromList [("type", "text/javascript"),
                                        ("src",  "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.8/semantic.min.js")]) $ return ()
    --elAttr "script" (Data.Map.fromList [("type", "text/javascript"),
    --                                    ("src",  "https://cdnjs.cloudflare.com/ajax/libs/webcomponentsjs/0.7.20/webcomponents.min.js")]) $ return ()
    elAttr "script" (Data.Map.fromList [("type", "text/javascript"),
                                        ("src",  "https://cdnjs.cloudflare.com/ajax/libs/webcomponentsjs/0.7.20/MutationObserver.min.js")]) $ return ()
    elAttr "link"   (Data.Map.fromList [("type", "text/css"),
                                        ("href", "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.8/semantic.min.css"),
                                        ("rel",  "stylesheet")]) $ return ()
    _ ← elDynHtml' "style" (constDyn css)
    _ ← elDynHtml' "script" (constDyn js)
    return ()
  where
    elMeta name content = do
      void $ elAttr "meta" (Data.Map.fromList [("name",    name), 
                                               ("content", content)]) $ return ()

main ∷ IO ()
main = mainWidgetWithHead buildHead $ do
    Just window  ← liftIO $ DOM.currentWindow
    Just storage ← liftIO $ DOMWindow.getLocalStorage window
    rawStoredDB  ← liftIO $ DOMStorage.getItem storage ("drinks" ∷ JSString)
    let loadedDB = rawStoredDB >>= \jsstr → Aeson.decode (encodeUtf8 (pack (fromJSString jsstr)))
    let initialDrinkDB = maybe (DrinkDB []) id loadedDB
    today ← Clock.utctDay <$> (liftIO $ Clock.getCurrentTime)
    let yesterday = Calendar.addDays (-1) today

    -- view part 1: drink input
    inputDrinkComponentsInputEv ← mdo
      inputModalAttrs ← forDyn modalActive $ \modalActive' →
        Data.Map.fromList [("class", if modalActive' then "ui basic modal transition visible active" else "ui basic modal transition hidden")]
      pageDimmerAttrs ← forDyn modalActive $ \modalActive' →
        Data.Map.fromList [("class", if modalActive' then "ui dimmer modals page transition visible active" else "ui dimmer modals page transition hidden")]
      modalActive ← foldDyn ($) False $ mergeWith (.)
                      [ fmap (\_ → not) toggleButtonClick
                      , fmap (\_ _ → False) cancelClick
                      , fmap (\_ _ → False) approveClick
                      , fmap (\_ _ → False) inputSubmit
                      ]
      toggleButtonClick ← elAttr "div" ("class" =: "ui fixed bottom right sticky fluid fullwidth") $ do
        bigPlusButton
      (cancelClick, approveClick, inputSubmit, inputDrinkComponents) ← elDynAttr "div" pageDimmerAttrs $ do
        elDynAttr "div" inputModalAttrs $ do
          elAttr "div" ("class" =: "header") $ do
            text "Add drink"
          (inputSubmit', inputDrinkComponents') ← elAttr "div" ("class" =: "content") $ do
            drinkInput toggleButtonClick inputDrinkSubmitEv
          (cancelClick'', approveClick'') ← elAttr "div" ("class" =: "actions ui grid") $ do
            cancelClick' ← uiButton "large four wide column cancel" $ do
              text "Cancel"
            approveClick' ← uiButton "large ten wide column positive approve" $ do
              text "Add"
            return (cancelClick', approveClick')
          return (cancelClick'', approveClick'', inputSubmit', inputDrinkComponents')
      let inputDrinkSubmitEv = approveClick `appendEvents` inputSubmit
      let inputDrinkComponentsInputEv = tag (current inputDrinkComponents) inputDrinkSubmitEv 
      return inputDrinkComponentsInputEv

    -- model
    drinkDB ← foldDyn (\maybeInput drinkDB →
                         maybe
                           drinkDB
                           (\(inputDesc, inputMilliliters, inputTime) →
                              let minimalUnusedID =
                                    foldr (\drink acc → max acc (1 + (drink^.drinkId))) 0 (unDrinkDB drinkDB)
                                  newDrink =
                                    Drink { _drinkId          = minimalUnusedID
                                          , _drinkDescription = inputDesc
                                          , _drinkTimestamp   = inputTime
                                          , _drinkLiters      = inputMilliliters/1000
                                          }
                              in DrinkDB $ newDrink:(unDrinkDB drinkDB))
                           maybeInput)
                      initialDrinkDB
                      inputDrinkComponentsInputEv
    -- model: save on change
    let saveDB = map (\updatedDB → do
                        liftIO $ js_log "DB has been updated."
                        liftIO $ DOMStorage.setItem storage ("drinks" ∷ JSString) (toJSString (unpack (decodeUtf8 (Aeson.encode updatedDB)))))
                     (updated drinkDB)
    performEvent_ saveDB

    -- view part 2
    el "div" $ do
      -- the progress message that tells you how you're doing today
      totalLitersToday ← forDyn drinkDB $ \drinkDB' →
        sum $ map _drinkLiters $ filter (\drink → Clock.utctDay (drink^.drinkTimestamp) ≡ today) (unDrinkDB drinkDB')
      totalLitersYesterday ← forDyn drinkDB $ \drinkDB' →
        sum $ map _drinkLiters $ filter (\drink → Clock.utctDay (drink^.drinkTimestamp) ≡ yesterday) (unDrinkDB drinkDB')
      progressMessageAttrs ← forDyn totalLitersToday $ \x → if
        | x ≥ 2.5   → (Data.Map.fromList [("class", "ui green message")])
        | x ≥ 1.5   → (Data.Map.fromList [("class", "ui yellow message")])
        | otherwise → (Data.Map.fromList [("class", "ui red message")])
      elDynAttr "div" progressMessageAttrs $
        el "em" $ do
          dynText =<< mapDyn (\totalLitersYesterday' → "Yesterday: " ++ show totalLitersYesterday' ++ " of 2.5 liters") totalLitersYesterday
          el "br" $ return ()
          el "strong" $ do
            dynText =<< mapDyn (\totalLitersToday' → "Progress today: " ++ show totalLitersToday' ++ " of 2.5 liters") totalLitersToday
      -- the drink log
      rowKeysAndValues ← mapDyn
        (\drinkDB' → Data.Map.fromList $
           [((-drink^.drinkId, drink^.drinkTimestamp), drink) | drink ← (unDrinkDB drinkDB')])
        drinkDB
      _ ← tableDynAttr "ui celled inverted purple table"
                       [ ("Date",   \_ drinkDyn → dynText =<< mapDyn (\x → TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %R" (x^.drinkTimestamp)) drinkDyn)
                       , ("Type",   \_ drinkDyn → dynText =<< mapDyn (\x → unpack $ x^.drinkDescription) drinkDyn)
                       , ("Amount", \_ drinkDyn → dynText =<< mapDyn (\x → show (x^.drinkLiters) <> " liters") drinkDyn)
                       ]
                       rowKeysAndValues
                       (\(_, timestamp) → return $ constDyn $ if
                         | Clock.utctDay timestamp ≡ today →
                           mempty
                         | otherwise →
                           Data.Map.fromList [("class", "disabled")])
      return ()
