{-# OPTIONS_GHC -Wno-orphans #-}

module Yesod.Form.Classy.Instances where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (readMaybe)
import Yesod.Core (FileInfo, HandlerFor, SomeMessage (..), WidgetFor, whamlet)
import Yesod.Form (Enctype (UrlEncoded))
import Yesod.Form.Classy

instance
  (KnownOptionality opt) =>
  InputField app opt Single Text
  where
  inputEnctype ::
    (KnownOptionality opt) =>
    Const Enctype (app, FormShape opt Single Text)
  inputEnctype = Const UrlEncoded

  inputView ::
    (KnownOptionality opt) =>
    [(Text, Text)] ->
    Either Text (FormShape opt Single Text) ->
    WidgetFor app ()
  inputView attrs evalue = do
    let value = case evalue of
          Left x -> x
          Right shape -> case optionality @opt of
            RequiredRepr -> runIdentity shape
            OptionalRepr -> fromMaybe "" shape
    [whamlet|<input type="text" value="#{value}" *{attrs}>|]

  inputParse ::
    (KnownOptionality opt) =>
    [Text] ->
    [FileInfo] ->
    HandlerFor app (Either (SomeMessage app) (FormShape opt Single Text))
  inputParse inputs _ = pure case optionality @opt of
    RequiredRepr -> case inputs of
      [] -> Left "input is required"
      [x] -> Right (Identity x)
      _ -> Left "multiple given when one expected"
    OptionalRepr -> case inputs of
      [] -> Right Nothing
      [x] -> Right (Just x)
      _ -> Left "multiple given when zero or one expected"

instance
  (KnownOptionality opt) =>
  InputField app opt Single Int
  where
  inputEnctype ::
    (KnownOptionality opt) =>
    Const Enctype (app, FormShape opt Single Int)
  inputEnctype = Const UrlEncoded

  inputView ::
    (KnownOptionality opt) =>
    [(Text, Text)] ->
    Either Text (FormShape opt Single Int) ->
    WidgetFor app ()
  inputView attrs evalue = formShapeFunctor @opt @Single do
    let value = case fmap (fmap (Text.pack . show)) evalue of
          Left x -> x
          Right shape -> case optionality @opt of
            RequiredRepr -> runIdentity shape
            OptionalRepr -> fromMaybe "" shape
    [whamlet|<input type="number" value="#{value}" *{attrs}>|]

  inputParse ::
    (KnownOptionality opt) =>
    [Text] ->
    [FileInfo] ->
    HandlerFor app (Either (SomeMessage app) (FormShape opt Single Int))
  inputParse inputs _ = pure case optionality @opt of
    RequiredRepr -> case inputs of
      [] -> Left "input is required"
      [x] -> case readMaybe (Text.unpack x) of
        Nothing -> Left $ SomeMessage ("could not parse Int: " <> x)
        Just int -> Right (Identity int)
      _ -> Left "multiple given when one expected"
    OptionalRepr -> case inputs of
      [] -> Right Nothing
      [x] -> case readMaybe (Text.unpack x) of
        Nothing -> Left $ SomeMessage ("could not parse Int: " <> x)
        Just int -> Right (Just int)
      _ -> Left "multiple given when one expected"
