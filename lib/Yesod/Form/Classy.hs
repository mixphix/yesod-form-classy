module Yesod.Form.Classy
  ( InputField (..)
  , input
  , Optionality (..)
  , OptionalityRepr (..)
  , KnownOptionality (..)
  , Multiplicity (..)
  , MultiplicityRepr (..)
  , KnownMultiplicity (..)
  , FormShape
  )
where

import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Yesod

data Optionality
  = Required
  | Optional

type OptionalityRepr :: Optionality -> Type
data OptionalityRepr opt where
  RequiredRepr :: OptionalityRepr Required
  OptionalRepr :: OptionalityRepr Optional

type KnownOptionality :: Optionality -> Constraint
class KnownOptionality opt where
  optionality :: OptionalityRepr opt

instance KnownOptionality Required where
  optionality :: OptionalityRepr Required
  optionality = RequiredRepr

instance KnownOptionality Optional where
  optionality :: OptionalityRepr Optional
  optionality = OptionalRepr

data Multiplicity
  = Single
  | Multiple

type MultiplicityRepr :: Multiplicity -> Type
data MultiplicityRepr mul where
  SingleRepr :: MultiplicityRepr Single
  MultipleRepr :: MultiplicityRepr Multiple

type KnownMultiplicity :: Multiplicity -> Constraint
class KnownMultiplicity mul where
  multiplicity :: MultiplicityRepr mul

instance KnownMultiplicity Single where
  multiplicity :: MultiplicityRepr Single
  multiplicity = SingleRepr

instance KnownMultiplicity Multiple where
  multiplicity :: MultiplicityRepr Multiple
  multiplicity = MultipleRepr

type FormShape :: Optionality -> Multiplicity -> Type -> Type
type family FormShape opt mul = x | x -> opt mul where
  FormShape Required Single = Identity
  FormShape Optional Single = Maybe
  FormShape Required Multiple = NonEmpty
  FormShape Optional Multiple = []

shapeAttrs ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  Const [(Text, Text)] (FormShape opt mul)
shapeAttrs =
  let multiple = case multiplicity @mul of
        SingleRepr -> []
        MultipleRepr -> [("multiple", "multiple")]
      required = case optionality @opt of
        RequiredRepr -> [("required", "required")]
        OptionalRepr -> []
   in Const (multiple <> required)

type InputField :: Type -> Optionality -> Multiplicity -> Type -> Constraint
class
  ( KnownOptionality opt
  , KnownMultiplicity mul
  ) =>
  InputField app opt mul x
  where
  inputEnctype :: Const Enctype (app, FormShape opt mul x)
  inputView ::
    [(Text, Text)] ->
    Either Text (FormShape opt mul x) ->
    WidgetFor app ()
  inputParse ::
    [Text] ->
    [FileInfo] ->
    HandlerFor app (Either (SomeMessage app) (FormShape opt mul x))

input ::
  forall opt mul x app.
  ( KnownOptionality opt
  , KnownMultiplicity mul
  , RenderMessage app FormMessage
  , InputField app opt mul x
  ) =>
  Maybe (FormShape opt mul x) ->
  [(Text, Text)] ->
  MForm
    (HandlerFor app)
    (FormResult (FormShape opt mul x), WidgetFor app ())
input mshape config = do
  let Const shapeattrs = shapeAttrs @opt @mul
  tell $ getConst (inputEnctype @app @opt @mul @x)
  (environment, site, langs) <- ask
  name <- maybe newFormIdent pure (lookup "name" config)
  fmap (inputView (shapeattrs <> config)) <$> case environment of
    Nothing -> pure (FormMissing, maybe (Left "") Right mshape)
    Just (allinputs, allfiles) -> do
      let inputs = Map.findWithDefault [] name allinputs
          files = Map.findWithDefault [] name allfiles
      lift (inputParse @app @opt @mul @x inputs files) <&> \case
        Right x -> (FormSuccess x, Right x)
        Left (SomeMessage e) ->
          ( FormFailure [renderMessage site langs e]
          , Left (Text.intercalate ", " inputs)
          )
