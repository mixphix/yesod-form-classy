module Yesod.Form.Classy
  ( InputField (..)
  , input
  , select
  , Optionality (..)
  , OptionalityRepr (..)
  , KnownOptionality (..)
  , Multiplicity (..)
  , MultiplicityRepr (..)
  , KnownMultiplicity (..)
  , FormShape
  , formShapeFunctor
  , formShapeFoldable
  , formShapeTraversable
  , formShapeApplicative
  , formShapeMonad
  )
where

import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Yesod.Core
import Yesod.Form

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

formShapeFunctor ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  (forall x. ((Functor (FormShape opt mul)) => x) -> x)
formShapeFunctor x = case (optionality @opt, multiplicity @mul) of
  (RequiredRepr, SingleRepr) -> x
  (OptionalRepr, SingleRepr) -> x
  (RequiredRepr, MultipleRepr) -> x
  (OptionalRepr, MultipleRepr) -> x

formShapeFoldable ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  (forall x. ((Foldable (FormShape opt mul)) => x) -> x)
formShapeFoldable x = case (optionality @opt, multiplicity @mul) of
  (RequiredRepr, SingleRepr) -> x
  (OptionalRepr, SingleRepr) -> x
  (RequiredRepr, MultipleRepr) -> x
  (OptionalRepr, MultipleRepr) -> x

formShapeTraversable ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  (forall x. ((Traversable (FormShape opt mul)) => x) -> x)
formShapeTraversable x = case (optionality @opt, multiplicity @mul) of
  (RequiredRepr, SingleRepr) -> x
  (OptionalRepr, SingleRepr) -> x
  (RequiredRepr, MultipleRepr) -> x
  (OptionalRepr, MultipleRepr) -> x

formShapeApplicative ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  (forall x. ((Applicative (FormShape opt mul)) => x) -> x)
formShapeApplicative x = case (optionality @opt, multiplicity @mul) of
  (RequiredRepr, SingleRepr) -> x
  (OptionalRepr, SingleRepr) -> x
  (RequiredRepr, MultipleRepr) -> x
  (OptionalRepr, MultipleRepr) -> x

formShapeMonad ::
  forall opt mul.
  (KnownOptionality opt, KnownMultiplicity mul) =>
  (forall x. ((Monad (FormShape opt mul)) => x) -> x)
formShapeMonad x = case (optionality @opt, multiplicity @mul) of
  (RequiredRepr, SingleRepr) -> x
  (OptionalRepr, SingleRepr) -> x
  (RequiredRepr, MultipleRepr) -> x
  (OptionalRepr, MultipleRepr) -> x

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

select ::
  forall opt mul x app.
  ( KnownOptionality opt
  , KnownMultiplicity mul
  , RenderMessage app FormMessage
  , Eq x
  ) =>
  OptionList x ->
  Maybe (FormShape opt mul x) ->
  [(Text, Text)] ->
  MForm
    (HandlerFor app)
    (FormResult (FormShape opt mul x), WidgetFor app ())
select options mshape config = do
  let Const shapeattrs = shapeAttrs @opt @mul
      selectParse ::
        [Text] ->
        HandlerFor app (Either (SomeMessage app) (FormShape opt mul x))
      selectParse inputs = do
        let parse = case options of
              OptionList{..} -> olReadExternal
              OptionListGrouped{..} -> olReadExternalGrouped
        case (optionality @opt, multiplicity @mul) of
          (RequiredRepr, SingleRepr) -> pure case inputs of
            [] -> Left "input is required"
            [x] -> case parse x of
              Nothing -> Left $ SomeMessage ("unknown option: " <> x)
              Just y -> Right (Identity y)
            _ -> Left "multiple given when one expected"
          (OptionalRepr, SingleRepr) -> pure case inputs of
            [] -> Right Nothing
            [x] -> case parse x of
              Nothing -> Left $ SomeMessage ("unknown option: " <> x)
              Just y -> Right (Just y)
            _ -> Left "multiple given when one expected"
          (RequiredRepr, MultipleRepr) -> pure case nonEmpty inputs of
            Nothing -> Left "input is required"
            Just xs ->
              let results =
                    xs <&> \x -> case parse x of
                      Nothing -> Left $ SomeMessage ("unknown option: " <> x)
                      Just y -> Right y
               in sequence results
          (OptionalRepr, MultipleRepr) -> do
            let results =
                  inputs <&> \x -> case parse x of
                    Nothing -> Left $ SomeMessage ("unknown option: " <> x)
                    Just y -> Right y
            pure (sequence results)
      selectView ::
        (Eq x) =>
        [(Text, Text)] ->
        Either Text (FormShape opt mul x) ->
        WidgetFor app ()
      selectView attrs evalue = do
        let selected :: x -> Bool = case evalue of
              Left _ -> const False
              Right val -> case (optionality @opt, multiplicity @mul) of
                (RequiredRepr, SingleRepr) -> (== runIdentity val)
                (OptionalRepr, SingleRepr) -> maybe (const False) (==) val
                (RequiredRepr, MultipleRepr) -> flip elem (toList val)
                (OptionalRepr, MultipleRepr) -> flip elem val
        [whamlet|
          <select *{attrs}>
            $case options
              $of OptionList{olOptions}
                $forall option <- olOptions
                  <option :selected option.optionInternalValue:selected value="#{option.optionExternalValue}">
                    #{option.optionDisplay}
              $of OptionListGrouped{olOptionsGrouped}
                $forall (group, options) <- olOptionsGrouped
                  <optgroup label="#{group}">
                    $forall option <- options
                      <option :selected option.optionInternalValue:selected value="#{option.optionExternalValue}">
                        #{option.optionDisplay}
        |]
  tell UrlEncoded
  (environment, site, langs) <- ask
  name <- maybe newFormIdent pure (lookup "name" config)
  fmap (selectView (shapeattrs <> config)) <$> case environment of
    Nothing -> pure (FormMissing, maybe (Left "") Right mshape)
    Just (allinputs, _) -> do
      let inputs = Map.findWithDefault [] name allinputs
      lift (selectParse inputs) <&> \case
        Right x -> (FormSuccess x, Right x)
        Left (SomeMessage e) ->
          ( FormFailure [renderMessage site langs e]
          , Left (Text.intercalate ", " inputs)
          )