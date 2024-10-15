{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrolling
    ( Configuration (..)
    , Scrolling (..)
    , makeScrolling
    )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (forM)
import Htmx
    ( hxInclude_
    , hxPost_
    , hxSwapOob_
    , hxSwap_
    , hxTrigger_
    )
import Lucid
    ( Attribute
    , Html
    , div_
    , form_
    , hidden_
    , id_
    , input_
    , name_
    , table_
    , tbody_
    , value_
    )

data Direction = Before | After
    deriving (Show)

(<^>) :: Applicative m => m [a] -> m [a] -> m [a]
(<^>) = liftA2 (<>)

data Change index
    = Delete index
    | Add Direction index
    deriving (Show)
changes
    :: (Eq index, Monad m)
    => Configuration m index
    -> Set index
    -> index
    -> m [Change index]
changes configuration presences signal = case Set.toList presences of
    [_] -> onNext signal Add <^> onPrevious signal Add
    [p0, p1]
        | p1 == signal -> onNext signal Add
        | p0 == signal -> onPrevious signal Add
    [p0, _p1, p2]
        | p2 == signal -> onNext signal Add
        | p0 == signal -> onPrevious signal Add
    [p0, _p1, _p2, p3]
        | p3 == signal -> onNext signal Add <^> pure [Delete p0]
        | p0 == signal -> onPrevious signal Add <^> pure [Delete p3]
    _ -> pure []
  where
    onNext i f = do
        nextIndex <- next configuration i
        pure $ case nextIndex of
            Nothing -> []
            Just j -> [f After j]
    onPrevious i f = do
        previousIndex <- previous configuration i
        pure $ case previousIndex of
            Nothing -> []
            Just j -> [f Before j]

renderChange :: Monad m => Configuration m index -> Change index -> m (Html ())
renderChange c (Delete i) = pure $ do
    table_
        [ id_ $ appendScrollingId c tableName
        , hxSwapOob_
            $ "delete:" <> appendIndexAndScrollingId c i (mkId dataNames)
        ]
        mempty
    input_
        [ id_ $ appendIndexAndScrollingId c i stateName
        , hxSwapOob_ "delete"
        ]
renderChange c (Add dir i) = do
    newData <- table_ [hxSwapOob_ w] <$> renderPage c i
    pure $ do
        newData
        form_ [hxSwapOob_ f] $ renderPageId c i
  where
    f = "beforeend:" <> appendScrollingId c (mkId stateName)
    w = case dir of
        After -> "beforeend:" <> appendScrollingId c (mkId tableName)
        Before -> "afterbegin:" <> appendScrollingId c (mkId tableName)

data Configuration m index = Configuration
    { renderIndexRows :: index -> m (Html ())
    , uniqueScrollingId :: Text
    , previous :: index -> m (Maybe index)
    , next :: index -> m (Maybe index)
    , renderIndex :: index -> Text
    , updateURL :: index -> Text
    , zeroIndex :: m index
    , presentFieldName :: Text
    }

stateName :: Text
stateName = "state"

tableName :: Text
tableName = "table"

dataNames :: Text
dataNames = "data"

mkId :: Text -> Text
mkId x = "#" <> x

appendScrollingId :: Configuration m index -> Text -> Text
appendScrollingId Configuration{uniqueScrollingId} =
    (<> ("-" <> uniqueScrollingId))

appendIndex :: Configuration m index -> index -> Text -> Text
appendIndex Configuration{renderIndex} j = (<> ("-" <> renderIndex j))

appendIndexAndScrollingId :: Configuration m index -> index -> Text -> Text
appendIndexAndScrollingId c j = appendScrollingId c . appendIndex c j

dontSwap :: [Attribute] -> [Attribute]
dontSwap = (:) $ hxSwap_ "none"

triggerIntersect :: [Attribute] -> [Attribute]
triggerIntersect = (:) $ hxTrigger_ "intersect"

includeScrollingState :: Configuration m index -> [Attribute] -> [Attribute]
includeScrollingState c =
    (:)
        $ hxInclude_
        $ mkId
        $ appendScrollingId c stateName

tbodyDataId :: Configuration m index -> index -> [Attribute] -> [Attribute]
tbodyDataId c j =
    (:)
        $ id_
        $ appendIndexAndScrollingId c j dataNames

renderPageId :: Configuration m index -> index -> Html ()
renderPageId c j =
    input_
        [ id_ $ appendIndexAndScrollingId c j stateName
        , hidden_ ""
        , name_ $ presentFieldName c
        , value_ $ renderIndex c j
        ]

postToUpdate :: Configuration m index -> index -> [Attribute] -> [Attribute]
postToUpdate c j = (:) $ hxPost_ $ updateURL c j

updaterAttributes :: Configuration m index -> index -> [Attribute]
updaterAttributes c j =
    dontSwap
        . triggerIntersect
        . includeScrollingState c
        . postToUpdate c j
        . tbodyDataId c j
        $ []

renderPage
    :: Functor m
    => Configuration m index
    -> index
    -> m (Html ())
renderPage c j = tbody_ (updaterAttributes c j) <$> renderIndexRows c j

setup :: Monad m => Configuration m index -> m (Html ())
setup c = do
    zero <- zeroIndex c
    zeroContent <- renderPage c zero
    pure $ do
        div_ [id_ $ "scrolling-" <> uniqueScrollingId c] $ do
            form_
                [id_ $ appendScrollingId c stateName]
                $ renderPageId c zero
            table_ [id_ $ appendScrollingId c tableName] zeroContent

data Scrolling m index = Scrolling
    { widget :: m (Html ())
    , scroll :: Set index -> index -> m (Html ())
    }

makeScrolling
    :: (Eq index, Monad m)
    => Configuration m index
    -> Scrolling m index
makeScrolling c =
    Scrolling
        { widget = setup c
        , scroll = \state focus -> do
            cs <- changes c state focus
            fmap mconcat $ forM cs $ renderChange c
        }
