{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrolling
    ( Configuration (..)
    , Scrolling (..)
    , makeScrolling
    )
where

import Data.Set (Set, toList)
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

data ChangeShown index
    = ChangeShown
        { changeDirection :: Direction
        , changeId :: index
        }
    | DeleteShown
        { deleteId :: index
        }
    deriving (Show)
changes
    :: (Eq index, Monad m)
    => Configuration m index
    -> Set index
    -> index
    -> m [ChangeShown index]
changes configuration shown rcenter =
    let insertAfter c md = do
            mnc <- next configuration c
            pure $ do
                Just nc <- [mnc]
                let delete = case md of
                        Nothing -> []
                        Just d -> [DeleteShown d]
                delete <> [ChangeShown After nc]
        insertBefore c md = do
            mpc <- previous configuration c
            pure $ do
                Just pc <- [mpc]
                let delete = case md of
                        Nothing -> []
                        Just d -> [DeleteShown d]
                delete <> [ChangeShown Before pc]
        boot = pure [ChangeShown After rcenter]
    in  case toList shown of
            [] -> boot
            [c0] -> insertAfter c0 Nothing
            [_c0, c1]
                | c1 == rcenter -> insertAfter c1 Nothing
            [_c0, _c1, c2]
                | c2 == rcenter -> insertAfter c2 Nothing
            [c0, c1, _c2, c3]
                | c1 == rcenter -> insertBefore c0 $ Just c3
                | c3 == rcenter -> insertAfter c3 $ Just c0
            _ -> pure []

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

intersectNames :: Text
intersectNames = "intersect"

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

tbodyIntersectId :: Configuration m index -> index -> [Attribute] -> [Attribute]
tbodyIntersectId c j =
    (:)
        $ id_
        $ appendIndexAndScrollingId c j intersectNames

blockId :: Configuration m index -> index -> Html ()
blockId c j =
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
        . tbodyIntersectId c j
        $ []

renderBlock :: Functor m => Configuration m index -> index -> m (Html ())
renderBlock c j = tbody_ (tbodyDataId c j []) <$> renderIndexRows c j

renderChange
    :: Monad m
    => Configuration m index
    -> ChangeShown index
    -> m (Html ())
renderChange c (ChangeShown dir i) = do
    newData <- table_ [hxSwapOob_ w] <$> renderBlock c i
    pure $ do
        let newIntersect =
                table_ [hxSwapOob_ w]
                    $ tbody_ (updaterAttributes c i) mempty
        case dir of
            After -> do
                newIntersect
                newData
            Before -> do
                newData
                newIntersect
        form_ [hxSwapOob_ f] $ blockId c i
  where
    f = case dir of
        After -> "afterbegin:" <> appendScrollingId c (mkId stateName)
        Before -> "beforeend:" <> appendScrollingId c (mkId stateName)
    w = case dir of
        After -> "beforeend:" <> appendScrollingId c (mkId tableName)
        Before -> "afterbegin:" <> appendScrollingId c (mkId tableName)
renderChange c (DeleteShown i) = pure $ do
    table_
        [ id_ $ appendScrollingId c tableName
        , hxSwapOob_
            $ "delete:" <> appendIndexAndScrollingId c i (mkId intersectNames)
        ]
        mempty
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

setup :: Monad m => Configuration m index -> m (Html ())
setup c = do
    zero <- zeroIndex c
    pure $ do
        div_ [id_ $ "scrolling-" <> uniqueScrollingId c] $ do
            form_ [id_ $ appendScrollingId c stateName] mempty
            table_ [id_ $ appendScrollingId c tableName]
                $ tbody_ (updaterAttributes c zero) mempty

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
