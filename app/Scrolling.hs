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
    :: Eq index
    => Configuration index
    -> Set index
    -> index
    -> [ChangeShown index]
changes configuration shown rcenter =
    let insertAfter c md = do
            Just nc <- [next configuration c]
            let delete = case md of
                    Nothing -> []
                    Just d -> [DeleteShown d]
            delete <> [ChangeShown After nc]
        insertBefore c md = do
            Just pc <- [previous configuration c]
            let delete = case md of
                    Nothing -> []
                    Just d -> [DeleteShown d]
            delete <> [ChangeShown Before pc]
        boot = [ChangeShown After rcenter]
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
            _ -> []

data Configuration index = Configuration
    { renderIndexRows :: index -> Html ()
    , uniqueScrollingId :: Text
    , previous :: index -> Maybe index
    , next :: index -> Maybe index
    , renderIndex :: index -> Text
    , updateURL :: index -> Text
    , zeroIndex :: index
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

appendScrollingId :: Configuration index -> Text -> Text
appendScrollingId Configuration{uniqueScrollingId} =
    (<> ("-" <> uniqueScrollingId))

appendIndex :: Configuration index -> index -> Text -> Text
appendIndex Configuration{renderIndex} j = (<> ("-" <> renderIndex j))

appendIndexAndScrollingId :: Configuration index -> index -> Text -> Text
appendIndexAndScrollingId c j = appendScrollingId c . appendIndex c j

dontSwap :: [Attribute] -> [Attribute]
dontSwap = (:) $ hxSwap_ "none"

triggerIntersect :: [Attribute] -> [Attribute]
triggerIntersect = (:) $ hxTrigger_ "intersect"

includeScrollingState :: Configuration index -> [Attribute] -> [Attribute]
includeScrollingState c =
    (:)
        $ hxInclude_
        $ mkId
        $ appendScrollingId c stateName

tbodyDataId :: Configuration index -> index -> [Attribute] -> [Attribute]
tbodyDataId c j =
    (:)
        $ id_
        $ appendIndexAndScrollingId c j dataNames

tbodyIntersectId :: Configuration index -> index -> [Attribute] -> [Attribute]
tbodyIntersectId c j =
    (:)
        $ id_
        $ appendIndexAndScrollingId c j intersectNames

blockId :: Configuration index -> index -> Html ()
blockId c j =
    input_
        [ id_ $ appendIndexAndScrollingId c j stateName
        , hidden_ ""
        , name_ $ presentFieldName c
        , value_ $ renderIndex c j
        ]

postToUpdate :: Configuration index -> index -> [Attribute] -> [Attribute]
postToUpdate c j = (:) $ hxPost_ $ updateURL c j

updaterAttributes :: Configuration index -> index -> [Attribute]
updaterAttributes c j =
    dontSwap
        . triggerIntersect
        . includeScrollingState c
        . postToUpdate c j
        . tbodyIntersectId c j
        $ []

renderBlock :: Configuration index -> index -> Html ()
renderBlock c j = tbody_ (tbodyDataId c j []) $ renderIndexRows c j

renderChange :: Configuration index -> ChangeShown index -> Html ()
renderChange c (ChangeShown dir i) = do
    let newIntersect =
            table_ [hxSwapOob_ w]
                $ tbody_ (updaterAttributes c i) mempty
        newData = table_ [hxSwapOob_ w] $ renderBlock c i
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
renderChange c (DeleteShown i) = do
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

setup :: Configuration index -> Html ()
setup c = div_ [id_ $ "scrolling-" <> uniqueScrollingId c] $ do
    form_ [id_ $ appendScrollingId c stateName] mempty
    table_ [id_ $ appendScrollingId c tableName]
        $ tbody_ (updaterAttributes c $ zeroIndex c) mempty

data Scrolling index = Scrolling
    { widget :: Html ()
    , scroll :: Set index -> index -> Html ()
    }

makeScrolling :: Eq index => Configuration index -> Scrolling index
makeScrolling c =
    Scrolling
        { widget = setup c
        , scroll = \state focus ->
            foldMap (renderChange c)
                $ changes c state focus
        }
