{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Htmx (showT, useHtmx)
import Lucid
    ( Html
    , ToHtml (toHtml)
    , body_
    , charset_
    , content_
    , head_
    , html_
    , meta_
    , name_
    , renderText
    , td_
    , title_
    , tr_
    )
import Scrolling
    ( Configuration (..)
    , Scrolling (..)
    , makeScrolling
    , scroll
    )
import Web.Scotty
    ( formParams
    , get
    , html
    , post
    , queryParam
    , scotty
    )

parsePresents :: [Text] -> [Int]
parsePresents = fmap read . filter (/= "") . fmap unpack

main :: IO ()
main = scotty 3000
    $ do
        get "/" $ do
            html $ renderText $ do
                html_ [] $ do
                    head_ $ do
                        title_ "Scrolling Ints"
                        meta_ [charset_ "utf-8"]
                        meta_
                            [ name_ "viewport"
                            , content_ "width=device-width, initial-scale=1.0"
                            ]
                        useHtmx
                    body_ $ widget scrollingInts

        post "/update" $ do
            center <- queryParam "center"
            presents <- fmap snd . filter ((== "present") . fst) <$> formParams
            let cs = Set.fromList $ parsePresents presents
            html $ renderText $ scroll scrollingInts cs center

scrollingInts :: Scrolling Int
scrollingInts =
    makeScrolling
        $ Configuration
            { renderIndexRows = renderRows
            , uniqueScrollingId = "ints"
            , previous = \n -> if n == 0 then Nothing else Just (n - 1)
            , next = \n -> Just (n + 1)
            , renderIndex = showT
            , updateURL = \n -> "/update?center=" <> showT n
            , zeroIndex = 0
            , presentFieldName = "present"
            }
  where
    renderRows :: Int -> Html ()
    renderRows j = do
        forM_ [j * blockSize .. (j + 1) * blockSize - 1]
            $ \i -> tr_ $ td_ $ toHtml $ show i
    blockSize = 200
