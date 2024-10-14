{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_)
import Data.Functor.Identity (Identity (..))
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Htmx (showT, useHtmx)
import Lucid
    ( Html
    , ToHtml (toHtml)
    , a_
    , body_
    , charset_
    , content_
    , h4_
    , head_
    , href_
    , html_
    , meta_
    , name_
    , renderText
    , span_
    , td_
    , title_
    , tr_
    )
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , showDefault
    , value
    , (<**>)
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

data Options = Options
    { port :: Int
    , page :: Int
    }

parseOptions :: Parser Options
parseOptions =
    Options
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Port to run the server on"
                <> showDefault
                <> value 3000
            )
        <*> option
            auto
            ( long "page"
                <> short 'n'
                <> metavar "PAGE"
                <> help "Page size of the scrolling"
                <> showDefault
                <> value 200
            )

optsParser :: ParserInfo Options
optsParser =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc "Run an HTML server of infinite positive integers"
            <> header "Scrolling Ints Server"
        )

parsePresents :: [Text] -> [Int]
parsePresents = fmap read . filter (/= "") . fmap unpack

title :: Html ()
title = h4_ "Infinie scrolling in constant space with HTMX"

note :: Html ()
note = do
    span_ $ do
        "Code: "
        a_ [href_ "https://github.com/paolino/htmx-scroll"] "paolino/htmx-scroll"

main :: IO ()
main = do
    opts <- execParser optsParser
    let portNumber = port opts
        scroller = scrollingInts $ page opts
    scotty portNumber $ do
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
                    body_ $ do
                        title
                        note
                        runIdentity $ widget scroller

        post "/update" $ do
            center <- queryParam "center"
            presentsRaw <-
                fmap snd . filter ((== "present") . fst)
                    <$> formParams
            let presents = Set.fromList $ parsePresents presentsRaw
            html $ renderText $ runIdentity $ scroll scroller presents center

scrollingInts :: Int -> Scrolling Identity Int
scrollingInts pageSize =
    makeScrolling
        $ Configuration
            { renderIndexRows = pure . renderRows
            , uniqueScrollingId = "ints"
            , previous = \n -> pure $ if n == 0 then Nothing else Just (n - 1)
            , next = \n -> pure $ Just (n + 1)
            , renderIndex = showT
            , updateURL = \n -> "/update?center=" <> showT n
            , zeroIndex = 0
            , presentFieldName = "present"
            }
  where
    renderRows :: Int -> Html ()
    renderRows j = do
        forM_ [j * pageSize .. (j + 1) * pageSize - 1]
            $ \i -> tr_ $ td_ $ toHtml $ show i
