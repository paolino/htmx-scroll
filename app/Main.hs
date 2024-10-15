{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM_)
import Data.Functor.Identity (Identity (..))
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Htmx (showT, useHtmx)
import Lucid
    ( Html
    , ToHtml (toHtml)
    , a_
    , body_
    , button_
    , charset_
    , content_
    , div_
    , form_
    , h4_
    , head_
    , href_
    , html_
    , id_
    , input_
    , meta_
    , name_
    , renderText
    , span_
    , style_
    , table_
    , td_
    , title_
    , tr_
    , type_
    , value_
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
    , (<|>)
    )
import Scrolling
    ( Configuration (..)
    , Scrolling (..)
    , makeScrolling
    , scroll
    )
import Web.Scotty
    ( formParamMaybe
    , formParams
    , get
    , html
    , post
    , queryParam
    , queryParamMaybe
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
title = h4_ "Infinite scrolling in constant HTML with HTMX"

note :: Html ()
note = do
    span_ $ do
        "Code: "
        a_ [href_ "https://github.com/paolino/htmx-scroll"] "paolino/htmx-scroll"

control :: Maybe Int -> Maybe Int -> Maybe Int -> Html ()
control from to start = do
    form_ [id_ "control"] $ do
        table_ $ do
            tr_ $ do
                td_ "From: "
                td_ $ input_ $ [type_ "number", name_ "from"] <> defaulting from
            tr_ $ do
                td_ "To: "
                td_ $ input_ $ [type_ "number", name_ "to"] <> defaulting to
            tr_ $ do
                td_ "Start: "
                td_ $ input_ $ [type_ "number", name_ "start"] <> defaulting start
        button_
            [ type_ "submit"
            ]
            "Update"
    form_
        $ button_ "Reset"
  where
    defaulting = maybe [] (\s -> [value_ $ showT s])

main :: IO ()
main = do
    opts <- execParser optsParser
    let portNumber = port opts
        scroller = scrollingInts (page opts)
    scotty portNumber $ do
        get "/" $ do
            from <- queryParamMaybe "from"
            to <- queryParamMaybe "to"
            start <- queryParamMaybe "start"
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
                        div_
                            [style_ "position: fixed; top: 0; left: 0; padding-left: 4em;"]
                            $ do
                                title
                                control from to start
                                note
                        runIdentity $ widget $ scroller from to start

        post "/update" $ do
            center <- queryParam "center"
            from <- formParamMaybe "from"
            to <- formParamMaybe "to"
            start <- formParamMaybe "start"
            presentsRaw <-
                fmap snd . filter ((== "present") . fst)
                    <$> formParams
            let presents = Set.fromList $ parsePresents presentsRaw
            html $ renderText $ runIdentity $ scroll (scroller from to start) presents center

scrollingInts :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> Scrolling Identity Int
scrollingInts pageSize from to start =
    makeScrolling
        $ Configuration
            { renderIndexRows = pure . renderRows
            , uniqueScrollingId = "ints"
            , previous = \n -> pure $ case bottomPage of
                Nothing -> Just (n - 1)
                Just f -> if n > f then Just (n - 1) else Nothing
            , next = \n -> pure $ case topPage of
                Nothing -> Just (n + 1)
                Just t -> if n < t then Just (n + 1) else Nothing
            , renderIndex = showT
            , updateURL = \n -> "/update?center=" <> showT n
            , zeroIndex = pure $ maybe 0 (`div` pageSize) $ start <|> from <|> to
            , presentFieldName = "present"
            , controlSelector = "#control"
            }
  where
    topPage = (`div` pageSize) <$> to
    bottomPage = (`div` pageSize) <$> from
    renderRows :: Int -> Html ()
    renderRows j = do
        forM_ [bottom .. top]
            $ \i -> tr_ $ td_ $ toHtml $ show i
      where
        top = case to of
            Just t -> min t ((j + 1) * pageSize - 1)
            Nothing -> (j + 1) * pageSize - 1
        bottom = case from of
            Just f -> max f (j * pageSize)
            Nothing -> j * pageSize
