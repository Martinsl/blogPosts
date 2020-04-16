{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.HTML.Scalpel
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Control.Monad.Zip (mzipWith)
import Control.Monad.Zip (MonadZip, mzipWith)

postTextScraper :: Scraper String [String]
postTextScraper = do
    itemData <- texts $ "div" @: [hasClass "item-page"] // "p"
    return $ itemData

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- List of all posts, with (Title, Uri)
posts :: Scraper String [(String, String)]
posts = chroots postList post
    where
        blogDiv = "div" @: [hasClass "blog"]
        itemsLeadingDiv = "div" @: [hasClass "items-leading"]
        blogPosts = "div" @: ["itemprop" @= "blogPost"]
        postList = blogDiv // itemsLeadingDiv // blogPosts

-- get simple post data
post :: Scraper String (String, String)
post = do
    postTitle <- text $ aTag
    postUrl <- attr "href" $ aTag
    return $ (trim postTitle, postUrl)
    where
        aTag = "h2" @: ["itemProp" @= "name"] // "a"
    
formatPrintPost :: (String, String) -> String
formatPrintPost (t, u) = t <> " - " <> u

formatPrintPostFull :: (String, String, String) -> String
formatPrintPostFull (t, u, d) = t <> " - " <> u <> ":\n\n\n" <> d

printTriples :: Foldable t => t (String, String, String) -> IO()
printTriples = mapM_ $ putStrLn . formatPrintPostFull

concatUri :: URL -> (String, String) -> (String, String)
concatUri url (t, u) = (t, url <> u)

-- Concats base url with post uri
completeUrl :: URL -> [(String, String)] -> [(String, String)]
completeUrl url postData = map (concatUri url) postData

getPostText :: (String, URL) -> IO String
getPostText (t, url) = do
    postText <- scrapeURL url postTextScraper
    return $ fromMaybe t $ concatText postText
    where
        concatText = fmap unlines 

addPostText ((t, u), text) = do
    textString <- text
    return (t, u, textString)

mzipMaybe :: MonadZip m => m [a] -> m [b] -> m [(a, b)]
mzipMaybe infos texts = mzipWith (\a b -> zip a b) infos texts

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _     = putStrLn "usage: list-all-images URL"

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
    -- getting titles and uris
    postHeader <- scrapeURL url posts
    -- concating base url with uri
    let postHeaderFull = fmap (completeUrl url) postHeader
    -- getting post full text
    let postTexts = fmap (map getPostText) postHeaderFull
    -- creating triple (title, url, data)
    let maybeData = fmap (map addPostText) $ mzipMaybe postHeaderFull postTexts
    allData <- sequence $ fromMaybe [] maybeData
    putStrLn $ printTriples allData
    where
        printError   = putStrLn "ERROR: Could not scrape the URL!"
        printTriples = unlines . map formatPrintPostFull
