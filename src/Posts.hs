{-# LANGUAGE OverloadedStrings #-}

module Posts
    ( listUrlsForSite
    ) where

import System.Environment
import Text.HTML.Scalpel
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Control.Monad.Zip (MonadZip, mzipWith)

data Post = Post { title   :: String
                 , url     :: String
                 , content :: Maybe String
                 } deriving (Show)

postTextScraper :: Scraper String [String]
postTextScraper = texts $ "div" @: [hasClass "item-page"] // "p"

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- List of all posts, with (Title, Uri)
posts :: Scraper String [Post]
posts = chroots postList post
    where
        blogDiv = "div" @: [hasClass "blog"]
        itemsLeadingDiv = "div" @: [hasClass "items-leading"]
        blogPosts = "div" @: ["itemprop" @= "blogPost"]
        postList = blogDiv // itemsLeadingDiv // blogPosts

-- get simple post data
post :: Scraper String Post
post = do
    postTitle <- text aTag
    postUrl <- attr "href" aTag
    return Post { title=trim postTitle, url=postUrl, content=Nothing}
    where
        aTag = "h2" @: ["itemProp" @= "name"] // "a"
    
formatPrintPost :: Post -> String
formatPrintPost p = title p <> " - " <> url p

formatPrintPostFull :: Post -> String
formatPrintPostFull p = 
    case content p of
        Nothing   -> title p <> " - " <> url p <> ":\n\n"
        Just cont -> title p <> " - " <> url p <> ":\n\n" <> cont <> "\n\n\n"

printTriples :: Foldable t => t Post -> IO()
printTriples = mapM_ $ putStrLn . formatPrintPostFull

concatUri :: URL -> Post -> Post
concatUri u p = p { url = u <> url p }

completeUrl :: URL -> [Post] -> [Post]
completeUrl url = map (concatUri url)

getPostText :: Post -> IO String
getPostText p = do
    postText <- scrapeURL (url p) postTextScraper
    return $ fromMaybe (title p) $ concatText postText
    where
        concatText = fmap unlines 

addPostText (p, text) = do
    textString <- text
    return p { content = Just $ trim textString }

mzipMaybe :: MonadZip m => m [a] -> m [b] -> m [(a, b)]
mzipMaybe = mzipWith zip

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
    -- getting titles and uris
    postHeader <- scrapeURL url posts
    -- concating base url with uri
    let postHeaderFull = fmap (completeUrl url) postHeader
    -- getting post full text
    let postTexts = fmap (map getPostText) postHeaderFull
    -- creating triple (title, url, data)
    let maybeData = map addPostText <$> mzipMaybe postHeaderFull postTexts
    allData <- sequence $ fromMaybe [] maybeData
    putStrLn $ printTriples allData
    where
        printError   = putStrLn "ERROR: Could not scrape the URL!"
        printTriples = unlines . map formatPrintPostFull
