{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), (&&&), arr)
import Control.Category (id)
import Control.Monad (forM_)
import Data.Monoid (mempty, mappend, mconcat)
import Data.Char (toUpper)

import System.FilePath (takeDirectory, dropExtension, dropFileName, combine, splitDirectories, joinPath)
import Data.List (isInfixOf, intercalate)

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze ((!), toValue)
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll 
import qualified Overrides as O

import Control.Monad.IO.Class (liftIO)

-- TODO:
-- Comments
-- Translate About page
-- Translate Tech page
-- Move assets from Dropbox to Google Drive

blogTitle :: String
blogTitle =  "G&iacute;sli Kristj&aacute;nsson"

main :: IO ()
main = hakyll $ do

	-- Static content
	forM_ ["favicon.ico", "lib/*", "images/**", "video/*", "CNAME"] $ \p ->
		match p $ do
			route idRoute
			compile copyFileCompiler

    -- Compress CSS
	match "css/*" $ do
		route idRoute
		compile compressCssCompiler

	-- Photos must not be run through Pandoc
	match "photos.html" $ do
		route           $ pageRoute
		compile         $ pageCompiler'
			>>> commonCompiler

	-- Render static pages
	match "*.markdown" $ do
		route          $ pageRoute
		compile		   $ pageCompiler
			>>> commonCompiler

	-- Read blog posts in a different group to avoid dependency
	-- cycles while producing tagclouds in a blog post
	group "tagcloud" $
		match "blog/*" $ 
			compile $ readPageCompiler

	create "tagcloud" $ 
		requireAll ("blog/*" `mappend` inGroup (Just "tagcloud")) (\_ ps -> readTags ps :: Tags String)

	-- Render blog
	match ("blog/*" `mappend` inGroup Nothing) $ do
		route      $ blogRoute
		compile    $ pageCompiler
			>>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
			>>> renderTagsField "prettytags" (fromCapture "tags/*")
			>>> arr (copyBodyToField "content")
			>>> arr (clearField "bodyTitle")
			>>> dropFileNameFromUrl 
			>>> addTeaser
			>>> applyTemplateCompiler "templates/blog_show.html"
			>>> requireA "tagcloud" (setFieldA "tagcloud" (O.renderTagCloud)) 
			>>> commonCompiler 

	-- Blog index (which is incidentally our root)
	match "index.html"  $ route idRoute
	create "index.html" $ constA mempty
		>>> arr (setField "pageTitle" blogTitle)
		>>> arr (clearField "bodyTitle")
		>>> setFieldPageList (recentFirst . excludeTag "icelandic") 
				"templates/blog_list_item.html" "posts" ("blog/*" `mappend` inGroup Nothing)
		>>> applyTemplateCompiler "templates/blog_list.html"
		>>> commonCompiler

	-- Tags
	create "tags" $ 
		requireAll ("blog/*" `mappend` inGroup Nothing) (\_ ps -> readTags ps :: Tags String)

	-- Add a tag list compiler for every tag
	match "tags/*" $ route pageRoute
	metaCompile    $ require_ "tags"
		>>> arr tagsMap
		>>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

	-- Render RSS feed
	match "rss.xml"  $ route idRoute
	create "rss.xml" $
		requireAll_ ("blog/*" `mappend` inGroup Nothing)
			>>> mapCompiler (arr $ copyBodyToField "description")
			>>> arr (excludeTag "icelandic")
			>>> renderRss feedConfiguration

	-- Read templates
	match "templates/*" $ compile templateCompiler

-- | Auxilary compiler: like pageCompiler without running the result through Pandoc
pageCompiler' =
   readPageCompiler >>>
   addDefaultFields >>>  -- Sets some things like tutorials/faq.markdown
   arr applySelf          -- Used to fill in $var$s in the page

-- | Auxilary compiler: most of the common functionality
commonCompiler = 
	arr (getField "title" &&& id)
		>>> arr (\(t, p) -> (trySetField "bodyTitle" t . trySetField "pageTitle" t) p)
		>>> requireA "tagcloud" (setFieldA "tagcloud" (O.renderTagCloud))
		>>> applyTemplateCompiler "templates/layout.html" 

--  changeFieldIfNull :: String -> String -> Page a -> Page a
--  changeFieldIfNull field value = changeField field (\value' -> value)

--  setFieldIfNull :: String -> String -> Page a -> Page a
--  setFieldIfNull field value p = if null (getField field p)
									--  then (setField field value) p
									--  else p


renderTagCloud' :: Compiler (Tags String) String
--  renderTagCloud' = renderTagCloud tagIdentifier 100 120
renderTagCloud' = renderTagCloud tagIdentifier 100 120

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/blog_list_item.html" (\ps t -> map (applyTemplate t) ps)
        >>> arr mconcat
        >>> arr pageBody

addTeaser :: Compiler (Page String) (Page String) 
addTeaser = arr (copyBodyToField "teaser")
    >>> arr (changeField "teaser" extractTeaser)
    >>> (arr $ getField "url" &&& id) 
    >>> fixTeaserResourceUrls
    >>> (id &&& arr pageBody)
    >>> arr (\(p, b) -> setField "readmore" 
                        (if (isInfixOf "<!--MORE-->" (pageBody p)) 
                         then (readMoreLink p) else "") p)
      where
        extractTeaser = unlines . (noTeaser . extractTeaser') . lines
        extractTeaser' = takeWhile (/= "<!--MORE-->")
        
        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) = 
          drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : (noTeaser xs)
        
        readMoreLink :: Page String -> String
        readMoreLink p = renderHtml $ H.div ! A.class_ "readmore" $ 
                         H.a ! A.href (toValue $ getField "url" p) $ 
                         preEscapedString "Read more &raquo;"
                         
        fixTeaserResourceUrls :: Compiler (String, (Page String)) (Page String)
        fixTeaserResourceUrls = arr $ (\(url, p) -> fixResourceUrls' url p)
          where fixResourceUrls' url p = 
                  changeField "teaser" (fixResourceUrls'' (takeDirectory url)) p

dropFileNameFromUrl :: Compiler (Page a) (Page a) 
dropFileNameFromUrl = arr (changeField "url" $ dropFileName)

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
	constA (mempty, posts)
		>>> addPostList
		>>> arr (setField "pageTitle" (capitalize tag))
		>>> applyTemplateCompiler "templates/blog_list.html"
		>>> commonCompiler
	where capitalize (x:xs) = toUpper x : xs

excludeTag :: String -> [Page a] -> [Page a]
excludeTag tag = filter (\p -> tag `notElem` O.getTags p)

fixResourceUrls'' :: String -> String -> String
fixResourceUrls'' path = O.withUrls ["src", "href", "data"] 
                         (\x -> if '/' `elem` x then x 
                                else path ++ "/" ++ x)

-- | Take a page like @\"/about/notebooks.md\"@ and route it to
-- @\"/about/notebooks\"@, i.e. turn a filename into a drectory.
pageRoute :: Routes
pageRoute = customRoute fileToDirectory

blogRoute :: Routes
blogRoute = customRoute blogToDirectory

-- | Turn a filename reference into a directory with an index file.
fileToDirectory :: Identifier a -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

blogToDirectory :: Identifier a -> FilePath
blogToDirectory = formatDate . dropFirstDirectory . fileToDirectory
	where
		dropFirstDirectory = joinPath . drop 1 . splitDirectories
		formatDate = formatDate' . split '-'
		formatDate' (year:month:_:rest) = joinPath [year, month, intercalate "-" rest]
		formatDate' xs = intercalate "-" xs
		dashToDirectory = map dashToSlash
		dashToSlash '-' = '/'
		dashToSlash c = c
		split :: (Eq a) => a -> [a] -> [[a]]
		split x xs = split' x xs []
		split' _ [] yys = reverse yys
		split' x xs yys = let (as, bs) = span (/= x) xs in
								case bs of 
									[] -> split' x [] (as:yys)
									_  -> split' x (tail bs) (as:yys)
		join :: a -> [[a]] -> [a]
		join x xxs = join' x xxs []
		join' x [] ys  = ys
		join' x (xs:xxs) ys = join' x xxs $ ys++(x:xs)


clearField :: String -> Page a -> Page a
clearField field = setField field ""


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
	{ feedTitle       = "Gisli's RSS feed."
	, feedDescription = "Jack of all trades, master of none"
	, feedAuthorName  = "Gisli Kristjansson"
	, feedAuthorEmail = "gislik@hamstur.is"
	, feedRoot        = "http://gisli.hamstur.is"
	}
