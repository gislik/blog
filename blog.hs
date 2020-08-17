{-# LANGUAGE OverloadedStrings, TupleSections #-}
import            Data.Maybe                       (fromMaybe, listToMaybe)
import            Data.Monoid                      ((<>), mconcat)
import            Data.Functor                     ((<$>), fmap)
import            Data.List                        (intercalate, intersperse, foldl')
import            Data.Char                        (toLower, toUpper)
import            Data.Time.Clock                  (UTCTime (..))
import            Control.Applicative              ((<|>), Alternative(..))
import            Control.Monad                    (msum, filterM, (<=<), liftM, filterM)
import            Control.Monad.Fail               (MonadFail)
import            System.Environment               (getArgs)
import            Data.Time.Format                 (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import            Text.Blaze.Html                  (toHtml, toValue, (!))
import            Text.Blaze.Html.Renderer.String  (renderHtml)
import            Text.HTML.TagSoup                (Tag(..), (~==), isTagText, fromTagText)
import qualified  Data.Map                         as M
import qualified  Text.Blaze.Html5                 as H
import qualified  Text.Blaze.Html5.Attributes      as A
import            System.FilePath                  
import            Hakyll

--------------------------------------------------------------------------------
-- TODO
--------------------------------------------------------------------------------
{-
   1. Series
   2. Count words (5 min read)
   3. RSS -> Twitter?
   4. RSS respecting <!--more-->
-}

--------------------------------------------------------------------------------
-- SITE
--------------------------------------------------------------------------------
main :: IO ()
main = do
   isWatching <- fmap (== "watch") <$> listToMaybe <$> getArgs
   let allPattern = case isWatching of
                        Just True -> (blogPattern .||. draftPattern) 
                        _         -> blogPattern
   hakyll $ do

      excludePattern <- liftM fromList $ includeTagM "icelandic" <=< getMatches $ blogPattern
      let visiblePattern = allPattern .&&. complement excludePattern

      pages      <- buildPages visiblePattern (\i -> fromCapture "*/index.html" (show i))
      categories <- buildCategories visiblePattern (fromCapture "*/index.html")
      tags       <- buildTags visiblePattern (fromCapture "tags/*/index.html")

      -- static content
      match staticPattern $ do
         route   $ rootRoute
         compile $ copyFileCompiler

      -- static css
      match "css/**.css" $ do
         route   $ idRoute
         compile $ compressCssCompiler

      match ("css/**.scss" .&&. complement "css/**_*.scss") $ do
         route   $ setExtension "css"
         compile $ sassCompiler  
            >>= return . fmap compressCss
            >>= relativizeUrls

      match "css/webfonts/*" $ do
         route   $ idRoute
         compile $ copyFileCompiler

      -- static pages
      match "*.md" $ do
         route   $ pageRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

      -- index
      match "index.html" $ do
         route   $ idRoute
         compile $ getResourceBody
            >>= applyAsTemplate (pageCtx 1 pages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx 
            >>= indexCompiler
            >>= relativizeUrls

      -- blogs
      match allPattern $ do
         route   $ blogRoute
         compile $ pandocCompiler
            >>= saveSnapshot blogSnapshot
            >>= loadAndApplyTemplate "templates/blog-detail.html"    (blogDetailCtx categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- blog pages
      paginateRules pages $ \i _ -> do
         route   $ idRoute
         compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i pages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- blog category index
      tagsRules categories $ \category pattern -> do
         catPages <- buildPages pattern (\i -> fromCaptures "*/*/index.html" [category, show i])
         route   $ idRoute
         compile $ makeItem category
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx 1 catPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls
         paginateRules catPages $ \i _ -> do -- blog category pages
            route   $ idRoute
            compile $ makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i catPages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx
               >>= indexCompiler
               >>= relativizeUrls

      -- blog tags index 
      tagsRules tags $ \tag pattern -> do
         tagPages <- buildPages pattern (\i -> fromCaptures "tags/*/*/index.html" [tag, show i])
         route   $ idRoute
         compile $ makeItem tag
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx 1 tagPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls
         paginateRules tagPages $ \i _ -> do -- blog tags pages
            route idRoute
            compile $ do
               makeItem tag
                  >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i tagPages categories tags)
                  >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                  >>= indexCompiler
                  >>= relativizeUrls

      -- slides
      match "slides/*.md" $ do
         route   $ slidesRoute
         compile $ pandocCompiler
            >>= saveSnapshot slidesSnapshot
            >>= loadAndApplyTemplate "templates/slides.html" slidesDetailCtx
            >>= indexCompiler
            >>= relativizeUrls

      match "slides/**" $ do
         route   $ slidesAssetsRoute
         compile $ copyFileCompiler

      create ["slides/index.html"] $ do
         route   $ idRoute
         compile $ makeItem "Slides"
            >>= loadAndApplyTemplate "templates/slides-list.html" slidesCtx
            >>= loadAndApplyTemplate "templates/default.html" slidesCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- rss
      create ["rss/index.html"] $ do
         route idRoute
         compile $ renderBlogRss <=< fmap (take 20) . loadBlogs $ visiblePattern

      -- templates
      match "templates/*.html" $ 
         compile $ templateCompiler

--------------------------------------------------------------------------------
-- CONFIGURATION
--------------------------------------------------------------------------------
blogPattern :: Pattern
blogPattern = "blog/**"

draftPattern :: Pattern
draftPattern = "drafts/**"

staticPattern :: Pattern
staticPattern = "static/**"

blogSnapshot :: Snapshot
blogSnapshot = "blog-content"

blogPerPage :: Int
blogPerPage = 4

slidesSnapshot :: Snapshot
slidesSnapshot = "slides-content"

feedConfiguration :: FeedConfiguration
feedConfiguration =
   FeedConfiguration
      { feedTitle       = "Gísli Kristjánsson"
      , feedDescription = "Jack of all trades, master of none"
      , feedAuthorName  = "G&iacute;sli Kristj&aacute;nsson"
      , feedAuthorEmail = "gislik@hamstur.is"
      , feedRoot        = "http://gisli.hamstur.is"
      }

--------------------------------------------------------------------------------
-- CONTEXTS
--------------------------------------------------------------------------------
defaultCtx :: Context String
defaultCtx = 
   constField       "pagetitle" "Gísli Kristjánsson | Jack of all trades" <>
   -- prettyTitleField "title"                                               <>
   bodyField        "body"                                                <>
   metadataField                                                          <>
   titleField       "title"                                               <>
   urlField         "url"                                                 <>
   pathField        "path"                                                <>
   polishField      "polish"                                              
   -- missingField

pageCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
pageCtx i pages categories tags = 
      listField "blogs" (blogDetailCtx categories tags) (loadBlogs pattern) <>
      categoryListField "categories" categories                             <>
      tagsListField "tags" tags                                             <>
      pagesField i <>
      defaultCtx
  where
      pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
      pagesField = aliasContext alias . paginateContext pages
      alias "pages.first.number"    = "firstPageNum"
      alias "pages.first.url"       = "firstPageUrl"
      alias "pages.next.number"     = "nextPageNum"
      alias "pages.next.url"        = "nextPageUrl"
      alias "pages.previous.number" = "previousPageNum"
      alias "pages.previous.url"    = "previousPageUrl"
      alias "pages.last.number"     = "lastPageNum"
      alias "pages.last.url"        = "lastPageUrl"
      alias "pages.current.number"  = "currentPageNum"
      alias "pages.count"           = "numPages"
      alias x                       = x

blogDetailCtx :: Tags -> Tags -> Context String
blogDetailCtx categories tags = 
   dateField "date" "%B %e, %Y"                 <>
   mapContext dropFileName (urlField "url")     <>
   categoryField' "category" categories         <>
   tagsField' "tags" tags                       <>
   field "pages.next.url" nextBlog              <>
   field "pages.previous.url" previousBlog      <>
   defaultCtx                                   <>  -- summary from metadata
   teaserField "summary" blogSnapshot           <>  -- teaser is summary
   previewField "summary" blogSnapshot          <>  -- first paragraph is summary
   readingTimeField "reading.time" blogSnapshot

slidesCtx :: Context String
slidesCtx =
   slidesTitleField "title"                                      <>
   listField "slides" slidesDetailCtx (loadSlides "slides/*.md") <>
   defaultCtx

slidesDetailCtx :: Context String
slidesDetailCtx = 
   slidesTitleField "title"                                                                               <>
   dateField "date" "%B %e, %Y"                                                                           <>
   mapContext dropFileName (urlField "url")                                                               <>
   functionField "featureimage" (\_ -> liftM (fromMaybe "") . getRoute . fromFilePath . featuredFileName) <>
   defaultCtx
   where
      featuredFileName = flip replaceExtension "png" . toFilePath . itemIdentifier

rssCtx :: Context String
rssCtx = 
   cdataContext metadataField <>
   bodyField "description"    <>
   urlField "url"             <>
   defaultCtx
   where
      cdataContext = mapContext (\s -> "<![CDATA[" <> s <> "]]>")

--------------------------------------------------------------------------------
-- ROUTES
--------------------------------------------------------------------------------
rootRoute :: Routes 
rootRoute =
   customRoute (joinPath . dropDirectory . splitPath . toFilePath)
  where
    dropDirectory []       = []
    dropDirectory ("/":ds) = dropDirectory ds
    dropDirectory ds       = tail ds

pageRoute :: Routes
pageRoute = removeExtension `composeRoutes` addIndex
   where 
   removeExtension = setExtension mempty
   addIndex = postfixRoute "index.html"
   postfixRoute postfix = customRoute $ (</> postfix) . toFilePath

blogRoute :: Routes
blogRoute = 
   customRoute (takeFileName . toFilePath) `composeRoutes`
   metadataRoute dateRoute                 `composeRoutes`
   dropDateRoute                           `composeRoutes`
   pageRoute
   where 
      dateRoute metadata = customRoute $ \id' -> joinPath [dateFolder id' metadata, toFilePath id']
      dateFolder id' = maybe mempty (formatTime defaultTimeLocale "%Y/%m") . tryParseDate id'
      dropDateRoute = gsubRoute "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" (const mempty)

slidesRoute :: Routes
slidesRoute = 
   blogRoute `composeRoutes` prefixRoute "slides"
   where 
      prefixRoute prefix = customRoute $ (prefix </>) . toFilePath

slidesAssetsRoute :: Routes
slidesAssetsRoute = 
   yearRoute    `composeRoutes`
   monthRoute   `composeRoutes`
   dropDayRoute
   where 
      yearRoute = gsubRoute "[[:digit:]]{4}-" (\xs -> take 4 xs <> "/")
      monthRoute = gsubRoute "/[[:digit:]]{2}-" (\xs -> "/" <> (take 2 . drop 1) xs <> "/")
      dropDayRoute = gsubRoute "/[[:digit:]]{2}-" (const "/")

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------
-- contexts
-- prettyTitleField :: String -> Context a
-- prettyTitleField = mapContext (defaultTitle . pageTitle) . pathField 
--    where
--       pageTitle :: String -> String
--       pageTitle = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName
--       defaultTitle :: String -> String
--       defaultTitle "." = "Blog"
--       defaultTitle x = x
--       capitalize :: String -> String
--       capitalize [] = []
--       capitalize (x:xs) = toUpper x : map toLower xs

slidesTitleField :: String -> Context a
slidesTitleField = 
   mapContext (defaultTitle . slideTitle) . pathField
   where
      slideTitle :: String -> String
      slideTitle = capitalize . drop 11 . takeBaseName
      defaultTitle :: String -> String
      defaultTitle [] = "Slides"
      defaultTitle x = x
      capitalize :: String -> String
      capitalize [] = []
      capitalize (x:xs) = toUpper x : map toLower xs

categoryField' :: String -> Tags -> Context a 
categoryField' =
   tagsFieldWith getCategory (renderLink "@") mconcat

categoryListField :: String -> Tags -> Context a
categoryListField key tags = 
   field key (const $ renderList tags) 
   where
      renderList = renderTags makeLink (intercalate " ")
      makeLink tag url _ _ _ = renderHtml $ do
         "@"
         H.a ! A.href (toValue url) $ toHtml tag

tagsField' :: String -> Tags -> Context a 
tagsField' = 
   tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

tagsListField :: String -> Tags -> Context a
tagsListField key tags = 
   field key (const $ renderList tags) 
   where
      renderList = renderTags makeLink (intercalate " ")
      makeLink tag url _ _ _ = renderHtml $ do
         "#"
         H.a ! A.href (toValue url) $ toHtml tag

previewField :: String -> Snapshot -> Context String
previewField key snapshot  = 
   field key trim'
   where
      trim' item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList firstParagraph body
      firstParagraph = map fst . takeWhile (\(_, s) -> s > 0) . acc 0 . (map cnt)
      acc _ [] = []
      acc s ((x, s'):xs) = (x, s + s') : acc  (s + s') xs
      cnt tag@(TagOpen "p" _) = (tag, 1)
      cnt tag@(TagClose "p")  = (tag, -1)
      cnt tag               = (tag, 0)

readingTimeField :: String -> Snapshot -> Context String
readingTimeField key snapshot = 
   field key calculate
   where
      calculate :: Item String -> Compiler String
      calculate item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList acc body
      acc ts = [TagText (show (time ts))]
      time ts = foldl' count 0  ts `div` 265
      count n (TagText s) = n + length (words s)
      count n _           = n

aliasContext :: (String -> String) -> Context a -> Context a
aliasContext f (Context c) = 
   Context $ \k a i -> c (f k) a i <|> c' k
   where 
      c' k = noResult $ unwords ["Tried to alias", k, "as", f k, "which doesn't exist"]

polishField :: String -> Context String
polishField name = 
   functionField name f
   where 
      f [] _    = return ""
      f (a:_) _ = return a

-- compilers
loadBlogs :: Pattern -> Compiler [Item String]
loadBlogs = 
   recentFirst <=< flip loadAllSnapshots blogSnapshot

loadSlides :: Pattern -> Compiler [Item String]
loadSlides = 
   recentFirst <=< flip loadAllSnapshots slidesSnapshot

buildPages :: (MonadMetadata m, MonadFail m) => Pattern -> (PageNumber -> Identifier) -> m Paginate
buildPages pattern makeId = 
   buildPaginateWith
      (return . paginateEvery blogPerPage <=< sortRecentFirst) 
      pattern 
      makeId

renderBlogRss :: [Item String] -> Compiler (Item String)
renderBlogRss = 
   renderRss feedConfiguration rssCtx

sassCompiler :: Compiler (Item String)
sassCompiler = 
   getUnderlying >>=
   return . toFilePath >>= 
   \file -> unixFilter "sass" [file] ""  >>=
   makeItem

indexCompiler :: Item String -> Compiler (Item String)
indexCompiler x = 
   withItemBody (return . withTags dropIndex) x
   where
      dropIndex (TagOpen "a" attrs) = TagOpen "a" (href <$> attrs)
      dropIndex tag = tag
      href ("href", url) = ("href", dropFileName url)
      href z             = z

-- metadata
includeTagM :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
includeTagM tag = 
   filterTagsM (return . elem tag)

filterTagsM :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTagsM p = 
   filterM $ p <=< getTags 

-- html
renderLink :: String -> String -> (Maybe FilePath) -> Maybe H.Html
renderLink _ _   Nothing            = Nothing
renderLink pre text (Just url) =
   Just $ do
      toHtml pre
      H.a ! A.href (toValue $ toUrl url) $ toHtml text

-- dates
tryParseDate :: Identifier -> Metadata -> Maybe UTCTime
tryParseDate = 
   tryParseDateWithLocale defaultTimeLocale

tryParseDateWithLocale :: TimeLocale -> Identifier -> Metadata -> Maybe UTCTime
tryParseDateWithLocale locale id' metadata = do
   let tryField k fmt = lookupString k metadata >>= parseTime' fmt
       fn             = takeFileName $ toFilePath id'

   maybe empty' return $ msum $
      [tryField "published" fmt | fmt <- formats] ++
      [tryField "date"      fmt | fmt <- formats] ++
      [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
   where
      empty'     = fail $ "Hakyll.Web.Template.Context.getItemUTC: " 
                        ++ "could not parse time for " ++ show id'
      parseTime' = parseTimeM True locale 
      formats    =
         [ "%a, %d %b %Y %H:%M:%S %Z"
         , "%Y-%m-%dT%H:%M:%S%Z"
         , "%Y-%m-%d %H:%M:%S%Z"
         , "%Y-%m-%d"
         , "%B %e, %Y %l:%M %p"
         , "%B %e, %Y"
         ]


nextBlog :: Item String -> Compiler String
nextBlog blog = do
   blogs <- loadBlogs blogPattern :: Compiler [Item String]
   let idents = map itemIdentifier blogs
       ident = itemAfter idents (itemIdentifier blog)
   case ident of
      Just i -> (fmap (maybe empty toUrl) . getRoute) i
      Nothing -> empty
   where
      itemAfter xs x = 
         lookup x $ zip xs (tail xs)

previousBlog :: Item String -> Compiler String
previousBlog blog = do
   blogs <- loadBlogs blogPattern :: Compiler [Item String]
   let idents = map itemIdentifier blogs
       ident = itemBefore idents (itemIdentifier blog)
   case ident of
      Just i -> (fmap (maybe empty toUrl) . getRoute) i
      Nothing -> empty
   where
         itemBefore xs x =
            lookup x $ zip (tail xs) xs
