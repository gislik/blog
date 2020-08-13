{-# LANGUAGE OverloadedStrings, TupleSections #-}
import            Data.Typeable                    (Typeable)
import            Data.Binary                      (Binary)
import            Data.Maybe                       (fromMaybe, listToMaybe)
import            Data.Monoid                      ((<>), mconcat)
import            Data.Functor                     ((<$>))
import            Data.List                        (intercalate, intersperse, unfoldr, sortBy, isSuffixOf)
import            Data.Char                        (toLower, toUpper)
import            Data.Time.Clock                  (UTCTime (..))
import            Control.Applicative              ((<|>))
import            Control.Monad                    (msum, filterM, (<=<), liftM, forM, filterM)
import            Control.Monad.Fail               (MonadFail)
import            System.Environment               (getArgs)
import            Data.Time.Format                 (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import            Text.Printf                      (printf)
import            Text.Blaze.Html                  (toHtml, toValue, (!))
import            Text.Blaze.Html.Renderer.String  (renderHtml)
import qualified  Data.Set                         as S
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

      pages      <- buildPages Nothing visiblePattern
      categories <- buildCategories visiblePattern (fromCapture "*/index.html")
      tags       <- buildTags visiblePattern (fromCapture "tags/*/index.html")

      -- static content
      match staticPattern $ do
         route   rootRoute
         compile copyFileCompiler

      -- static css
      match "css/**.css" $ do
         route idRoute
         compile compressCssCompiler

      match ("css/**.scss" .&&. complement "css/**_*.scss") $ do
         route $ setExtension "css"
         compile $  sassCompiler  >>=
            return . fmap compressCss

      match "css/webfonts/*" $ do
         route idRoute
         compile copyFileCompiler

      -- static pages
      match "*.md" $ do
         route pageRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx

      -- index
      match "index.html" $ do
         route idRoute
         compile $ do
            getResourceBody
               >>= applyAsTemplate (pageCtx 1 pages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx 

      -- blog pages
      paginateRules pages $ \i _ -> do
         route idRoute
         compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i pages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx

      -- blog category index
      tagsRules categories $ \category pattern -> do
         catPages <- buildPages (Just category) pattern
         route idRoute
         compile $ do
            makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx 1 catPages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx
         paginateRules catPages $ \i _ -> do -- blog category pages
            route idRoute
            compile $ do
               makeItem category
                  >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i catPages categories tags)
                  >>= loadAndApplyTemplate "templates/default.html" defaultCtx

      -- blog tags index 
      tagsRules tags $ \tag pattern -> do
         tagPages <- buildPages (Just $ "tags" </> tag) pattern
         route idRoute
         compile $ do
            makeItem tag
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx 1 tagPages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx
         paginateRules tagPages $ \i _ -> do -- blog tags pages
            route idRoute
            compile $ do
               makeItem tag
                  >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i tagPages categories tags)
                  >>= loadAndApplyTemplate "templates/default.html" defaultCtx

      -- blogs
      match allPattern $ do
         route blogRoute
         compile $ pandocCompiler
            >>= saveSnapshot blogSnapshot
            >>= loadAndApplyTemplate "templates/blog-detail.html"    (blogDetailCtx categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx

      -- slides
      match "slides/*.md" $ do
         route slidesRoute
         compile $ pandocCompiler
            >>= saveSnapshot slidesSnapshot
            >>= loadAndApplyTemplate "templates/slides.html" slidesDetailCtx

      match "slides/**" $ do
        route   slidesAssetsRoute
        compile copyFileCompiler

      create ["slides/index.html"] $ do
         route  idRoute
         compile $ makeItem "Slides"
            >>= loadAndApplyTemplate "templates/slides-list.html" slidesCtx
            >>= loadAndApplyTemplate "templates/default.html" slidesCtx

      -- rss
      create ["rss/index.html"] $ do
         route idRoute
         compile $ renderBlogRss <=< fmap (take 20) . loadBlogs $ visiblePattern

      -- templates
      match "templates/*.html" $ compile templateCompiler

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

blogOrder :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
blogOrder = recentFirst

blogFeedConfiguration :: FeedConfiguration
blogFeedConfiguration = FeedConfiguration 
                      { feedTitle = "Gísli Kristjánsson"
                      , feedDescription = "Jack of all trades, master of none"
                      , feedAuthorName = "G&iacute;sli Kristj&aacute;nsson"
                      , feedAuthorEmail = "gislik@hamstur.is"
                      , feedRoot = "http://gisli.hamstur.is"
                      }

slidesSnapshot :: Snapshot
slidesSnapshot = "slides-content"

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
   polishFunction   "polish"                                              <>
   missingField

pageCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
pageCtx i pages categories tags = 
      listField "blogs" (blogDetailCtx categories tags) (loadBlogs pattern) <>
      field "categories" (const . renderTagList' $ categories)         <>
--      constField "title" "Pagination"                                  <>
      pagesField pages i                                               <>
      defaultCtx
  where
      pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
      pagesField pages i = replaceWithBase . dropIndex $ aliasContext' pages i
      dropIndex = mapContextP (".url" `isSuffixOf`) dropFileName 
      replaceWithBase = mapContextP ("pages.previous.url" `isSuffixOf`) dropOne
      dropOne url | "/1/" `isSuffixOf` url = joinPath . reverse . tail . reverse . splitPath $ url
      dropOne url                          = url
      mapContextP p f c'@(Context c) = Context $ \k a i -> 
        if p k then unContext (mapContext f c') k a i else c k a i
      aliasContext' pages = aliasContext alias . paginateContext pages
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
      dateField "date" "%B %e, %Y"             <>
      mapContext dropFileName (urlField "url") <>
      categoryField' "category" categories     <>
      -- tagsField "tags" categories              <>
      -- tagsFieldWith (getTags <=< return . fromFilePath . (</> "index.html") . toFilePath) simpleRenderLink (mconcat . intersperse ",  ") "tags" tags <>
      tagsField "tags" tags <>
      teaserField "summary" blogSnapshot       <>
      defaultCtx

slidesCtx :: Context String
slidesCtx = 
    slidesTitleField "title"                                            <> 
    listField "slides" slidesDetailCtx (loadSlides "slides/*.md") <> 
    defaultCtx

slidesDetailCtx :: Context String
slidesDetailCtx = 
    slidesTitleField "title" <> 
    dateField "date" "%B %e, %Y" <> 
    mapContext dropFileName (urlField "url") <>
    functionField "featureimage" (\_ -> liftM (fromMaybe "") . getRoute . fromFilePath . featuredFileName)  <>
    defaultCtx
  where
    featuredFileName = flip replaceExtension "png" . toFilePath . itemIdentifier

rssCtx :: Context String
rssCtx = 
      cdataContext metadataField    <>
      bodyField "description"       <>
      urlField "url"                <>
      defaultCtx
   where
      cdataContext = mapContext (\s -> "<![CDATA[" <> s <> "]]>")

--------------------------------------------------------------------------------
-- ROUTES
--------------------------------------------------------------------------------
rootRoute :: Routes 
rootRoute = customRoute (joinPath . dropDirectory . splitPath . toFilePath)
  where
    dropDirectory [] = []
    dropDirectory ("/":ds) = dropDirectory ds
    dropDirectory ds = tail ds

pageRoute :: Routes
pageRoute = removeExtension `composeRoutes` addIndex
   where 
      removeExtension       = setExtension mempty
      addIndex              = postfixRoute "index.html"
      postfixRoute postfix  = customRoute $ (</> postfix) . toFilePath

blogRoute :: Routes
blogRoute = 
      customRoute (takeFileName . toFilePath)     `composeRoutes`
      metadataRoute dateRoute                     `composeRoutes` 
      dropDateRoute                               `composeRoutes` 
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
      yearRoute     `composeRoutes`
      monthRoute    `composeRoutes`
      dropDayRoute  
   where 
      yearRoute = gsubRoute "[[:digit:]]{4}-" (\xs -> take 4 xs <> "/")
      monthRoute = gsubRoute "/[[:digit:]]{2}-" (\xs -> "/" <> (take 2 . drop 1) xs <> "/")
      dropDayRoute = gsubRoute "/[[:digit:]]{2}-" (const "/")


--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------
-- contexts
prettyTitleField :: String -> Context a
prettyTitleField = mapContext (defaultTitle . pageTitle) . pathField 
   where
      pageTitle :: String -> String
      pageTitle = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName

      defaultTitle :: String -> String
      defaultTitle "." = "Blog"
      defaultTitle x = x

      capitalize :: String -> String
      capitalize [] = []
      capitalize (x:xs) = toUpper x : map toLower xs

slidesTitleField :: String -> Context a
slidesTitleField = mapContext (defaultTitle . slideTitle) . pathField
   where
      slideTitle :: String -> String
      slideTitle = capitalize . drop 11 . takeBaseName

      defaultTitle :: String -> String
      defaultTitle [] = "Slides"
      defaultTitle x = x

      capitalize :: String -> String
      capitalize [] = []
      capitalize (x:xs) = toUpper x : map toLower xs
      

categoryField' :: String -> Tags -> Context a -- drops the filename from the link
-- categoryField' = tagsFieldWith getCategory simpleRenderLink (mconcat . intersperse ", ")
categoryField' = tagsFieldWith getCategory simpleRenderLink mconcat
   where
      getCategory :: Identifier -> Compiler [String]
      getCategory = return . return . takeBaseName . takeDirectory . toFilePath

aliasContext :: (String -> String) -> Context a -> Context a
aliasContext f (Context c) = Context $ \k a i -> c (f k) a i <|> c' k
   where 
      c' k = noResult $ unwords ["Tried to alias", k, "as", f k, "which doesn't exist"]

polishFunction :: String -> Context a
polishFunction name = functionField name f
  where f (a:_) i = return a

-- compilers
loadBlogs :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadBlogs = blogOrder <=< flip loadAllSnapshots blogSnapshot

loadSlides = blogOrder <=< flip loadAllSnapshots slidesSnapshot

buildPages :: (MonadMetadata m, Functor m) => Maybe String -> Pattern -> m Paginate
buildPages mprefix pattern = 
   buildPaginateWith 
      (return . paginateEvery blogPerPage)
      pattern
      (asIdentifier mprefix . show)
   where
      asIdentifier :: Maybe String -> String -> Identifier
      asIdentifier Nothing    = fromCapture "*/index.html" 
      asIdentifier (Just pre) = fromCapture . fromGlob $ pre <> "/*/index.html" 

renderTagList' :: Tags -> Compiler String -- drops the filename from the link
renderTagList' = renderTags makeLink (intercalate " ")
   where
      makeLink tag url count _ _ = renderHtml $
         H.a ! A.href (toValue . dropFileName $ url) $ toHtml (tag ++ " (" ++ show count ++ ")")

renderBlogRss :: [Item String] -> Compiler (Item String)
renderBlogRss = renderRss blogFeedConfiguration rssCtx

sassCompiler :: Compiler (Item String)
sassCompiler = getUnderlying >>=
        return . toFilePath >>= 
        \file -> unixFilter "sass" [file] ""  >>=
        makeItem

-- metadata
includeTagM :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
includeTagM tag = filterTagsM (return . elem tag)

filterTagsM :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTagsM p = filterM $ p <=< getTags 

-- html
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl . dropFileName $ filePath) $ toHtml tag

-- dates
tryParseDate :: Identifier -> Metadata -> Maybe UTCTime
tryParseDate = tryParseDateWithLocale defaultTimeLocale

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
