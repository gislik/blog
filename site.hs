{-# LANGUAGE OverloadedStrings, TupleSections #-}
import            Data.Typeable                    (Typeable)
import            Data.Binary                      (Binary)
import            Data.Maybe                       (fromMaybe, listToMaybe)
import            Data.Monoid                      ((<>), mconcat)
import            Data.Functor                     ((<$>))
import            Data.List                        (intercalate, intersperse, unfoldr, sortBy, isSuffixOf)
import            Data.Char                        (toLower, toUpper)
import            Data.Time.Clock                  (UTCTime (..))
import            Control.Monad                    (msum, filterM, (<=<), liftM, forM, filterM)
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
   3. Tags
   4. RSS -> Twitter?
   5. RSS respecting <!--more-->
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

      categories <- buildCategories visiblePattern (fromCapture "*/index.html")
      pages <- buildPages Nothing visiblePattern

      -- static content
      match ("*.png" .||. "*.txt" .||. "assets/**" .||. "img/**") $ do
         route   idRoute
         compile copyFileCompiler

      -- slides
      match "slides/*.markdown" $ do
         route slidesRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/slides.html" defaultContext

      match "slides/**" $ do
        route   slidesAssetsRoute
        compile copyFileCompiler

      -- static pages
      match "*.markdown" $ do
         route pageRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

      -- blogs
      match allPattern $ do
         route blogRoute
         compile $ pandocCompiler
            >>= saveSnapshot blogSnapshot
            >>= loadAndApplyTemplate "templates/blog.html"    (blogDetailCtx categories)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

      -- index
      match "index.html" $ do
         route idRoute
         compile $ do
            getResourceBody
               >>= applyAsTemplate (pageCtx 1 pages categories)
               >>= loadAndApplyTemplate "templates/default.html" indexCtx 

      -- pages
      paginateRules pages $ \i _ -> do
         route idRoute
         compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i pages categories)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

      -- category index
      tagsRules categories $ \category pattern -> do
         catPages <- buildPages (Just category) pattern
         route idRoute
         compile $ do
            makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx 1 catPages categories)
               >>= loadAndApplyTemplate "templates/default.html" indexCtx

         -- category pages
         paginateRules catPages $ \i _ -> do
            route idRoute
            compile $ do
               makeItem category
                  >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i catPages categories)
                  >>= loadAndApplyTemplate "templates/default.html" defaultContext

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

blogSnapshot :: Snapshot
blogSnapshot = "blog-content"

blogPerPage :: Int
blogPerPage = 4

blogOrder :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
blogOrder = recentFirst

blogFeedConfiguration :: FeedConfiguration
blogFeedConfiguration = FeedConfiguration 
                      { feedTitle = "Gísli Kristjáns"
                      , feedDescription = "Jack of all trades, master of none"
                      , feedAuthorName = "G&iacute;sli Kristj&aacute;nsson"
                      , feedAuthorEmail = "gislik@hamstur.is"
                      , feedRoot = "http://gisli.hamstur.is"
                      }

--------------------------------------------------------------------------------
-- CONTEXTS
--------------------------------------------------------------------------------
indexCtx :: Context String
indexCtx = 
   prettyTitleField "title" <> 
   bodyField        "body"  <>
   metadataField            <>
   urlField         "url"   <>
   pathField        "path"  <>
   missingField

pageCtx :: PageNumber -> Paginate -> Tags -> Context String
pageCtx i pages categories = 
      blogListField "blogs" categories (loadBlogs pattern)      <>
      field "categories" (const . renderTagList' $ categories)  <>
      constField "title" "Pagination"                           <>
      paginateContext' pages i                                  <>
      defaultContext
  where
      pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
      paginateContext' pages i = mapContextP (isSuffixOf "Url") dropFileName (paginateContext pages i)
      blogListField name categories loader = listField name (blogDetailCtx categories) loader

blogDetailCtx :: Tags -> Context String
blogDetailCtx categories  = 
      dateField "date" "%B %e, %Y"              <>
      mapContext dropFileName (urlField "url")  <>
      categoryField' "category" categories      <>
      teaserField "teaser" blogSnapshot         <>
      defaultContext

rssCtx :: Context String
rssCtx = 
      cdataContext metadataField    <>
      bodyField "description"       <>
      urlField "url"                <>
      defaultContext
   where
      cdataContext = mapContext (\s -> "<![CDATA[" <> s <> "]]>")

--------------------------------------------------------------------------------
-- ROUTES
--------------------------------------------------------------------------------
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

pageRoute :: Routes
pageRoute = removeExtension `composeRoutes` addIndex
   where 
      removeExtension       = setExtension mempty
      addIndex              = postfixRoute "index.html"
      postfixRoute postfix  = customRoute $ (</> postfix) . toFilePath

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------
-- contexts
mapContextP :: (String -> Bool) -> (String -> String) -> Context a -> Context a
mapContextP p f c'@(Context c) = Context $ \k a i -> 
                      if p k 
                        then unContext (mapContext f c') k a i 
                        else c k a i

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

categoryField' :: String -> Tags -> Context a -- drops the filename from the link
categoryField' = tagsFieldWith getCategory simpleRenderLink (mconcat . intersperse ", ")
   where
      getCategory :: Identifier -> Compiler [String]
      getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- compilers
loadBlogs :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadBlogs = blogOrder <=< flip loadAllSnapshots blogSnapshot

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
   let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
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
