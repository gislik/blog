--------------------------------------------------------------------------------
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

{-
TODO: 
   0. Manual dependencies for index pages
   1. Series
   2. Count words (5 min read)
   3. Tags
   4. RSS -> Twitter?
   5. RSS respecting <!--more-->
   6. Fork Hakyll and add supporting code?
-}

main :: IO ()
main = do
   isWatching <- fmap (== "watch") <$> listToMaybe <$> getArgs
   let allPattern = case isWatching of
                     Just True -> (blogPattern .||. draftPattern) 
                     _         -> blogPattern
   hakyll $ do

   excludePattern <- liftM fromList $ includeTag "icelandic" <=< getMatches $ blogPattern
   let visiblePattern = allPattern .&&. complement excludePattern

   match ("*.png" .||. "*.txt") $ do
      route   idRoute
      compile copyFileCompiler

   match "assets/**" $ do
      route   idRoute
      compile copyFileCompiler

   match "img/**" $ do
      route   idRoute
      compile copyFileCompiler

   match "*.markdown" $ do
      route prettyRoute
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext

   categories <- buildCategories visiblePattern (fromCapture "*/index.html")
   pages <- buildPages Nothing visiblePattern

   -- posts
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
   
   -- tags
   {- tags <- buildTags' -}
   {- tagsRules tags $ \tag pattern -> do -}
      {- route idRoute -}
      {- compile $ makeItem tag -}
               {- >>= loadAndApplyTemplate "templates/blog-list.html" (tagCtx pattern) -}
               {- >>= loadAndApplyTemplate "templates/default.html" defaultContext -}

   create ["rss/index.html"] $ do
      route idRoute
      compile $ renderBlogRss <=< fmap (take 20) . loadBlogs $ visiblePattern

   match "templates/*.html" $ compile templateCompiler


--------------------------------------------------------------------------------
blogPattern :: Pattern
blogPattern = "blog/**"

draftPattern :: Pattern
draftPattern = "drafts/**"

--------------------------------------------------------------------------------
blogSnapshot :: Snapshot
blogSnapshot = "blog-content"

--------------------------------------------------------------------------------
blogPerPage :: Int
blogPerPage = 4

--------------------------------------------------------------------------------
blogOrder :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
blogOrder = recentFirst

--------------------------------------------------------------------------------
blogFeedConfiguration :: FeedConfiguration
blogFeedConfiguration = FeedConfiguration 
                      { feedTitle = "Gísli Kristjáns"
                      , feedDescription = "Jack of all trades, master of none"
                      , feedAuthorName = "G&iacute;sli Kristj&aacute;nsson"
                      , feedAuthorEmail = "gislik@hamstur.is"
                      , feedRoot = "http://gisli.hamstur.is"
                      }

renderBlogRss :: [Item String] -> Compiler (Item String)
renderBlogRss = renderRss blogFeedConfiguration rssCtx

--------------------------------------------------------------------------------
blogListField :: String -> Tags -> Compiler [Item String] -> Context String
blogListField name categories loader = listField name (blogDetailCtx categories) loader

--------------------------------------------------------------------------------
indexCtx :: Context String
indexCtx = 
   prettyTitleField "title" <> 
   bodyField        "body"  <>
   metadataField            <>
   urlField         "url"   <>
   pathField        "path"  <>
   missingField

mapContextP :: (String -> Bool) -> (String -> String) -> Context a -> Context a
mapContextP p f c'@(Context c) = Context $ \k a i -> 
                      if p k 
                        then unContext (mapContext f c') k a i 
                        else c k a i

paginateContext' :: Paginate -> PageNumber -> Context String
paginateContext' pages i = mapContextP (isSuffixOf "Url") dropFileName (paginateContext pages i)

--------------------------------------------------------------------------------
blogDetailCtx :: Tags -> Context String
blogDetailCtx categories  = 
      dateField "date" "%B %e, %Y"         <>
      mapTakeDirectory (urlField "url")    <>
      categoryField' "category" categories <>
      teaserField "teaser" blogSnapshot    <>
      defaultContext
   where
      mapTakeDirectory = mapContext dropFileName
   
--------------------------------------------------------------------------------
rssCtx :: Context String
rssCtx = 
      cdataContext metadataField    <>
      bodyField "description"       <>
      urlField "url"                <>
      defaultContext
   where
      cdataContext = mapContext (\s -> "<![CDATA[" <> s <> "]]>")

--------------------------------------------------------------------------------
pageCtx :: PageNumber -> Paginate -> Tags -> Context String
pageCtx i pages categories = 
      blogListField "blogs" categories (loadBlogs pattern)          <>
      field "categories" (const . renderTagList' $ categories)      <>
      constField "title" "Pagination"                               <>
      paginateContext' pages i                                      <>
      defaultContext
  where
      pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages

--------------------------------------------------------------------------------
prettyTitleField :: String -> Context a
prettyTitleField = mapContext (g . f) . pathField
   where
      f = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName
      g "." = "Blog"
      g x = x

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : map toLower xs

--------------------------------------------------------------------------------
buildPages :: (MonadMetadata m, Functor m) => Maybe String -> Pattern -> m Paginate
buildPages mprefix pattern = 
  buildPaginateWith 
    (return . paginateEvery blogPerPage)
    pattern
    (asIdentifier mprefix . show)
  where
    asIdentifier Nothing    = fromCapture "*/index.html" 
    asIdentifier (Just pre) = fromCapture . fromGlob $ pre <> "/*/index.html" 

renderTagList' :: Tags -> Compiler String
renderTagList' = renderTags makeLink (intercalate " ")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue . dropFileName $ url) $ toHtml (tag ++ " (" ++ show count ++ ")")

--------------------------------------------------------------------------------
-- | Render the category in a link
categoryField' :: String     -- ^ Destination key
              -> Tags       -- ^ Tags
              -> Context a  -- ^ Context
categoryField' =
  tagsFieldWith getCategory simpleRenderLink (mconcat . intersperse ", ")

--------------------------------------------------------------------------------
-- | Render one tag link
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl . dropFileName $ filePath) $ toHtml tag


prettyRoute :: Routes
prettyRoute = removeExtension `composeRoutes` addIndex
   where 
      removeExtension  = setExtension ""
      addIndex         = postfixRoute "/index.html"

--------------------------------------------------------------------------------
blogRoute :: Routes
blogRoute = 
      customRoute (takeFileName . toFilePath)     `composeRoutes`
      metadataRoute dateRoute                     `composeRoutes` 
      dropDateRoute                               `composeRoutes` 
      prettyRoute
   where 
      dateFolder :: Identifier -> Metadata -> FilePath
      dateFolder id' = maybe "" (formatTime defaultTimeLocale "%Y/%m") . tryParseDate id'
      
      dateRoute :: Metadata -> Routes
      dateRoute metadata = customRoute $ \id' -> joinPath [dateFolder id' metadata, toFilePath id']

      dropDateRoute :: Routes
      dropDateRoute = gsubRoute "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" (const "")

loadBlogs :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadBlogs = blogOrder <=< flip loadAllSnapshots blogSnapshot

--------------------------------------------------------------------------------
postfixRoute :: String -> Routes
postfixRoute postfix = customRoute $ (++ postfix) . toFilePath

--------------------------------------------------------------------------------

filterTags :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTags p = filterM $ p <=< getTags 
--------------------------------------------------------------------------------
includeTag :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
includeTag tag = filterTags (return . elem tag)
--------------------------------------------------------------------------------
-- | Obtain categories from a page.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath

--------------------------------------------------------------------------------
tryParseDate :: Identifier -> Metadata -> Maybe UTCTime
tryParseDate = tryParseDateWithLocale defaultTimeLocale

--------------------------------------------------------------------------------
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

between :: Ord a => a -> a -> a -> Bool
between l h x | x < l || x > h = False
between _ _ _                  = True

betweenPages :: PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber
betweenPages l h x = if between l h x then Just x else Nothing

