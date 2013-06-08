--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import              Data.Monoid         (mappend)
import              Control.Monad       (msum, filterM, liftM, (<=<))
import              Control.Applicative ((<$>))
import              System.Locale       (TimeLocale, defaultTimeLocale)
import              System.FilePath     (takeFileName, joinPath, dropFileName)
import              Data.List           (intercalate)
import              Data.Time.Format    (formatTime, parseTime)
import              Data.Time.Clock     (UTCTime (..))
import qualified    Data.Map            as M
import              Data.Maybe          (maybeToList)
import              Text.Hastache 
import              Text.Hastache.Context 
import qualified    Data.ByteString.Lazy.Char8 as LZ 
import              Hakyll

--------------------------------------------------------------------------------

{-
TODO: 
   1. Hastache support
   2. Links for next, previous pages
   3. New layout and adaptive design
   4. Fork Hakyll and add supporting code?
-}

main :: IO ()
main = hakyll $ do

   match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

   match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

   match "*.markdown" $ do
      route prettyRoute
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls

   match blogPattern $ do
      route blogRoute
      compile $ pandocCompiler
         >>= saveSnapshot "blog-content"
         >>= loadAndApplyTemplate "templates/post.html"    postDetailCtx
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls

   create ["archive.html"] $ do
      route prettyRoute
      compile $ makeItem ""
               >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
               >>= loadAndApplyTemplate "templates/default.html" defaultContext
               >>= relativizeUrls

   match "index.html" $ do
      route idRoute
      compile $ getResourceBody
               >>= applyAsTemplate indexCtx
               >>= loadAndApplyTemplate "templates/default.html" defaultContext 
               >>= relativizeUrls

   tags <- buildTags'
   tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ makeItem tag
               >>= loadAndApplyTemplate "templates/archive.html" (tagCtx pattern)
               >>= loadAndApplyTemplate "templates/default.html" defaultContext
               >>= relativizeUrls

   categories <- buildCategories'
   tagsRules categories $ \category pattern -> do
      route idRoute
      compile $ makeItem category
            >>= loadAndApplyTemplate "templates/archive.html" (categoryCtx pattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

   pages <- buildPages'
   tagsRules pages $ \page pattern -> do
      route idRoute
      compile $ makeItem page
            >>= loadAndApplyTemplate "templates/archive.html" (pageCtx pattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

   -- Initial support for Hastache
   match "index.html" $ version "test" $ do
      let context "name"   = MuVariable ("Haskell" :: String)
          context "unread" = MuVariable (100 :: Int)
      route $ constRoute "test/index.html"
      compile $ getResourceBody 
            >>= loadAndApplyHastache "templates/design.htm" context 
            >>= relativizeUrls


   match "templates/*.html" $ compile templateCompiler

   match "templates/*.htm" $ compile hastacheCompiler

--------------------------------------------------------------------------------
blogPattern :: Pattern
blogPattern = "blog/**"

--------------------------------------------------------------------------------
blogOrder :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
blogOrder = recentFirst

--------------------------------------------------------------------------------
indexCtx :: Context String
indexCtx = 
      field "posts" posts      `mappend`
      field "tags" tags        `mappend`
      field "categories" cats  `mappend`
      field "pages" pages      `mappend`
      functionField "sum" sumFunction
   where
      posts = const $ postList blogPattern $ fmap (take 10) . blogOrder <=< excludeTag "icelandic"
      tags  = const $ renderTagList =<< buildTags'
      cats  = const $ renderTagList =<< buildCategories'
      pages = const $ renderTagList =<< buildPages'
      sumFunction :: [String] -> Item a -> Compiler String
      sumFunction ss _ = show <$> sum <$> mapM (return . read) ss

--------------------------------------------------------------------------------
postListCtx :: Pattern -> Context String
postListCtx pattern = field "posts" (const (postList pattern blogOrder))

--------------------------------------------------------------------------------
postDetailCtx :: Context String
postDetailCtx = 
      dateField "date" "%B %e, %Y"         `mappend`
      mapTakeDirectory (urlField "url")    `mappend`
      defaultContext
   where
      mapTakeDirectory = mapContext dropFileName
   
--------------------------------------------------------------------------------
archiveCtx :: Context String
archiveCtx = 
      postListCtx blogPattern        `mappend`
      constField "title" "Archives"  `mappend`
      defaultContext

--------------------------------------------------------------------------------
tagCtx :: Pattern -> Context String
tagCtx pattern = 
      postListCtx pattern        `mappend`
      constField "title" "Tags"  `mappend`
      defaultContext

--------------------------------------------------------------------------------
categoryCtx :: Pattern -> Context String
categoryCtx pattern = 
      postListCtx pattern               `mappend`
      constField "title" "Categories"   `mappend`
      defaultContext

--------------------------------------------------------------------------------
pageCtx :: Pattern -> Context String
pageCtx pattern = 
      postListCtx pattern               `mappend`
      constField "title" "Pagination"   `mappend`
      defaultContext

--------------------------------------------------------------------------------
buildTags' :: MonadMetadata m => m Tags
buildTags' = buildTags blogPattern (fromCapture "tag/*/index.html")

--------------------------------------------------------------------------------
buildCategories' :: MonadMetadata m => m Tags
buildCategories' = buildCategories blogPattern (fromCapture "category/*/index.html")

--------------------------------------------------------------------------------
buildPages' :: (MonadMetadata m, Functor m) => m Tags
buildPages' = buildTagsWith getPages' blogPattern (fromCapture "page/*/index.html")

--------------------------------------------------------------------------------
getPages' :: (MonadMetadata m, Functor m) => Identifier -> m [String]
getPages' = getPages blogPattern sorter 8 
   where 
      sorter :: (MonadMetadata m, Functor m) => [Identifier] -> m [Identifier]
      sorter = mapM (return.itemIdentifier) <=< blogOrder <=< mapM (\id' -> return $ Item id' "") 

--------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
postList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList pattern sortFilter = do
   blog    <- sortFilter =<< loadAllSnapshots pattern "blog-content"
   itemTpl <- loadBody "templates/post-item.html"
   applyTemplateList itemTpl postDetailCtx blog


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--  --------------------------------------------------------------------------------
--  prefixRoute :: String -> Routes
--  prefixRoute prefix = customRoute $ (prefix ++) . toFilePath

--------------------------------------------------------------------------------
postfixRoute :: String -> Routes
postfixRoute postfix = customRoute $ (++ postfix) . toFilePath

--  --------------------------------------------------------------------------------
--  infixRoute :: Int -> String -> Routes
--  infixRoute n s = customRoute $ 
   --  where (pre, post) = splitAt n . toFilePath

--------------------------------------------------------------------------------
--  folderRoute :: String -> Routes
--  folderRoute folder = customRoute $ addFolder folder . toFilePath
   --  where 
      --  addFolder fld fp = let (fld', fl') = splitFileName fp in joinPath [fld', fld, fl']

--------------------------------------------------------------------------------
filterTags :: MonadMetadata m => ([String] -> m Bool) -> [Item String] -> m [Item String]
filterTags p = filterM $ p <=< getTags . itemIdentifier 

--------------------------------------------------------------------------------
excludeTag :: MonadMetadata m => String -> [Item String] -> m [Item String]
excludeTag tag = filterTags (return . notElem tag)

{- -------------------------------------------------------------------------------- -}
{- includeTag :: MonadMetadata m => String -> [Item String] -> m [Item String] -}
{- includeTag tag = filterTags (return . elem tag) -}

{- -------------------------------------------------------------------------------- -}
{- -- | Obtain categories from a page. -}
{- getCategory :: MonadMetadata m => Identifier -> m [String] -}
{- getCategory = return . return . takeBaseName . takeDirectory . toFilePath -}

{- -------------------------------------------------------------------------------- -}
{- filterCategories :: MonadMetadata m => (Maybe String -> m Bool) -> [Item String] -> m [Item String] -}
{- filterCategories p = filterM $ p <=< listToMaybeM <=< getCategory . itemIdentifier -}
   {- where  -}
      {- listToMaybeM = return . listToMaybe -}

{- -------------------------------------------------------------------------------- -}
{- includeCategory :: MonadMetadata m => String -> [Item String] -> m [Item String] -}
{- includeCategory cat = filterCategories maybeEquals -}
   {- where  -}
      {- maybeEquals Nothing   = return False -}
      {- maybeEquals (Just x)  = return (x == cat) -}

--------------------------------------------------------------------------------
getPages :: MonadMetadata m => Pattern 
                            -> ([Identifier] -> m [Identifier]) 
                            -> Int 
                            -> Identifier 
                            -> m [String]
getPages pattern sorter k id' = do
   ids <- sorter =<< getMatches pattern
   let m = M.fromList $ concat $ zipWith idx [1..] (partitionAll k ids)
   return . maybeToList $ show <$> M.lookup id' m

   where
      idx :: Int -> [a] -> [(a, Int)]
      idx i = map (\x -> (x, i))

{- -------------------------------------------------------------------------------- -}
{- filterPages :: MonadMetadata m => Tags -> (Int -> m Bool) -> [Item String] -> m [Item String] -}
{- filterPages pages p = filterM $ \item ->  -}
   {- liftM (elem (itemIdentifier item)) (p' (tagsMap pages)) -}

   {- where -}
      {- p' = liftM concat . mapM (return . snd) <=< filterM (p . read . fst)  -}
      
{- --------------------------------------------------------------------------------    -}
{- includePage :: MonadMetadata m => Tags -> Int -> [Item String] -> m [Item String] -}
{- includePage tags pageId = filterPages tags (return . (==pageId)) -}

--------------------------------------------------------------------------------
partitionAll :: Int -> [a] -> [[a]]
partitionAll n xs = splitter (splitAt n xs)
   where 
      splitter :: ([a], [a]) -> [[a]]
      splitter (as,[]) = [as]
      splitter (as,bs) = as : partitionAll n bs

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
      parseTime' = parseTime locale 
      formats    =
         [ "%a, %d %b %Y %H:%M:%S %Z"
         , "%Y-%m-%dT%H:%M:%S%Z"
         , "%Y-%m-%d %H:%M:%S%Z"
         , "%Y-%m-%d"
         , "%B %e, %Y %l:%M %p"
         , "%B %e, %Y"
         ]

--------------------------------------------------------------------------------
--  tagLinks :: String -> Identifier
--  tagLinks = fromCapture "tags/*/"

--  withTagIdentifiers :: (String -> Identifier) -> Tags -> Tags
--  withTagIdentifiers f t = t { tagsMakeId = f }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type MuTemplate = String

hastacheCompiler :: Compiler (Item MuTemplate)
hastacheCompiler = cached "Hakyll.Web.Mu.hastacheCompiler" getResourceString

--------------------------------------------------------------------------------
applyHastacheWithConfig :: MuVar a
                        => MuConfig IO 
                        -> MuTemplate 
                        -> (String -> MuType IO)
                        -> Item a 
                        -> Compiler (Item String)
applyHastacheWithConfig cfg tpl ctx itm = do
   let ctx' "body" = MuVariable (itemBody itm)
       ctx' x      = ctx x
   content <- liftM LZ.unpack $ unsafeCompiler (hastacheStr cfg (encodeStr tpl) (mkStrContext ctx'))
   return $ itemSetBody content itm

--------------------------------------------------------------------------------
applyHastache :: MuVar a
              => MuTemplate
              -> (String -> MuType IO)
              -> Item a
              -> Compiler (Item String)
applyHastache = applyHastacheWithConfig defaultConfig

--------------------------------------------------------------------------------
loadAndApplyHastache :: MuVar a
                     => Identifier
                     -> (String -> MuType IO)
                     -> Item a
                     -> Compiler (Item String)
loadAndApplyHastache id' ctx itm = do
   tpl <- loadBody id'
   applyHastache tpl ctx itm
   

