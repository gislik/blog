--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import            Data.Typeable              (Typeable)
import            Data.Binary                (Binary)
import            Data.Maybe                 (maybeToList, fromMaybe)
import            Data.Monoid                (mappend, (<>))
import            Data.Ord                   (comparing)
import            Data.List                  (intercalate, unfoldr, sortBy)
import            Data.Time.Clock            (UTCTime (..))
import            Data.Time.Format           (formatTime, parseTime)
import            Control.Monad              (msum, filterM, (<=<), liftM, forM, filterM)
import            Control.Applicative        ((<$>), (<*>), pure)
import            System.Locale              (TimeLocale, defaultTimeLocale)
import            System.FilePath            (takeFileName, joinPath, dropFileName, takeBaseName, takeDirectory)
import qualified  Data.Set                   as S
import qualified  Data.Map                   as M
{- import qualified  Data.ByteString.Lazy.Char8 as LZ  -}
{- import            Text.Hastache  -}
{- import            Text.Hastache.Context  -}
import            Hakyll

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

   -- match "images/*" $ do
      -- route   idRoute
      -- compile copyFileCompiler

   -- match "css/*" $ do
      -- route   idRoute
      -- compile compressCssCompiler

   -- match "js/*" $ do
      -- route   idRoute
      -- compile copyFileCompiler

   match "assets/**" $ do
      route   idRoute
      compile copyFileCompiler

   match "*.markdown" $ do
      route prettyRoute
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         {- >>= relativizeUrls -}

   match blogPattern $ do
      route blogRoute
      compile $ pandocCompiler
         >>= saveSnapshot "blog-content"
         >>= loadAndApplyTemplate "templates/blog.html"    blogDetailCtx
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         {- >>= relativizeUrls -}

   {- create ["archive.html"] $ do -}
      {- route prettyRoute -}
      {- compile $ makeItem "" -}
               {- >>= loadAndApplyTemplate "templates/archive.html" archiveCtx -}
               {- >>= loadAndApplyTemplate "templates/default.html" defaultContext -}
               {- >>= relativizeUrls -}

   pages <- buildPages'
   categories <- buildCategories'
   match "index.html" $ do
      route idRoute
      compile $ do
               ident <- getUnderlying
               getResourceBody
                  >>= applyAsTemplate (indexCtx ident pages categories)
                  >>= loadAndApplyTemplate "templates/default.html" defaultContext 
                  {- >>= relativizeUrls -}

   {- tags <- buildTags' -}
   {- tagsRules tags $ \tag pattern -> do -}
      {- route idRoute -}
      {- compile $ makeItem tag -}
               {- >>= loadAndApplyTemplate "templates/blog-list.html" (tagCtx pattern) -}
               {- >>= loadAndApplyTemplate "templates/default.html" defaultContext -}
               {- >>= relativizeUrls -}

   tagsRules categories $ \category pattern -> do
      catPages <- buildCategoryPages' category pattern
      route idRoute
      compile $ do
         ident <- getUnderlying
         makeItem category
            >>= loadAndApplyTemplate "templates/blog-list.html" (indexCtx ident catPages categories)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            {- >>= relativizeUrls -}
      paginateRules catPages $ \i pattern -> do
         route idRoute
         compile $ do
            {- ident <- getUnderlying -}
            {- let catPages' = catPages { paginatePlaces = M.insert ident 1 (paginatePlaces catPages) }  -}
            makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx catPages pattern)
               >>= loadAndApplyTemplate "templates/default.html" defaultContext
               {- >>= relativizeUrls -}
   
   paginateRules pages $ \i pattern -> do
      route idRoute
      compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx pages pattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            {- >>= relativizeUrls -}

   -- Initial support for Hastache
   {- match "index.html" $ version "test" $ do -}
      {- let context "name"   = MuVariable ("Haskell" :: String) -}
          {- context "unread" = MuVariable (100 :: Int) -}
      {- route $ constRoute "test/index.html" -}
      {- compile $ getResourceBody  -}
            {- >>= loadAndApplyHastache "templates/design.htm" context  -}
            {- >>= relativizeUrls -}

   match "templates/*.html" $ compile templateCompiler

   match "templates/*.htm" $ compile hastacheCompiler

--------------------------------------------------------------------------------
blogPattern :: Pattern
blogPattern = "blog/**"

--------------------------------------------------------------------------------
blogSnapshot :: Snapshot
blogSnapshot = "blog-content"

--------------------------------------------------------------------------------
blogPerPage :: Int
blogPerPage = 2

--------------------------------------------------------------------------------
blogOrder :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
blogOrder = recentFirst

--------------------------------------------------------------------------------
indexCtx :: Identifier -> Paginate -> Tags -> Context String
indexCtx ident pages categories = let pages' = pages { paginatePlaces = M.insert ident 1 (paginatePlaces pages) } in
      {- blogListCtx "blogs" (selectBlogs =<< loadBlogs blogPattern) `mappend` -}
      blogListCtx "blogs" (blogOrder =<< loadPage 1 pages)  `mappend`
      field "tags" tags                                     `mappend`
      field "categories" cats                               `mappend`
      paginateContext pages'
      {- functionField "sum" sumFunction -}
   where
      {- selectBlogs = fmap (take 10) . blogOrder <=< excludeTag "icelandic" -}
      loadPage i = loadBlogs . fromList . fromMaybe [] .  M.lookup i . paginatePages
      tags  = const $ renderTagList =<< buildTags'
      cats  = const $ renderTagList categories
      {- cats  = const $ renderTagList =<< buildCategories' -}
      {- pages = const $ renderTagList =<< buildPages' -}
      {- sumFunction :: [String] -> Item a -> Compiler String -}
      {- sumFunction ss _ = show <$> sum <$> mapM (return . read) ss -}

--------------------------------------------------------------------------------
blogListCtx :: String -> Compiler [Item String] -> Context String
{- blogListCtx name pattern = listField name blogDetailCtx (selectAndSort =<< loadAllSnapshots pattern blogSnapshot) -}
{- blogListCtx name pattern = listField name blogDetailCtx (loadAllSnapshots pattern blogSnapshot) -}
blogListCtx name = listField name blogDetailCtx 
   {- where -}
      {- selectAndSort = fmap (take 10) . blogOrder <=< excludeTag "icelandic" -}

--------------------------------------------------------------------------------
blogDetailCtx :: Context String
blogDetailCtx = 
      dateField "date" "%B %e, %Y"         `mappend`
      mapTakeDirectory (urlField "url")    `mappend`
      defaultContext
   where
      mapTakeDirectory = mapContext dropFileName
   
--------------------------------------------------------------------------------
{- archiveCtx :: Context String -}
{- archiveCtx =  -}
      {- blogListCtx blogPattern        `mappend` -}
      {- constField "title" "Archives"  `mappend` -}
      {- defaultContext -}

--------------------------------------------------------------------------------
{- tagCtx :: Pattern -> Context String -}
{- tagCtx pattern =  -}
      {- blogListCtx "blogs" (blogOrder =<< loadBlogs pattern)   `mappend` -}
      {- field "categories" categories             `mappend` -}
      {- [> constField "title" "Tags"                 `mappend` <] -}
      {- defaultContext -}
   {- where -}
      {- categories = const $ renderTagList =<< buildCategories' -}

--------------------------------------------------------------------------------
{- categoryCtx :: Pattern -> Context String -}
{- categoryCtx pattern =  -}
      {- blogListCtx "blogs" (blogOrder =<< loadBlogs pattern)   `mappend` -}
      {- field "categories" categories             `mappend` -}
      {- constField "title" "Categories"           `mappend` -}
      {- defaultContext -}
   {- where -}
      {- categories = const $ renderTagList =<< buildCategories' -}

--------------------------------------------------------------------------------
pageCtx :: Paginate -> Pattern -> Context String
pageCtx pages pattern = 
      blogListCtx "blogs" (blogOrder =<< loadBlogs pattern) `mappend`
      field "categories" categories                         `mappend`
      constField "title" "Pagination"                       `mappend`
      paginateContext pages                                 `mappend`
      defaultContext
   where
      categories = const $ renderTagList =<< buildCategories'

--------------------------------------------------------------------------------
buildTags' :: MonadMetadata m => m Tags
buildTags' = buildTags blogPattern (fromCapture "tag/*/index.html")

--------------------------------------------------------------------------------
buildCategories' :: MonadMetadata m => m Tags
buildCategories' = do
   categories <- buildCategories blogPattern (fromCapture "*/index.html") 
   catMap <- forM (tagsMap categories) $ \(category, identifiers) -> do
      filteredIdentifiers <- excludeTag' "icelandic" identifiers
      return (category, filteredIdentifiers)
   return $ categories { tagsMap = filter (not . null . snd) catMap }
   
categoryMakeId :: String -> Int -> Identifier
categoryMakeId category = fromCapture (fromGlob (category <> "/*/index.html")) . show

--------------------------------------------------------------------------------
buildPaginateWith' :: MonadMetadata m
                  => Int
                  -> (PageNumber -> Identifier)
                  -> Pattern
                  {- -> (Identifier -> Identifier -> Ordering) -}
                  -> ([Identifier] -> m [Identifier])
                  {- -> [Identifier] -}
                  -> m Paginate
{- buildPaginateWith' n makeId idents = do -}
buildPaginateWith' n makeId pattern cmp = do
    idents <- cmp =<< getMatches pattern
    let pages          = flip unfoldr idents $ \xs ->
            if null xs then Nothing else Just (splitAt n xs)
        nPages         = length pages
        paginatePages' = zip [1..] pages
        pagPlaces'     =
            [(ident, idx) | (idx,ids) <- paginatePages', ident <- ids] ++
            [(makeId i, i) | i <- [1 .. nPages]]
        {- pattern        = fromList idents -}

    return $ Paginate (M.fromList paginatePages') (M.fromList pagPlaces') makeId
        (PatternDependency pattern (S.fromList idents))

{- buildPages' :: (MonadMetadata m, Functor m) => m Tags -}
{- buildPages' = buildTagsWith getPages' blogPattern (fromCapture "page/*/index.html") -}

buildPages' :: (MonadMetadata m, Functor m) => m Paginate
{- buildPages' = buildPaginate blogPattern -}
buildPages' = buildPaginateWith' blogPerPage (fromCapture "*/index.html" . show) blogPattern matchAndOrder
   where
      matchAndOrder = identRecentFirst <=< excludeTag' "icelandic" 

buildCategoryPages' :: (MonadMetadata m, Functor m) => String -> Pattern -> m Paginate
buildCategoryPages' name pattern = 
   buildPaginateWith' blogPerPage (categoryMakeId name) pattern (identRecentFirst <=< excludeTag' "icelandic")
--------------------------------------------------------------------------------
{- sorter = mapM (return.itemIdentifier) <=< blogOrder <=< mapM (\id' -> return $ Item id' (""::String))  -}
{- sortByM :: (Monad m, Ord a) => (a -> a -> Ordering) -> [a] -> m [a] -}
{- sortByM cmp = undefined -}

{- comparingM :: Ord a => (b -> m a) -> b -> b -> m Ordering -}
{- comparingM = undefined -}

compare' :: Ord a => a -> a -> Ordering
compare' = flip compare

comparingBy :: Ord a => (b -> a) -> (a -> a -> Ordering) -> b -> b -> Ordering
comparingBy f cmp x y = cmp (f x) (f y)

sortIdentifierBy :: (MonadMetadata m, Ord a) => (a -> a -> Ordering) -> (Identifier -> m a) -> [Identifier] -> m [Identifier]
{- sortIdentifierBy cmp f ids = liftM (map fst . sortBy (comparing snd))  mapM (\x -> (,) <$> pure x <*> f x) ids -}
sortIdentifierBy cmp f ids = liftM (map fst . sortBy (comparingBy snd cmp)) $ mapM (\x -> liftM (x,) (f x)) ids

identChronolgical :: (MonadMetadata m) => [Identifier] -> m [Identifier]
identChronolgical = sortIdentifierBy compare (getItemUTC defaultTimeLocale)

identRecentFirst :: (MonadMetadata m, Functor m) => [Identifier] -> m [Identifier]
identRecentFirst = fmap reverse . identChronolgical
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
{- blogList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String -}
{- blogList pattern sortFilter = do -}
   {- blog    <- sortFilter =<< loadAllSnapshots pattern "blog-content" -}
   {- itemTpl <- loadBody "templates/post-item.html" -}
   {- applyTemplateList itemTpl blogDetailCtx blog -}

loadBlogs :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadBlogs = flip loadAllSnapshots blogSnapshot

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
{- filterTags :: MonadMetadata m => ([String] -> m Bool) -> [Item String] -> m [Item String] -}
{- filterTags p = filterM $ p <=< getTags . itemIdentifier  -}

filterTags' :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTags' p = filterM $ p <=< getTags 
--------------------------------------------------------------------------------
{- excludeTag :: MonadMetadata m => String -> [Item String] -> m [Item String] -}
{- excludeTag tag = filterTags (return . notElem tag) -}

excludeTag' :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
excludeTag' tag = filterTags' (return . notElem tag)
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
{- getPages :: MonadMetadata m => Pattern  -}
                            {- -> ([Identifier] -> m [Identifier])  -}
                            {- -> Int  -}
                            {- -> Identifier  -}
                            {- -> m [String] -}
{- getPages pattern sorter k id' = do -}
   {- ids <- sorter =<< getMatches pattern -}
   {- let m = M.fromList $ concat $ zipWith idx [1..] (partitionAll k ids) -}
   {- return . maybeToList $ show <$> M.lookup id' m -}

   {- where -}
      {- idx :: Int -> [a] -> [(a, Int)] -}
      {- idx i = map (\x -> (x, i)) -}


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
-- | Obtain categories from a page.
{- getCategory :: MonadMetadata m => Identifier -> m [String] -}
{- getCategory = return . return . takeBaseName . takeDirectory . toFilePath -}



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type MuTemplate = String

hastacheCompiler :: Compiler (Item MuTemplate)
hastacheCompiler = cached "Hakyll.Web.Mu.hastacheCompiler" getResourceString

--------------------------------------------------------------------------------
{- applyHastacheWithConfig :: MuVar a -}
                        {- => MuConfig IO  -}
                        {- -> MuTemplate  -}
                        {- -> (String -> MuType IO) -}
                        {- -> Item a  -}
                        {- -> Compiler (Item String) -}
{- applyHastacheWithConfig cfg tpl ctx itm = do -}
   {- let ctx' "body" = MuVariable (itemBody itm) -}
       {- ctx' x      = ctx x -}
   {- content <- liftM LZ.unpack $ unsafeCompiler (hastacheStr cfg (encodeStr tpl) (mkStrContext ctx')) -}
   {- return $ itemSetBody content itm -}

--------------------------------------------------------------------------------
{- applyHastache :: MuVar a -}
              {- => MuTemplate -}
              {- -> (String -> MuType IO) -}
              {- -> Item a -}
              {- -> Compiler (Item String) -}
{- applyHastache = applyHastacheWithConfig defaultConfig -}

--------------------------------------------------------------------------------
{- loadAndApplyHastache :: MuVar a -}
                     {- => Identifier -}
                     {- -> (String -> MuType IO) -}
                     {- -> Item a -}
                     {- -> Compiler (Item String) -}
{- loadAndApplyHastache id' ctx itm = do -}
   {- tpl <- loadBody id' -}
   {- applyHastache tpl ctx itm -}
   

