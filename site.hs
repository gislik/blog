--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import            Data.Typeable              (Typeable)
import            Data.Binary                (Binary)
import            Data.Maybe                 (fromMaybe)
import            Data.Monoid                ((<>), mconcat)
import            Data.List                  (intercalate, unfoldr, sortBy)
import            Data.Time.Clock            (UTCTime (..))
import            Data.Time.Format           (formatTime, parseTime)
import            Control.Monad              (msum, filterM, (<=<), liftM, forM, filterM)
import            System.Locale              (TimeLocale, defaultTimeLocale)
import            System.FilePath            (takeFileName, joinPath, dropFileName)
import            Text.Printf                (printf)
import qualified  Data.Set                   as S
import qualified  Data.Map                   as M
{- import            Text.Hastache  -}
{- import            Text.Hastache.Context  -}
import            Hakyll

--------------------------------------------------------------------------------

{-
TODO: 
   1. Links for numbered pagination
   2. Selected category highlighted
   3. Selected page highlighted
   4. Remove index.html from categories
   5. Remove comma from category selector
   6. Tags
   7. Fork Hakyll and add supporting code?
-}

main :: IO ()
main = hakyll $ do

   match "assets/**" $ do
      route   idRoute
      compile copyFileCompiler

   match "*.markdown" $ do
      route prettyRoute
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext

   match blogPattern $ do
      route blogRoute
      compile $ pandocCompiler
         >>= saveSnapshot "blog-content"
         >>= loadAndApplyTemplate "templates/blog.html"    blogDetailCtx
         >>= loadAndApplyTemplate "templates/default.html" defaultContext

   -- pages
   pages <- buildPages'
   match "index.html" $ do
      route idRoute
      compile $ do
               ident <- getUnderlying
               getResourceBody
                  >>= applyAsTemplate (indexCtx ident pages)
                  >>= loadAndApplyTemplate "templates/default.html" defaultContext 

   paginateRules pages $ \i pattern -> do
      route idRoute
      compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i pages pattern)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

   -- categories
   categories <- buildCategories'
   tagsRules categories $ \category pattern -> do
      catPages <- buildCategoryPages' category pattern
      route idRoute
      compile $ do
         ident <- getUnderlying
         makeItem category
            >>= loadAndApplyTemplate "templates/blog-list.html" (indexCtx ident catPages)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

      paginateRules catPages $ \i catPattern -> do
         route idRoute
         compile $ do
            makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (pageCtx i catPages catPattern)
               >>= loadAndApplyTemplate "templates/default.html" defaultContext
   
   -- tags
   {- tags <- buildTags' -}
   {- tagsRules tags $ \tag pattern -> do -}
      {- route idRoute -}
      {- compile $ makeItem tag -}
               {- >>= loadAndApplyTemplate "templates/blog-list.html" (tagCtx pattern) -}
               {- >>= loadAndApplyTemplate "templates/default.html" defaultContext -}

   match "templates/*.html" $ compile templateCompiler

   -- Initial support for Hastache
   {- match "index.html" $ version "test" $ do -}
      {- let context "name"   = MuVariable ("Haskell" :: String) -}
          {- context "unread" = MuVariable (100 :: Int) -}
      {- route $ constRoute "test/index.html" -}
      {- compile $ getResourceBody  -}
            {- >>= loadAndApplyHastache "templates/design.htm" context  -}
            {- >>= relativizeUrls -}

   {- match "templates/*.htm" $ compile hastacheCompiler -}

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
blogListField :: String -> Compiler [Item String] -> Context String
blogListField name = listField name blogDetailCtx 

--------------------------------------------------------------------------------
indexCtx :: Identifier -> Paginate -> Context String
indexCtx ident pages = 
      blogListField "blogs" (blogOrder =<< loadPage 1 pages)    <>
      field "tags" tags                                         <>
      field "categories" cats                                   <>
      paginateContext pages'                                    <>
      paginatorContext pages' 1
   where
      loadPage i = loadBlogs . fromList . fromMaybe [] .  M.lookup i . paginatePages
      pages' = pages { paginatePlaces = M.insert ident 1 (paginatePlaces pages) }
      tags  = const $ renderTagList =<< buildTags'
      cats  = const $ renderTagList =<< buildCategories'

--------------------------------------------------------------------------------
blogDetailCtx :: Context String
blogDetailCtx = 
      dateField "date" "%B %e, %Y"         <>
      mapTakeDirectory (urlField "url")    <>
      defaultContext
   where
      mapTakeDirectory = mapContext dropFileName
   
--------------------------------------------------------------------------------
pageCtx :: PageNumber -> Paginate -> Pattern -> Context String
pageCtx i pages pattern = 
      blogListField "blogs" (blogOrder =<< loadBlogs pattern)   <>
      field "categories" categories                             <>
      constField "title" "Pagination"                           <>
      paginateContext pages                                     <>
      paginatorContext pages i                                  <>
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
   
--------------------------------------------------------------------------------
buildPaginateWith' :: MonadMetadata m
                  => Int
                  -> (PageNumber -> Identifier)
                  -> Pattern
                  -> ([Identifier] -> m [Identifier])
                  -> m Paginate
buildPaginateWith' n makeId pattern cmp = do
    idents <- cmp =<< getMatches pattern
    let pages          = flip unfoldr idents $ \xs ->
            if null xs then Nothing else Just (splitAt n xs)
        nPages         = length pages
        paginatePages' = zip [1..] pages
        pagPlaces'     =
            [(ident, idx) | (idx,ids) <- paginatePages', ident <- ids] ++
            [(makeId i, i) | i <- [1 .. nPages]]

    return $ Paginate (M.fromList paginatePages') (M.fromList pagPlaces') makeId
        (PatternDependency pattern (S.fromList idents))

buildPages' :: (MonadMetadata m, Functor m) => m Paginate
buildPages' = buildPaginateWith' blogPerPage (fromCapture "*/index.html" . show) blogPattern matchAndOrder
   where
      matchAndOrder = identRecentFirst <=< excludeTag' "icelandic" 

buildCategoryPages' :: (MonadMetadata m, Functor m) => String -> Pattern -> m Paginate
buildCategoryPages' name pattern = 
   buildPaginateWith' blogPerPage (categoryMakeId name) pattern (identRecentFirst <=< excludeTag' "icelandic")
   where
      categoryMakeId category = fromCapture (fromGlob (category <> "/*/index.html")) . show


--------------------------------------------------------------------------------
{- sorter = mapM (return.itemIdentifier) <=< blogOrder <=< mapM (\id' -> return $ Item id' (""::String))  -}
{- sortByM :: (Monad m, Ord a) => (a -> a -> Ordering) -> [a] -> m [a] -}
{- sortByM cmp = undefined -}

{- comparingM :: Ord a => (b -> m a) -> b -> b -> m Ordering -}
{- comparingM = undefined -}

{- compare' :: Ord a => a -> a -> Ordering -}
{- compare' = flip compare -}

comparingBy :: Ord a => (b -> a) -> (a -> a -> Ordering) -> b -> b -> Ordering
comparingBy f cmp x y = cmp (f x) (f y)

sortIdentifierBy :: (MonadMetadata m, Ord a) => (a -> a -> Ordering) -> (Identifier -> m a) -> [Identifier] -> m [Identifier]
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
{- partitionAll :: Int -> [a] -> [[a]] -}
{- partitionAll n xs = splitter (splitAt n xs) -}
   {- where  -}
      {- splitter :: ([a], [a]) -> [[a]] -}
      {- splitter (as,[]) = [as] -}
      {- splitter (as,bs) = as : partitionAll n bs -}

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
-- | Takes first, current, last page and produces index of next page
type RelPage = PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber

--------------------------------------------------------------------------------
paginateField :: Paginate -> String -> RelPage -> Context a
paginateField pag fieldName relPage = field fieldName $ \item ->
    let identifier = itemIdentifier item
    in case M.lookup identifier (paginatePlaces pag) of
        Nothing -> fail $ printf
            "Hakyll.Web.Paginate: there is no page %s in paginator map."
            (show identifier)
        Just pos -> case relPage 1 pos nPages of
            Nothing   -> fail "Hakyll.Web.Paginate: No page here."
            Just pos' -> do
                let nextId = paginateMakeId pag pos'
                mroute <- getRoute nextId
                case mroute of
                    Nothing -> fail $ printf
                        "Hakyll.Web.Paginate: unable to get route for %s."
                        (show nextId)
                    Just rt -> return $ toUrl rt
  where
    nPages = M.size (paginatePages pag)

paginatorContext :: Paginate -> PageNumber -> Context a
paginatorContext pages i = 
   mconcat  [ paginateField pages "page1" (\f _ l -> betweenPages f l j1) 
            , paginateField pages "page2" (\f _ l -> betweenPages f l j2)
            , paginateField pages "page3" (\f _ l -> betweenPages f l j3) 
            , paginateField pages "page4" (\f _ l -> betweenPages f l j4) 
            , paginateField pages "page5" (\f _ l -> betweenPages f l j5) 
            , constField "page1n" (show j1)
            , constField "page2n" (show j2)
            , constField "page3n" (show j3)
            , constField "page4n" (show j4)
            , constField "page5n" (show j5)
            , constField  ("page" <> show i <> "a") "active"
            ]
   where
      (j1:j2:j3:j4:j5:_) = iterate (+1) ((i - 1) `div` 5 * 5 + 1)
   

between :: Ord a => a -> a -> a -> Bool
between l h x | x < l || x > h = False
between _ _ _                  = True

betweenPages :: PageNumber -> PageNumber -> PageNumber -> Maybe PageNumber
betweenPages l h x = if between l h x then Just x else Nothing
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

{- type MuTemplate = String -}

{- hastacheCompiler :: Compiler (Item MuTemplate) -}
{- hastacheCompiler = cached "Hakyll.Web.Mu.hastacheCompiler" getResourceString -}

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
   

