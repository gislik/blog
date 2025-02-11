{-# LANGUAGE OverloadedStrings #-}
import            Data.Maybe                      (fromMaybe, listToMaybe)
import            Data.Either                     (fromRight)
import            Data.Monoid                     ((<>), mconcat)
import            Data.Functor                    ((<$>), fmap, (<&>))
import            Data.Char                       (isSpace, toLower, toUpper)
import            Data.List                       (intercalate, intersperse, foldl', isPrefixOf, isSuffixOf)
import            Data.Text                       (pack)
import            Data.Time.Clock                 (UTCTime(..))
import            Control.Applicative             ((<|>), Alternative(..))
import            Control.Monad                   (msum, filterM, (<=<), liftM, filterM)
import            Control.Monad.Fail              (MonadFail)
import            System.Environment              (getArgs)
import            Data.Time.Format                (TimeLocale, defaultTimeLocale, parseTimeM, formatTime)
import            Text.Blaze.Html                 (toHtml, toValue, (!))
import            Text.Blaze.Html5.Attributes     (href, class_)
import            Text.Blaze.Html.Renderer.String (renderHtml)
import            Text.HTML.TagSoup               (Tag(..))
import qualified  Data.Map                        as M
import qualified  Text.Blaze.Html5                as H
import            System.FilePath
import            Text.Pandoc.Options
import            Text.Pandoc.Templates
import            Text.Pandoc.Class
import            Hakyll

--------------------------------------------------------------------------------
-- SITE
--------------------------------------------------------------------------------
main :: IO ()
main = do
   isWatching <- fmap (== "watch") . listToMaybe <$> getArgs
   let allPattern =
         case isWatching of
            Just True -> blogPattern .||. draftPattern
            _         -> blogPattern

   hakyllWith config $ do
      excludePattern <- fmap fromList $ includeTagM "icelandic" <=< getMatches $ blogPattern
      let visiblePattern =
            allPattern .&&. complement excludePattern

      pages      <- buildPages visiblePattern (fromCapture "*/index.html" . show)
      categories <- buildCategories visiblePattern (fromCapture "*/index.html")
      tags       <- buildTags visiblePattern (fromCapture "tags/*/index.html")

      -- static pages
      match "*.md" $ do
         route pageRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page-detail.html" defaultCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

      -- index
      create ["index.html"] $ do
         route idRoute
         compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 pages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- blogs
      match allPattern $ do
         route blogRoute
         compile $ blogCompiler
            >>= saveSnapshot blogSnapshot
            >>= loadAndApplyTemplate "templates/blog-detail.html" (blogDetailCtx categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- blog pages
      paginateRules pages $ \i _ -> do
         route idRoute
         compile $ makeItem (show i)
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i pages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- blog category index
      tagsRules categories $ \category pattern -> do
         catPages <- buildPages pattern (\i -> fromCaptures "*/*/index.html" [category, show i])
         route idRoute
         compile $ makeItem category
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 catPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls
         paginateRules catPages $ \i _ -> do -- blog category pages
            route idRoute
            compile $ makeItem category
               >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i catPages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx
               >>= indexCompiler
               >>= relativizeUrls

      -- blog tags index 
      tagsRules tags $ \tag pattern -> do
         tagPages <- buildPages pattern (\i -> fromCaptures "tags/*/*/index.html" [tag, show i])
         route idRoute
         compile $ makeItem tag
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 tagPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls
         paginateRules tagPages $ \i _ -> do -- blog tags pages
            route idRoute
            compile $
               makeItem tag
               >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i tagPages categories tags)
               >>= loadAndApplyTemplate "templates/default.html" defaultCtx
               >>= indexCompiler
               >>= relativizeUrls

      -- decks
      match "decks/*.md" $ do
         route decksRoute
         compile $ blogCompiler
            >>= saveSnapshot decksSnapshot
            >>= loadAndApplyTemplate "templates/decks-detail.html" decksDetailCtx
            >>= indexCompiler
            >>= relativizeUrls

      match "decks/**" $ do
         route decksAssetsRoute
         compile copyFileCompiler

      create ["decks/index.html"] $ do
         route idRoute
         compile $ makeItem "decks"
            >>= loadAndApplyTemplate "templates/decks-list.html" decksCtx
            >>= loadAndApplyTemplate "templates/default.html" decksCtx
            >>= indexCompiler
            >>= relativizeUrls

      -- atom
      create ["atom.xml"] $ do
         route idRoute
         compile $ renderBlogAtom <=< fmap (take 20) . loadBlogs $ visiblePattern

      -- static content
      match "static/**" $ do
         route rootRoute
         compile copyFileCompiler

      -- static css
      match "css/**.css" $ do
         route idRoute
         compile compressCssCompiler

      match ("css/**.scss" .&&. complement "css/**_*.scss") $ do
         route   $ setExtension "css"
         compile $ sassCompiler <&> fmap compressCss

      match "css/webfonts/*" $ do
         route idRoute
         compile copyFileCompiler

      create [".nojekyll"] $ do -- disable GitHub's Jekyll
         route idRoute
         compile copyFileCompiler

      -- templates
      match "templates/*.html" $
         compile templateCompiler

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

blogTitle :: String
blogTitle = "Crypto and Code"

blogDescription :: String
blogDescription = "My thoughts on blockchains and software"

blogAuthor :: String
blogAuthor = "Gísli Kristjánsson"

blogAuthorEmail :: String
blogAuthorEmail = "gislik@hamstur.is"

blogRoot :: String
blogRoot = "https://gisli.hamstur.is"

decksSnapshot :: Snapshot
decksSnapshot = "decks-content"

feedConfiguration :: FeedConfiguration
feedConfiguration =
   FeedConfiguration
      { feedTitle       = blogTitle
      , feedDescription = blogDescription
      , feedAuthorName  = blogAuthor
      , feedAuthorEmail = blogAuthorEmail
      , feedRoot        = blogRoot
      }

blogReaderOptions :: ReaderOptions
blogReaderOptions =
   defaultHakyllReaderOptions
      {
         readerExtensions =
            readerExtensions defaultHakyllReaderOptions <> extensionsFromList
               [
                 Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
               , Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
               , Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
               , Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
               , Ext_inline_code_attributes     -- Ext_inline_code_attributes
               , Ext_abbreviations              -- PHP markdown extra abbreviation definitions
               ]
      }

-- blogWriterOptions configures pandoc to include a table of contents
-- and uses MathJax to render math.
blogWriterOptions :: WriterOptions
blogWriterOptions =
   defaultHakyllWriterOptions
      {
        writerHTMLMathMethod = MathJax ""
      , writerTableOfContents = True
      , writerNumberSections  = True
      , writerTOCDepth        = 2
      , writerTemplate        =
         let
            toc = "$toc$" :: String
            body = "$body$" :: String
            html = pack . renderHtml $ do
                     H.div ! class_ "toc" $
                        toHtml toc
                     toHtml body
            template  =  fromRight mempty <$> compileTemplate "" html
            runPureWithDefaultPartials = runPure . runWithDefaultPartials
            eitherToMaybe = either (const Nothing) Just
         in
            eitherToMaybe (runPureWithDefaultPartials template)
      }

config :: Configuration
config =
   defaultConfiguration {
      ignoreFile = ignoreFile'
   }
   where
     ignoreFile' path
         | "#"    `isPrefixOf` fileName = True
         | "~"    `isSuffixOf` fileName = True
         | ".swp" `isSuffixOf` fileName = True
         | otherwise                    = False
      where
         fileName = takeFileName path

--------------------------------------------------------------------------------
-- CONTEXTS
--------------------------------------------------------------------------------
pageTitleField :: String -> Context String
pageTitleField key =
   aliasContext alias metadataField <> -- use page title from metadata
   pathTitleField key               <> -- or read from the path
   constField key "Crypto and Code"    -- alternatively use this
   where
      alias x | x == key = "title"
      alias x            = x

defaultCtx :: Context String
defaultCtx =
   bodyField "page.body"                          <>
   pageTitleField "page.title"                    <>
   constField "page.description" blogDescription  <>
   constField "page.root" blogRoot                <>
   urlField' "page.url"                           <>
   pathField "page.path"                          <>
   polishField "polish"                           <>
   metadataField

blogCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
blogCtx i pages categories tags =
   listField "blogs" (blogDetailCtx categories tags) (loadBlogs pattern) <>
   categoryListField "categories" categories                             <>
   tagsListField "tags" tags                                             <>
   pagesField i                                                          <>
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
   pageTitleField "blog.title"                        <>
   dateField "blog.date" "%B %e, %Y"                  <>
   urlField' "blog.url"                               <>
   categoryField' "blog.category" categories          <>
   tagsField' "blog.tags" tags                        <>
   field "blog.next.url" nextBlog                     <>
   field "blog.previous.url" previousBlog             <>
   summaryField "blog.summary"                        <>
   readingTimeField "blog.reading.time" blogSnapshot  <>
   defaultCtx

decksCtx :: Context String
decksCtx =
   listField "decks" decksDetailCtx (loadDecks "decks/*.md") <>
   defaultCtx

decksDetailCtx :: Context String
decksDetailCtx =
   dateField "date" "%B %e, %Y" <>
   urlField' "url"              <>
   defaultCtx                   <>
   constField "theme" "black"

atomCtx :: Context String
atomCtx =
   mapContext cdata (pageTitleField "title")   <>
   aliasContext alias metadataField        <>  -- description from metadata
   teaserField "description" blogSnapshot  <>  -- teaser is description
   previewField "description" blogSnapshot <>  -- first paragraph is description
   urlField' "url"
   where
      alias "description" = "summary"
      alias x             = x
      cdata s | "<![CDATA[" `isPrefixOf` s = s
      cdata s                              = "<![CDATA[" <> s <> "]]>"

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------
-- compilers
blogCompiler :: Compiler (Item String)
blogCompiler = do
   ident <- getUnderlying
   toc   <- getMetadataField ident "withtoc"
   pandocCompilerWith blogReaderOptions (maybe defaultOptions blogOptions toc)
   where
      defaultOptions = defaultHakyllWriterOptions
      blogOptions = const blogWriterOptions

indexCompiler :: Item String -> Compiler (Item String)
indexCompiler = withItemBody (return . withTags dropIndex)
   where
      dropIndex (TagOpen "a" attrs) = TagOpen "a" (dropIndex' <$> attrs)
      dropIndex tag                 = tag
      dropIndex' ("href", url) | not (isExternal url) = ("href", dropFileName url <> takeHash url)
      dropIndex' z                                    = z
      takeHash = dropWhile (/= '#')

sassCompiler :: Compiler (Item String)
sassCompiler = do
   ident <- getUnderlying
   output <- unixFilter "sass" [toFilePath ident] ""
   makeItem output

loadBlogs :: Pattern -> Compiler [Item String]
loadBlogs =
   recentFirst <=< flip loadAllSnapshots blogSnapshot

nextBlog :: Item String -> Compiler String
nextBlog blog = do
   blogs <- loadBlogs blogPattern
   let idents = map itemIdentifier blogs
       ident = itemAfter idents (itemIdentifier blog)
   maybe empty (fmap (maybe empty toUrl) . getRoute) ident
   where
      itemAfter xs x =
         lookup x $ zip xs (tail xs)

previousBlog :: Item String -> Compiler String
previousBlog blog = do
   blogs <- loadBlogs blogPattern
   let idents = map itemIdentifier blogs
       ident = itemBefore idents (itemIdentifier blog)
   maybe empty (fmap (maybe empty toUrl) . getRoute) ident
   where
      itemBefore xs x =
         lookup x $ zip (tail xs) xs

loadDecks :: Pattern -> Compiler [Item String]
loadDecks =
   recentFirst <=< flip loadAllSnapshots decksSnapshot

renderBlogAtom :: [Item String] -> Compiler (Item String)
renderBlogAtom =
   renderAtom feedConfiguration atomCtx

-- routes
rootRoute :: Routes
rootRoute =
   customRoute (joinPath . dropDirectory . splitPath . toFilePath)
   where
      dropDirectory []       = []
      dropDirectory ("/":ds) = dropDirectory ds
      dropDirectory ds       = tail ds

pageRoute :: Routes
pageRoute =
   removeExtension `composeRoutes` addIndex
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

decksRoute :: Routes
decksRoute =
   blogRoute `composeRoutes` prefixRoute "decks"
   where
      prefixRoute prefix = customRoute $ (prefix </>) . toFilePath

decksAssetsRoute :: Routes
decksAssetsRoute =
   yearRoute  `composeRoutes`
   monthRoute `composeRoutes`
   dropDayRoute
   where
      yearRoute = gsubRoute "[[:digit:]]{4}-" (\xs -> take 4 xs <> "/")
      monthRoute = gsubRoute "/[[:digit:]]{2}-" (\xs -> "/" <> (take 2 . drop 1) xs <> "/")
      dropDayRoute = gsubRoute "/[[:digit:]]{2}-" (const "/")

-- contexts
pathTitleField :: String -> Context String
pathTitleField =
   flip field title
   where
      title = maybe empty (emptyTitle . pageTitle) <=< getRoute . itemIdentifier
      pageTitle = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName
      emptyTitle "." = empty
      emptyTitle x = return x
      capitalize []     = []
      capitalize (x:xs) = toUpper x : map toLower xs

urlField' :: String -> Context String
urlField' =
  mapContext dropFileName . urlField

categoryField' :: String -> Tags -> Context a
categoryField' =
   tagsFieldWith getCategory (renderLink "@") mconcat

categoryListField :: String -> Tags -> Context a
categoryListField key tags =
   field key (const $ renderList tags)
   where
      renderList = renderTags makeLink unwords
      makeLink tag url _ _ _ = renderHtml $ do
         "@"
         H.a ! href (toValue url) $ toHtml tag

tagsField' :: String -> Tags -> Context a
tagsField' =
   tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

tagsListField :: String -> Tags -> Context a
tagsListField key tags =
   field key (const $ renderList tags)
   where
      renderList = renderTags makeLink unwords
      makeLink tag url _ _ _ = renderHtml $ do
         "#"
         H.a ! href (toValue url) $ toHtml tag

summaryField :: String -> Context String
summaryField key =
   field key meta                 <>  -- summary from metadata
   teaserField key blogSnapshot   <>  -- teaser is summary
   previewField key blogSnapshot      -- first paragraph is summary
   where
      meta :: Item a -> Compiler String
      meta item = do
         summary <- getMetadataField' (itemIdentifier item) "summary"
         return . renderHtml $
            H.p (toHtml summary)
      alias x | x == key  = "summary"
      alias x             = x

previewField :: String -> Snapshot -> Context String
previewField key snapshot  =
   field key trim'
   where
      trim' item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList firstParagraph body
      firstParagraph = map fst . takeWhile (\(_, s) -> s > 0) . acc 0 . map cnt
      acc _ []           = []
      acc s ((x, s'):xs) = (x, s + s') : acc  (s + s') xs
      cnt tag@(TagOpen "p" _) = (tag, 1)
      cnt tag@(TagClose "p")  = (tag, -1)
      cnt tag                 = (tag, 0)

readingTimeField :: String -> Snapshot -> Context String
readingTimeField key snapshot =
   field key calculate
   where
      calculate item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList acc body
      acc ts = [TagText (show (time ts))]
      time ts = foldl' count 0  ts `div` 265
      count n (TagText s) = n + length (words s)
      count n _           = n

-- aliasContext maps a new key to another key. If the other key
-- is not defined or returns empty the alias returns empty.
aliasContext :: (String -> String) -> Context a -> Context a
aliasContext f (Context c) =
   Context $ \k a i -> c (f k) a i <|> c' k
   where
      c' k = noResult $ unwords ["Tried to alias", k, "as", f k, "which doesn't exist"]

polishField :: String -> Context String
polishField name =
   functionField name $ \args _ ->
      return $ withTags text' (unwords args)
   where
      text' (TagText s) = TagText (concatMap f (split isSpace s))
      text' t           = t
      f ""                   = ""
      f ":+1:"               = "👍"
      f ":coffee:"           = "☕️"
      f ":disappointed:"     = "😞"
      f ":frowning:"         = "😦"
      f ":grinning:"         = "😀"
      f ":heart:"            = "❤"
      f ":ramen:"            = "🍜"
      f ":rice_ball:"        = "🍙"
      f ":smile:"            = "😄"
      f ":sushi:"            = "🍣"
      f ":stuck_out_tongue:" = "😛"
      f ":thumbsup:"         = "👍"
      f ":tada:"             = "🎉"
      f x                    = x

-- metadata
includeTagM :: MonadMetadata m => String -> [Identifier] -> m [Identifier]
includeTagM tag =
   filterTagsM (return . elem tag)

filterTagsM :: MonadMetadata m => ([String] -> m Bool) -> [Identifier] -> m [Identifier]
filterTagsM p =
   filterM (p <=< getTags)

-- pagination
buildPages :: (MonadMetadata m, MonadFail m) => Pattern -> (PageNumber -> Identifier) -> m Paginate
buildPages = buildPaginateWith
      (return . paginateEvery blogPerPage <=< sortRecentFirst)

-- html
renderLink :: String -> String -> Maybe FilePath -> Maybe H.Html
renderLink _ _   Nothing       = Nothing
renderLink pre text (Just url) =
   Just $ do
      toHtml pre
      H.a ! href (toValue $ toUrl url) $ toHtml text

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

-- misc
split :: (Char -> Bool) -> String -> [String]
split p' s =
   go p' ("", s)
   where
      go _ ("", "") = []
      go p ("", y) = go (not . p) (break p y)
      go p (x, y) = x : go (not . p) (break p y)


