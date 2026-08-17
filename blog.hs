{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (filterM, liftM, (<=<))
import Data.ByteString.Lazy (toStrict)
import Data.Char (isSpace, toLower, toUpper)
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (TimeLocale, defaultTimeLocale, formatTime, parseTimeM)
import Hakyll hiding (buildCategories, categoryField, getCategory, tagsField, urlField)
import qualified Hakyll.Web.Template.Context as C
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, dropFileName, joinPath, splitDirectories, splitPath, takeBaseName, takeDirectory, takeFileName, (</>))
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (class_, href)
import Text.HTML.TagSoup (Tag (..))
import Text.Pandoc (Block (..), Pandoc (..))
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Options (Extension (..), HTMLMathMethod (..), ReaderOptions (..), WriterOptions (..), extensionsFromList)
import Text.Pandoc.Templates (WithDefaultPartials (..), compileTemplate)
import Text.ParserCombinators.ReadP (between, char, eof, many1, munch1, readP_to_S, string, (<++))

--------------------------------------------------------------------------------
-- CONFIGURATION
--------------------------------------------------------------------------------
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

blogPattern :: Pattern
blogPattern = "blog/**.md" .||. "blog/**.html" -- legacy blog posts

draftPattern :: Pattern
draftPattern = "drafts/**.md"

assetPattern :: Pattern
assetPattern = ("blog/**" .||. "drafts/**") .&&. complement (blogPattern .||. draftPattern)

blogSnapshot :: Snapshot
blogSnapshot = "blog-content"

blogPerPage :: Int
blogPerPage = 4

blogConfig :: Configuration
blogConfig =
  defaultConfiguration
    { ignoreFile = isIgnoredFile,
      watchIgnore = isWatchIgnoreFile
    }
  where
    isIgnoredFile path
      | "#" `isPrefixOf` fileName = True
      | "~" `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path

    isWatchIgnoreFile "blog.hs" = True
    isWatchIgnoreFile _ = False

blogReaderOptions :: ReaderOptions
blogReaderOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        readerExtensions defaultHakyllReaderOptions
          <> extensionsFromList
            [ Ext_tex_math_single_backslash, -- TeX math btw (..) [..]
              Ext_tex_math_double_backslash, -- TeX math btw \(..\) \[..\]
              Ext_tex_math_dollars, -- TeX math between $..$ or $$..$$
              Ext_latex_macros, -- Parse LaTeX macro definitions (for math only)
              Ext_inline_code_attributes, -- Ext_inline_code_attributes
              Ext_abbreviations -- PHP markdown extra abbreviation definitions
            ]
    }

-- blogWriterOptions configures pandoc to include a table of contents
-- and uses MathJax to render math.
blogWriterOptions :: WriterOptions
blogWriterOptions =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax "",
      writerTableOfContents = True,
      writerNumberSections = True,
      writerTOCDepth = 2,
      writerTemplate =
        let toc = "$toc$" :: String
            body = "$body$" :: String
            html = pack . renderHtml $ do
              H.div ! class_ "toc" $
                toHtml toc
              toHtml body
            template = fromRight mempty <$> compileTemplate "" html
            runPureWithDefaultPartials = runPure . runWithDefaultPartials
            eitherToMaybe = either (const Nothing) Just
         in eitherToMaybe (runPureWithDefaultPartials template)
    }

decksSnapshot :: Snapshot
decksSnapshot = "decks-content"

feedLength :: Int
feedLength = 20

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = blogTitle,
      feedDescription = blogDescription,
      feedAuthorName = blogAuthor,
      feedAuthorEmail = blogAuthorEmail,
      feedRoot = blogRoot
    }

--------------------------------------------------------------------------------
-- SITE
--------------------------------------------------------------------------------
main :: IO ()
main = do
  isDrafts <- isMaybeTrue <$> lookupEnv "DRAFTS"
  let includePattern =
        if isDrafts
          then blogPattern .||. draftPattern
          else blogPattern

  hakyllWith blogConfig $ do
    excludePattern <- fromList <$> (getMatches blogPattern >>= hasTag "icelandic")
    let visiblePattern =
          includePattern .&&. complement excludePattern

    pages <- buildPages visiblePattern (fromCapture "*/index.html" . show)
    categories <- buildCategories visiblePattern (fromCapture "*/index.html")
    tags <- buildTags visiblePattern (fromCapture "tags/*/index.html")

    -- static pages
    match "*.md" $ do
      route indexRoute
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/page-detail.html" defaultCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

    -- index
    create ["index.html"] $ do
      route idRoute
      compile $
        makeItem ""
          >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 pages categories tags)
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

    -- blogs
    match includePattern $ do
      route blogRoute
      compile $
        blogCompiler
          >>= modifyUrl
          >>= saveSnapshot blogSnapshot
          >>= loadAndApplyTemplate "templates/blog-detail.html" (blogDetailCtx categories tags)
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

    -- blog assets
    match assetPattern $ do
      route assetRoute
      compile copyFileCompiler

    -- blog pages
    paginateRules pages $ \i _ -> do
      route idRoute
      compile $
        makeItem (show i)
          >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i pages categories tags)
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

    -- blog category index
    tagsRules categories $ \category pattern -> do
      catPages <- buildPages pattern (\i -> fromCaptures "*/*/index.html" [category, show i])
      route idRoute
      compile $
        makeItem category
          >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 catPages categories tags)
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

      -- blog category pages
      paginateRules catPages $ \i _ -> do
        route idRoute
        compile $
          makeItem category
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i catPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

    -- blog tags index
    tagsRules tags $ \tag pattern -> do
      tagPages <- buildPages pattern (\i -> fromCaptures "tags/*/*/index.html" [tag, show i])
      route idRoute
      compile $
        makeItem tag
          >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 tagPages categories tags)
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= relativizeUrls

      -- blog tags pages
      paginateRules tagPages $ \i _ -> do
        route idRoute
        compile $
          makeItem tag
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i tagPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls

    -- decks
    match "decks/**.md" $ do
      route decksRoute
      compile $
        blogCompiler
          >>= modifyUrl
          >>= saveSnapshot decksSnapshot
          >>= loadAndApplyTemplate "templates/decks-detail.html" decksDetailCtx
          >>= relativizeUrls

    -- old decks (redirect)
    match "decks/**.md" . version "redirect" $ do
      route oldDecksRoute
      compile $ do
        dir <- maybe mempty (takeBaseName . takeDirectory) <$> (getUnderlying >>= getRoute)
        makeItem (Redirect $ "../../../" <> dir)

    -- deck assets
    match "decks/**" $ do
      route decksAssetsRoute
      compile copyFileCompiler

    -- deck index
    create ["decks/index.html"] $ do
      route idRoute
      compile $
        makeItem "decks"
          >>= loadAndApplyTemplate "templates/decks-list.html" decksCtx
          >>= loadAndApplyTemplate "templates/default.html" defaultCtx
          >>= modifyUrl
          >>= relativizeUrls

    -- atom
    create ["atom.xml"] $ do
      route idRoute
      compile $
        take feedLength <$> loadBlogs visiblePattern
          >>= renderBlogAtom

    -- static content
    match "static/**" $ do
      route uponeRoute
      compile copyFileCompiler

    -- static css
    match "css/**.css" $ do
      route idRoute
      compile compressCssCompiler

    match ("css/**.scss" .&&. complement "css/**_*.scss") $ do
      route $ setExtension "css"
      compile $ fmap compressCss <$> sassCompiler

    match "css/webfonts/*" $ do
      route idRoute
      compile copyFileCompiler

    -- disable GitHub's Jekyll
    create [".nojekyll"] $ do
      route idRoute
      compile $ makeItem ("" :: String)

    -- files to include code in blog posts
    match ("blogs/**.ts" .||. "drafts/**.ts") $ do
      compile $ getResourceLBS

    -- templates
    match "templates/*.html" $
      compile templateCompiler

--------------------------------------------------------------------------------
-- CONTEXTS
--------------------------------------------------------------------------------
defaultCtx :: Context String
defaultCtx =
  bodyField "page.body"
    <> pageTitleField "page.title"
    <> constField "page.description" blogDescription
    <> constField "page.root" blogRoot
    <> urlField "page.url"
    <> pathField "page.path"
    <> polishField "polish"
    <> metadataField

pageTitleField :: String -> Context String
pageTitleField key =
  aliasContext alias metadataField
    <> pathTitleField key -- use page title from metadata
    <> constField key "Crypto and Code" -- or read from the path
  where
    alias x | x == key = "title"
    alias x = x

blogCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
blogCtx i pages categories tags =
  listField "blogs" (blogDetailCtx categories tags) (loadBlogs pattern)
    <> categoryListField "categories" categories
    <> tagsListField "tags" tags
    <> pagesField i
    <> defaultCtx
  where
    pattern = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
    pagesField = aliasContext alias . paginateContext pages
    alias "pages.first.number" = "firstPageNum"
    alias "pages.first.url" = "firstPageUrl"
    alias "pages.next.number" = "nextPageNum"
    alias "pages.next.url" = "nextPageUrl"
    alias "pages.previous.number" = "previousPageNum"
    alias "pages.previous.url" = "previousPageUrl"
    alias "pages.last.number" = "lastPageNum"
    alias "pages.last.url" = "lastPageUrl"
    alias "pages.current.number" = "currentPageNum"
    alias "pages.count" = "numPages"
    alias x = x

blogDetailCtx :: Tags -> Tags -> Context String
blogDetailCtx categories tags =
  pageTitleField "blog.title"
    <> dateField "blog.date" "%B %e, %Y"
    <> urlField "blog.url"
    <> categoryField "blog.category" categories
    <> tagsField "blog.tags" tags
    <> field "blog.next.url" nextBlog
    <> field "blog.previous.url" previousBlog
    <> summaryField "blog.summary"
    <> readingTimeField "blog.reading.time" blogSnapshot
    <> defaultCtx

decksCtx :: Context String
decksCtx =
  listField "decks" decksDetailCtx (loadDecks "decks/**.md")
    <> defaultCtx

decksDetailCtx :: Context String
decksDetailCtx =
  dateField "date" "%B %e, %Y"
    <> urlField "url"
    <> defaultCtx
    <> constField "theme" "black"

feedCtx :: Context String
feedCtx =
  mapContext cdata (pageTitleField "title")
    <> aliasContext alias metadataField
    <> teaserField "description" blogSnapshot -- description from metadata
    <> previewField "description" blogSnapshot -- teaser is description
    <> urlField "url" -- first paragraph is description
  where
    alias "description" = "summary"
    alias x = x
    cdata s | "<![CDATA[" `isPrefixOf` s = s
    cdata s = "<![CDATA[" <> s <> "]]>"

--------------------------------------------------------------------------------
-- COMPILERS
--------------------------------------------------------------------------------
blogCompiler :: Compiler (Item String)
blogCompiler = do
  ident <- getUnderlying
  isTOC <- isMaybeTrue <$> getMetadataField ident "withtoc"
  pandocCompilerWithTransformM blogReaderOptions (writerOptions isTOC) includeCode
  where
    writerOptions toc =
      if toc
        then blogWriterOptions
        else defaultHakyllWriterOptions

sassCompiler :: Compiler (Item String)
sassCompiler = do
  ident <- getUnderlying
  output <- unixFilter "sass" [toFilePath ident] ""
  makeItem output

-- includeCode tranforms the Pandoc code blocks, to include files relative to the blog post
--    $include("program.ts")$ will insert the contents of program.ts
includeCode :: Pandoc -> Compiler Pandoc
includeCode (Pandoc meta blocks) =
  Pandoc <$> pure meta <*> mapM updateCodeBlock blocks
  where
    updateCodeBlock :: Block -> Compiler Block
    updateCodeBlock (CodeBlock attr block) = do
      CodeBlock <$> pure attr <*> includeFile block
    updateCodeBlock b = return b

data Segment = Include FilePath | Static Text

includeFile :: Text -> Compiler Text
includeFile text = do
  mconcat <$> traverse include (parseSegments (unpack text))
  where
    include :: Segment -> Compiler Text
    include (Static t) = return t
    include (Include file) = do
      fp <- getResourceFilePath
      decodeUtf8 . toStrict <$> loadBody (fromFilePath (takeDirectory fp </> file))

    parseSegments :: String -> [Segment]
    parseSegments =
      concatMap fst . (readP_to_S (many1 parseSegment <* eof))
      where
        parseSegment = parseInclude <++ parseStatic
        parseStatic = Static . pack <$> munch1 (not . (== '$'))
        parseInclude = Include <$> (string "$include(" *> (surround '"' path <++ path) <* string ")$")
          where
            path = munch1 (not . flip elem ['"', ')'])
            surround c = between (char c) (char c)

modifyUrl :: Item String -> Compiler (Item String)
modifyUrl item = do
  fp <- liftM (fromMaybe mempty) $ getRoute =<< getUnderlying
  traverse (return . withTags (modifyTag fp)) item
  where
    modifyTag fp = \case
      (TagOpen "a" attrs) -> TagOpen "a" (modifyAttr fp <$> attrs)
      (TagOpen "img" attrs) -> TagOpen "img" (modifyAttr fp <$> attrs)
      tag -> tag
    modifyAttr fp = \case
      ("href", url)
        | not (isExternal url) ->
            ("href", toUrl $ takeDirectory fp </> dropIndex url)
      ("src", url)
        | not (isExternal url) ->
            ("src", toUrl $ takeDirectory fp </> dropIndex url)
      attr -> attr
    -- dropHash = dropWhile (/= '#')
    dropIndex = \case
      "index.html" -> mempty
      url -> url

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
      lookup x $ zip xs (drop 1 xs)

previousBlog :: Item String -> Compiler String
previousBlog blog = do
  blogs <- loadBlogs blogPattern
  let idents = map itemIdentifier blogs
      ident = itemBefore idents (itemIdentifier blog)
  maybe empty (fmap (maybe empty toUrl) . getRoute) ident
  where
    itemBefore xs x =
      lookup x $ zip (drop 1 xs) xs

loadDecks :: Pattern -> Compiler [Item String]
loadDecks =
  traverse modifyUrl
    <=< recentFirst
    <=< flip loadAllSnapshots decksSnapshot . (.&&. hasNoVersion)

renderBlogAtom :: [Item String] -> Compiler (Item String)
renderBlogAtom =
  renderAtom feedConfiguration feedCtx

--------------------------------------------------------------------------------
-- ROUTES
--------------------------------------------------------------------------------
uponeRoute :: Routes
uponeRoute =
  -- ..
  customRoute (joinPath . drop 1 . splitPath . toFilePath)

indexRoute :: Routes
indexRoute =
  -- name/index.html
  removeExtension `composeRoutes` addIndex
  where
    removeExtension = setExtension mempty
    addIndex = postfixRoute "index.html"
    postfixRoute postfix = customRoute $ (</> postfix) . toFilePath

dropDateRoute :: Routes
dropDateRoute =
  gsubRoute "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}-" (const mempty)

blogRoute :: Routes
blogRoute =
  -- YYYY/MM/name/index.html
  assetRoute
    `composeRoutes` setExtension mempty
    `composeRoutes` customRoute ((</> "index.html") . takeDirectory . toFilePath)

assetRoute :: Routes
assetRoute =
  -- YYYY/MM/name/file.ext
  (matchRoute "*/*/*/*" dateFolderRoute <> matchRoute "*/*/*" dateFileRoute)
    `composeRoutes` dropDateRoute
  where
    dateFileRoute = customRoute $ \ident -> dateFolder ident </> dropExtension (fileName ident) </> fileName ident
    dateFolderRoute = customRoute $ \ident -> dateFolder ident </> dirName ident </> fileName ident
    dateFolder = maybe mempty (formatTime defaultTimeLocale "%Y/%m") . filePathDate
    fileName = takeFileName . toFilePath
    dirName = takeBaseName . takeDirectory . toFilePath
    -- filePathDate walks down the file path from right to left,
    -- and tries to parse a date in the file or directory name on the form YYYY-MM-DD-name.
    filePathDate = asum . (fmap (tryParseDate "%Y-%m-%d" . takeDate) . (reverse . splitPath . toFilePath))
      where
        takeDate = intercalate "-" . take 3 . splitAll "-"

decksRoute :: Routes
decksRoute =
  -- decks/name/index.html
  decksAssetsRoute
    `composeRoutes` customRoute ((</> "index.html") . takeDirectory . toFilePath)

decksAssetsRoute :: Routes
decksAssetsRoute =
  -- decks/name/file.ext
  dropDateRoute

oldDecksRoute :: Routes
oldDecksRoute =
  blogRoute
    `composeRoutes` prefixRoute "decks"
  where
    prefixRoute prefix = customRoute $ (prefix </>) . toFilePath

--------------------------------------------------------------------------------
-- CONTEXTS
--------------------------------------------------------------------------------
pathTitleField :: String -> Context String
pathTitleField =
  flip field title
  where
    title = maybe empty (emptyTitle . pageTitle) <=< getRoute . itemIdentifier
    pageTitle = intercalate " &#x276f;&#x276f;= " . splitDirectories . capitalize . dropFileName
    emptyTitle "." = empty
    emptyTitle x = return x
    capitalize [] = []
    capitalize (x : xs) = toUpper x : map toLower xs

urlField :: String -> Context String
urlField =
  mapContext dropFileName . C.urlField

categoryField :: String -> Tags -> Context a
categoryField =
  tagsFieldWith getCategory (renderLink "@") mconcat

categoryListField :: String -> Tags -> Context a
categoryListField key tags =
  field key (const $ renderList tags)
  where
    renderList = renderTags makeLink unwords
    makeLink tag url _ _ _ = renderHtml $ do
      "@"
      H.a ! href (toValue url) $ toHtml tag

tagsField :: String -> Tags -> Context a
tagsField =
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
  field key meta -- summary from metadata
    <> teaserField key blogSnapshot -- teaser is summary
    <> previewField key blogSnapshot -- first paragraph is summary
  where
    meta :: Item a -> Compiler String
    meta item = do
      summary <- getMetadataField' (itemIdentifier item) "summary"
      return . renderHtml $
        H.p (toHtml summary)

previewField :: String -> Snapshot -> Context String
previewField key snapshot =
  field key trim'
  where
    trim' item = do
      body <- loadSnapshotBody (itemIdentifier item) snapshot
      return $ withTagList firstParagraph body
    firstParagraph = map fst . takeWhile (\(_, s) -> s > (0 :: Integer)) . acc 0 . map cnt
    acc _ [] = []
    acc s ((x, s') : xs) = (x, s + s') : acc (s + s') xs
    cnt tag@(TagOpen "p" _) = (tag, 1)
    cnt tag@(TagClose "p") = (tag, -1)
    cnt tag = (tag, 0)

readingTimeField :: String -> Snapshot -> Context String
readingTimeField key snapshot =
  field key calculate
  where
    calculate item = do
      body <- loadSnapshotBody (itemIdentifier item) snapshot
      return $ withTagList acc body
    acc ts = [TagText (show (time ts))]
    time ts = foldl' count 0 ts `div` 265
    count n (TagText s) = n + length (words s)
    count n _ = n

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
    text' (TagText s) = TagText (concatMap emoji (split isSpace s))
    text' t = t

    split :: (Char -> Bool) -> String -> [String]
    split p' s =
      go p' ("", s)
      where
        go _ ("", "") = []
        go p ("", y) = go (not . p) (break p y)
        go p (x, y) = x : go (not . p) (break p y)

    emoji "" = ""
    emoji ":+1:" = "👍"
    emoji ":coffee:" = "☕️"
    emoji ":disappointed:" = "😞"
    emoji ":frowning:" = "😦"
    emoji ":grinning:" = "😀"
    emoji ":heart:" = "❤"
    emoji ":ramen:" = "🍜"
    emoji ":rice_ball:" = "🍙"
    emoji ":smile:" = "😄"
    emoji ":sushi:" = "🍣"
    emoji ":stuck_out_tongue:" = "😛"
    emoji ":thumbsup:" = "👍"
    emoji ":tada:" = "🎉"
    emoji x = x

--------------------------------------------------------------------------------
-- METADATA
--------------------------------------------------------------------------------
hasTag :: (MonadMetadata m) => String -> [Identifier] -> m [Identifier]
hasTag tag =
  filterM (fmap (elem tag) . getTags)

-- getCategory uses the second deepest folder as the category
getCategory :: (MonadMetadata m) => Identifier -> m [String]
getCategory =
  return
    . maybeToList
    . fmap takeDirectory
    . listToMaybe
    . drop 1
    . splitPath
    . toFilePath

buildCategories :: (MonadMetadata m) => Pattern -> (String -> Identifier) -> m Tags
buildCategories = buildTagsWith getCategory

buildPages :: (MonadMetadata m, MonadFail m) => Pattern -> (PageNumber -> Identifier) -> m Paginate
buildPages =
  buildPaginateWith
    (return . paginateEvery blogPerPage <=< sortRecentFirst)

--------------------------------------------------------------------------------
-- HTML
--------------------------------------------------------------------------------
renderLink :: String -> String -> Maybe FilePath -> Maybe H.Html
renderLink _ _ Nothing = Nothing
renderLink pre text (Just url) =
  Just $ do
    toHtml pre
    H.a ! href (toValue $ toUrl url) $ toHtml text

--------------------------------------------------------------------------------
-- DATES
--------------------------------------------------------------------------------
tryParseDate :: String -> String -> Maybe UTCTime
tryParseDate format =
  tryParseDateWithLocale defaultTimeLocale format

tryParseDateWithLocale :: TimeLocale -> String -> String -> Maybe UTCTime
tryParseDateWithLocale locale format =
  parseTimeM True locale format

isMaybeTrue :: Maybe String -> Bool
isMaybeTrue Nothing = False
isMaybeTrue (Just s)
  | map toUpper s == "TRUE" = True
  | otherwise = False
