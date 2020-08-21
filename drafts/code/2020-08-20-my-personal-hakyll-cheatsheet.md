---
title: My personal Hakyll cheatsheet
tags: haskell
withtoc: true
summary: A cheatsheet for my future self. We cover using the Hakyll eDSL to write website rules, advanced configuration which enables interesting features such as auto-generation of table of contents and \( \LaTeX \) math support on this website as well as discussion on how to roll your own Hakyll constructs.
---

# Hakyll :heart: Pandoc
Since I don't write Haskell code professionally anymore it takes me longer to get into the right rhythm. This post is intended for my future self more or less and should serve as a cheatsheet for Hakyll development. I've already written a high level overview of how to edit content and build the website in this [README](https://github.com/gislik/gisli.hamstur.is/blob/master/README.md). Here I want to go deeper into how to construct new compilers and how to apply them in a context to templates. 

Under the hood Hakyll integrates natively with [Pandoc](http://johnmacfarlane.net/pandoc/) -- the swiss-army knife of file converters. Pandoc is also written in Haskell and can convert files between a wide variety of file formats and can be extended with custom [Lua](http://www.lua.org/) filters. All this has some configuration complexities associated with it and below I also discuss the various configurations and extensions used to enable the auto-generation of table of contents and $\LaTeX$ math support on this website.  


# Website rules

## Matching clause, routes and compilers

Writing rules in the DSL[^1] is simple enough. A rule needs three things to be valid; a matching clause for the source file, a route for the compiled output and the compiler itself which is responsible for the transformation between the input and the output. 

[^1]: DSL stands for Domain Specific Language. Hakyll is an eDSL, embedded Domain Specific Language, which means that the DSL is defined in the underlying programming languages code as opposed to a completely new language.

~~~haskell
-- static css
match "css/**.css" $ do
   route   $ idRoute
   compile $ compressCssCompiler
~~~

We match all files with the `.css` extension in the `css` directory and its subdirectories (had we used \* only files in the `css` directory would match). The `idRoute` preserves the filename as is but other routes may change the e.g. the extension or something more complex. For example we want to match files with `.scss` extension, compile them and pipe the compressed results to a file with the `.css` extension for browser compatibility.

~~~haskell
match ("css/**.scss" .&&. complement "css/**_*.scss") $ do
   route   $ setExtension "css"
   compile $ sassCompiler
      >>= return . fmap compressCss
~~~

In some cases instead of a source file to match we just create an output directly. This is the case with `index.html` which is dynamically generated from from the latest blog posts.

~~~haskell
-- index
create ["index.html"] $ do
   route   $ idRoute
   compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 pages categories tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx 
      >>= indexCompiler
      >>= relativizeUrls
~~~

## Templates and their contexts

Optionally the compiler may load apply a template to the input and interpolate variables from a context. We do so by chaining multiple compilers together into a more feature rich compiler. The output of a previous compiler is interpolated into the \$body\$ variable of the template. Other variables are made available to the template by defining them in the context. Before templates can be loaded they need to be compiled using the `templateCompiler`. 

~~~haskell
-- static pages
match "*.md" $ do
   route   $ pageRoute
   compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page-detail.html" defaultCtx
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx
      >>= relativizeUrls

-- templates
match "templates/*.html" $  -- note that the order does not matter
   compile $ templateCompiler
~~~

Contexts are monoids and can therefore be easily combined to create bigger contexts where the earlier definition of a non-empty (`mempty`) field takes precedence.

~~~haskell
pageTitleField :: String -> Context String
pageTitleField key = 
   aliasContext alias metadataField <> -- use page title from metadata
   pathTitleField key               <> -- or read from the path
   constField key "Crypto and Code"    -- alternatively use this
   where
      alias x | x == key = "title"
      alias x            = x
~~~

A special case of the string context is the function context which defines a variable which value is a function which can be called in the template.

~~~html
<header>
  $partial("templates/header.html")$
</header>

<p>
  $countwords(body)$
  $sum(1,2,3)$
</p>
~~~

## Metadata, flow control and list iteration

Metadata can be placed in the front matter of the markdown formatted as [YAML](https://yaml.org/). The metadata can be made available both to the compiler and to the template in case the `metadataField :: Context a` is applied to the template. Tags can eiter be comma separated or as a valid YAML list.

~~~markdown
---
title: This is the blog title
tags: tag1, tag2
tags: 
   - tag1
   - tag2
summary: |
  Introduction to the blog content
---

This is the blog body
~~~


Special function variables are used to define conditional branches to display and iterate over items. The variable names used within the \$for(blogs)\$ loop are defined in the `blog` list field. Everything between \$sep\$ and \$endfor\$ will be used as a separator between items, i.e. it is not included for the last element.

~~~html
<div class="blog-list">
$for(blogs)$
  <div class="blog-item">
    $title$
  $if(blog.author)$
    $blog.author$
  $else$ 
    Unknown author
  $endif$
  <gdiv>
  $sep$
  <hr g>
$endfor$
</div>
~~~

# Advanced features

Using the `pandocCompilerWith` some options can be passed in which affect the behavior of Pandoc. In addition extensions can be enabled which unlock additional features. 

~~~haskell
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)

blogCompiler :: Compiler (Item String)
blogCompiler = do
   ident <- getUnderlying
   toc   <- getMetadataField ident "withtoc"
   pandocCompilerWith blogReaderOptions (maybe defaultOptions blogOptions toc)
   where
      defaultOptions = defaultHakyllWriterOptions
      blogOptions = const blogWriterOptions

~~~

## Auto-generated table of contents

~~~haskell
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
         in
            Just . renderHtml $ do
               H.div ! class_ "toc" $ do
                  toHtml toc
               toHtml body
      }
~~~

## \( \KaTeX\ \) to render \( \LaTeX \)  math

Pandoc can transform math to MathJax when configured correctly. KaTeX is able to render the output by embedding the javascript and CSS.

~~~html
<link rel="stylesheet" href="/katex/katex.min.css">
<script defer src="/katex/katex.min.js"></script>
<script type="text/javascript" script defer src="/katex/auto-render.min.js" 
  onload="renderMathInElement(document.body);"></script>
~~~

Equations can be place on their own lines.

<p class="center">
`$$ \ln x = \int_{-\infty}^x \frac 1 y \, dy  $$`{.center}
</p>
<p class="center">
or
</p>
<p class="center">
`\[ \ln x = \int_{-\infty}^x \frac 1 y \, dy  \]`{.center}
</p>

<p class="center">
becomes
</p>
$$ \ln x = \int_{-\infty}^x \frac 1 y \, dy  $$



Writing `$x \equiv a \pmod{b}$` or `\( x \equiv a \pmod{b} \)` prdouces \( x \equiv a \pmod{b} \).


~~~haskell
blogReaderOptions :: ReaderOptions
blogReaderOptions = 
   defaultHakyllReaderOptions
      {
         readerExtensions = 
            (readerExtensions defaultHakyllReaderOptions) <> extensionsFromList
               [ 
                 Ext_tex_math_single_backslash  -- TeX math btw (..) [..]
               , Ext_tex_math_double_backslash  -- TeX math btw \(..\) \[..\]
               , Ext_tex_math_dollars           -- TeX math between $..$ or $$..$$
               , Ext_latex_macros               -- Parse LaTeX macro definitions (for math only)
               , Ext_inline_code_attributes     -- Ext_inline_code_attributes
               , Ext_abbreviations              -- PHP markdown extra abbreviation definitions
               ]
      }
~~~

## Decks and presentations

Gooogle decks can easily be embedded using an HTML snippet and requires no special handling on Hakyll's side. I wrap the standard `<iframe>` tag in a `div` tag with the a special class which makes the presentation responsive.

~~~html
<div class="responsive">
<iframe src="https://docs.google.com/presentation/d/e/<ID>/embed
  ?start=false&loop=false&delayms=3000" frameborder="0" width="960" height="569" 
  allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>
</div>
~~~

~~~css
.responsive {
  overflow: hidden;
  padding-bottom:56.25%;
  position: relative;
  height: 0;
  iframe {
    left: 0;
    top: 0;
    height: 100%;
    width: 100%;
    position: absolute;
  }
}
~~~

[reveal.js](https://revealjs.com/) -- the HTML presentation framework -- uses standard HTML tags to define the presentation and supports themes. Enabling reveal.js is achieved by modifying the deck's base template and adding a few options to the decks metadata. 

~~~markdown
---
reveal: true
theme: league
---
~~~


~~~html
<head>
$if(reveal)$
  <link rel="stylesheet" href="/reveal.js/reset.css">
  <link rel="stylesheet" href="/reveal.js/reveal.css">
  <link rel="stylesheet" href="/reveal.js/theme/$theme$.css" id="theme">

  <!-- Theme used for syntax highlighted code -->
  <link rel="stylesheet" href="/reveal.js/plugin/highlight/monokai.css" id="highlight-theme">
$endif$
</head>
~~~

~~~html
<body>
  <div class="reveal">
    <div class="slides">
      $body$
    </div>
  </div>

$if(reveal)$
  <script src="/reveal.js/reveal.js"></script>
  <script src="/reveal.js/plugin/notes/notes.js"></script>
  <script src="/reveal.js/plugin/markdown/markdown.js"></script>
  <script src="/reveal.js/plugin/highlight/highlight.js"></script>
  <script>
    // More info about initialization & config:
    // - https://revealjs.com/initialization/
    // - https://revealjs.com/config/
    Reveal.initialize({
      hash: true,

      // Learn about plugins: https://revealjs.com/plugins/
      plugins: [ RevealMarkdown, RevealHighlight, RevealNotes ]
    });
		</script>
$endif$
</body>
~~~

## Paginating posts

Blog posts are paginated by constructing a `Paginate` object with `buildPaginateWith` and then creating rules for every page. 

~~~haskell
type PageNumber = Int

data Paginate = Paginate
   { paginateMap        :: M.Map PageNumber [Identifier] -- used in blogCtx
   , paginateMakeId     :: PageNumber -> Identifier 
   , paginateDependency :: Dependency
   }

buildPaginateWith
    :: MonadMetadata m
    => ([Identifier] -> m [[Identifier]])  -- group items into pages
    -> Pattern                             -- items to paginate
    -> (PageNumber -> Identifier)          -- identifiers for the pages
    -> m Paginate
~~~

~~~haskell
hakyll $ do

   pages <- buildPages visiblePattern (\i -> fromCapture "*/index.html" (show i))

   paginateRules pages $ \i _ -> do -- i is the page number
      route   $ idRoute
      compile $ makeItem (show i)
         >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i pages categories tags)
         >>= loadAndApplyTemplate "templates/default.html" defaultCtx
         >>= indexCompiler
         >>= relativizeUrls

buildPages :: (MonadMetadata m, MonadFail m) 
   => Pattern -> (PageNumber -> Identifier) -> m Paginate
buildPages pattern makeId = 
   buildPaginateWith
      (return . paginateEvery blogPerPage <=< sortRecentFirst) 
      pattern 
      makeId

~~~

To access pages pagination links are added to the relevant context. `paginateContext` returns a default paginate context which provides standard pagination fields but I like to alias those fields to give them a bit more friendly names.

~~~haskell
-- paginateContext returns a default pagin
paginateContext :: Paginate -> PageNumber -> Context a

blogCtx :: PageNumber -> Paginate -> Tags -> Tags -> Context String
blogCtx i pages categories tags = 
      listField "blogs" (blogDetailCtx categories tags) (loadBlogs pat) <>
      categoryListField "categories" categories                         <>
      tagsListField "tags" tags                                         <>
      pagesField i                                                      <> --page links
      defaultCtx
  where
      pat = fromList . fromMaybe [] . M.lookup i . paginateMap $ pages
      pagesField = aliasContext alias . paginateContext pages -- alias standard fields
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
~~~

## Support for tags and categories

Tags are supported in a similar manner to pagination. The `Tags` object is constructed with either `buildTags` or `buildTagsWith`. Hakyll provides [functions](https://www.stackage.org/haddock/lts-16.10/hakyll-4.13.4.0/Hakyll-Web-Tags.html) to sort tags, render them and adding to a context. Rules for each tag must be created `using tagsRules` in order for tags to work. 


~~~haskell
data Tags = Tags
   { tagsMap        :: [(String, [Identifier])]
   , tagsMakeId     :: String -> Identifier
   , tagsDependency :: Dependency
   }

-- buildTags takes a pattern for loading resources and 
-- a mapping from the tag name to its identifier
buildTags :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags

-- getTags returns the tag field from the metadata as a list of strings
-- the field value can either be tags separated by a comma or a valid YAML list
getTags :: MonadMetadata m => Identifier -> m [String]
~~~


~~~haskell
hakyll $ do

   tags <- buildTags visiblePattern (fromCapture "tags/*/index.html")

   -- index
   create ["index.html"] $ do
      route   $ idRoute
      compile $ makeItem ""
         -- tags passed to blogCtx
         >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 pages categories tags)
         >>= loadAndApplyTemplate "templates/default.html" defaultCtx 
         >>= indexCompiler
         >>= relativizeUrls

      tagsRules tags $ \tag pattern -> do
         tagPages <- buildPages pattern (\i -> fromCaptures "tags/*/*/index.html" [tag, show i])
         route   $ idRoute
         compile $ makeItem tag
            >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 tagPages categories tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= indexCompiler
            >>= relativizeUrls
         paginateRules tagPages $ \i _ -> do -- blog tags pages (i is page within tag)
            route idRoute
            compile $ do
               makeItem tag
                  >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx i tagPages categories tags)
                  >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                  >>= indexCompiler
                  >>= relativizeUrls

blogDetailCtx :: Tags -> Tags -> Context String
blogDetailCtx categories tags = 
   dateField "date" "%B %e, %Y"                 <>
   mapContext dropFileName (urlField "url")     <>
   categoryField' "category" categories         <>
   tagsField' "tags" tags                       <> -- tags to context
   field "pages.next.url" nextBlog              <>
   field "pages.previous.url" previousBlog      <>
   defaultCtx                                   <> 
   teaserField "summary" blogSnapshot           <> 
   previewField "summary" blogSnapshot          <> 
   readingTimeField "reading.time" blogSnapshot
~~~

Tags can be rendered in a standard way using `renderTags`, as a tag cloud using `renderTagCloud` or in a custom way using `tagsFieldWith`.

~~~haskell
tagsField' :: String -> Tags -> Context a 
tagsField' = 
   tagsFieldWith getTags (renderLink "#") (mconcat . intersperse " ")

renderLink :: String -> String -> (Maybe FilePath) -> Maybe H.Html
renderLink _ _   Nothing       = Nothing
renderLink pre text (Just url) =
   Just $ do
      toHtml pre
      H.a ! href (toValue $ toUrl url) $ toHtml text
~~~

# Rolling your own

## Collecting rules in the Rules monad

The compiler rules live inside the `Rules` monad which is an instance of the `MonadMetadata` type class which enables monads to retrive the `Metadata` of the source file identified by the `Identifier`. `Metadata` is a wrapper around a YAML object with [functions](https://www.stackage.org/haddock/lts-16.10/hakyll-4.13.4.0/Hakyll-Core-Metadata.html) to lookup it's values as either strings or lists of strings.

~~~haskell
data Rules a

instance Monad Rules
instance Functor Rules
instance Applicative Rules
instance MonadMetadata

class Monad m => MonadMetadata m where
   getMetadata    :: Identifier -> m Metadata
   getMatches     :: Pattern -> m [Identifier]

-- getAllMetadata returns all metadata associated with a pattern
getAllMetadata :: MonadMetadata m => Pattern -> m [(Identifier, Metadata)]

-- getItemUTC tries to extract and parse the time from the published field 
-- or from the filename. 
getItemUTC :: MonadMetadata m	=> TimeLocale	-> Identifier	-> m UTCTime	

-- match matches a pattern and adds a rule for it.
match :: Pattern -> Rules () -> Rules ()

-- hakyll runs the rules in the IO monad
hakyll :: Rules a -> IO ()
~~~

## Identifiers and patterns

~~~haskell
data Item a = Item
   { itemIdentifier :: Identifier
   , itemBody       :: a
   } deriving (Show, Typeable)

instance Item
instance Item
instance Item
instance Show a => Show (Item a)
instance Binary a => Binary (Item a)
~~~


## Writing Compilers and loading Items
The other monad to implement the `MonadMetadata` type class is the `Compiler` monad which as the name implies compiles matched source files and takes care of dependencies between rules such that if one rule relies on the results of another rule they are executed in the correct order. The `Compiler` monad is an instance of `Alternative` which makes it easy to combine potentially failing compilers (using `empty`, `fail`, or `throwError`) compilers which are tried in sequence until one of them succeeds or else the combined compiler fails.

`Compiler a` values have access to a range of functions which can retrieve the identifier, the source and path of both the matched file and any file identified by an `Identifier`. Keep in mind that for such function to succeed there needs to exist a rule for that `Identifier` in the `Rules` monad. Otherwise Hakyll doesn't have any knowledge of its existence.

~~~haskell
data Compiler a -- a is the the type of the output - usually String

instance Monad Compiler
instance Functor Compiler
instance Applicative Compiler
instance Alternative Compiler
instance MonadMetadata Compiler
instance MonadError Compiler

-- compile adds a compiler to the Rules monad. Note that the Compiler value is Item a
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()

-- makeItem lifts a value into the the Compiler monad.
makeItem :: a -> Compiler (Item a)

-- getRoute returns the route for a specified item
getRoute :: Identifier -> Compiler (Maybe FilePath)

Parser to try to extract and parse the time from the published field or from the filename. See dateField for more information. Exported for user convenience.

-- getResourceBody returns the full contents of the matched source file
-- as a string without metadata preamble.
getResourceBody :: Compiler (Item String)

-- load an item compiled elsewhere.
load :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)

-- loadAll loads a dynamic list of items.
loadAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
~~~


## Snapshots to define a pipeline stage

When loading a compiled resource it usually has the base layout templates applied to it making it unusable in dynamic lists. Snapshots are the solution to that problem. During the compiler construction a snapshot of the resource can be saved in middle of the pipeline allowing you to later `load` that stage of the compiled resource.

~~~haskell
-- blogs
match allPattern $ do
   route   $ blogRoute
   compile $ blogCompiler
      >>= saveSnapshot blogSnapshot -- saving a Snapshot before templates are applied
      >>= loadAndApplyTemplate "templates/blog-detail.html" (blogDetailCtx categories tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx
      >>= indexCompiler
      >>= relativizeUrls

loadBlogs :: Pattern -> Compiler [Item String]
loadBlogs = 
   recentFirst <=< flip loadAllSnapshots blogSnapshot -- load blog items using a Snapshot
~~~


## Working with contexts and templates

Contexts define variables which can be accessed and interpolated in a template. There are three ways to define a `Context`.

1. Use the `Context` data constructor.
1. Use field functions, e.g. `field`, `constField` or `listField`.
1. Derive a new context from another context using `mapContext`.

~~~haskell
data ContextField
   = StringField String
   | forall a. ListField (Context a) [Item a]

-- Context data constructor wraps a function which takes a key (variable name), 
-- a list of arguments (for function fields), an item and returns a ContextField
-- in the Compiler monad
newtype Context a = Context
   { unContext :: String -> [String] -> Item a -> Compiler ContextField
   }

instance Semigroup (Context a)
instance Monoid (Context a) where
   -- mempty is basically empty from Alternative
   mempty                          = missingField 
   -- mappend is <|> from Alternative
   mappend (Context f) (Context g) = Context $ \k a i -> f k a i <|> g k a i 

-- field takes a key and a function that constructs a value based on the item 
-- (e.g. accessing metadata) and returns a new context.
field :: String	-> (Item a -> Compiler String)-> Context a	 

-- constField takes a key and a constant value and returns a context
constField :: String -> String -> Context a

-- listField takes a string, a context to be applied inside a 
-- $for(..)$ expression, and the items to iterate over.
listField :: String -> Context a -> Compiler [Item a] -> Context b

-- mapContext takes a function, a context and returns a new context
-- with all field values transformed.
mapContext :: (String -> String) -> Context a -> Context a
~~~

The `Context` data constructor is rarely used but necessary if the the context needs to be able to return `mempty` which is implimented as a failing `Compiler`. In the `Compiler` monad failure can be signalled with `empty`, `noResult` or `throwError`.

~~~haskell
-- aliasContext maps a new key to another key. If the other key
-- is not defined or returns empty the alias returns empty.
aliasContext :: (String -> String) -> Context a -> Context a
aliasContext f (Context c) = 
   Context $ \k a i -> c (f k) a i <|> c' k -- Compiler implements Alternative
   where 
      c' k = noResult $ unwords ["Tried to alias", k, "as", f k, "which doesn't exist"]

readingTimeField :: String -> Snapshot -> Context String
readingTimeField key snapshot = 
   field key calculate -- calculate :: Item String -> Compiler String
   where
      calculate item = do
         body <- loadSnapshotBody (itemIdentifier item) snapshot
         return $ withTagList acc body
      acc ts = [TagText (show (time ts))]
      time ts = foldl' count 0  ts `div` 265
      count n (TagText s) = n + length (words s)
      count n _           = n

decksDetailCtx :: Context String
decksDetailCtx = 
   dateField "date" "%B %e, %Y"             <>
   mapContext dropFileName (urlField "url") <> -- drops the file name from the urlField
   defaultCtx                               <>
   constField "theme" "black"

-- fail with an error message
noResult :: String -> Compiler a

-- empty is the identity of <|>
empty :: Alternative f => f a

-- throwError used within a monadic computation begins exception processing
throwError :: Monad m, MonadError e m | m -> e => e -> m a
~~~

