# gisli.hamstur.is

In this repo you will find the source code for my personal [blog](https://gisli.hamstur.is) where I write about **crypto** and **code**. The website is compiled from [Markdown](https://daringfireball.net/projects/markdown/) files using [Hakyll](https://jaspervdj.be/hakyll/). Hakyll allows you to compose a compiler using a DSL which specifies the rules which transform the inputs to the final output. The source code for my compiler is [here](blog.hs).


## Dependencies

The project depends on:

-  The [stackage](https://www.stackage.org/) build system 

    Please see the chapter [How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install) in the official documentation for stack. On macOS I find it easisest to use [Homebrew](https://brew.sh/) with `brew install haskell-stack`.

- The [sass](https://sass-lang.com/install) SCSS compiler

  Install using either Node.js or Homebrew.

  - `npm install -g sass` (using Node.js) 
  - `brew install sass/sass/sass` (using Homebrew)

## Building

~~~
$ stack build
~~~

Stack will pull in all the dependencies specified in the [cabal](blog.cabal) file, the correct version of `ghc, and build the compiler.


## Writing content

Pages and blogs are written in static Markdown like the [About](about.md) page. Metadata can be placed in the front matter formatted as [YAML](https://yaml.org/). The metadata can be made available both to the compiler and to the template.

~~~markdown
---
title: This is the blog title
tags: tag1, tag2
summary: |
  Introduction to the blog content
---

This is the blog body
~~~

Pages in the `blog` are considered to be blog posts which have additional features like pagination, categories and tags. Blog posts are ordered newest to oldest and each post's date is extracted from the `date` field, the `published` field or from the a prefix of the filename. Tags are generated from the `tags` field in the metadata but categories are derived from the parent folder's name.

Some parts of the site are generated without an existing source using Hakyll combinators like the index page which lists a few of the latest blog. 

~~~haskell
create ["index.html"] $ do
    route   $ idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/blog-list.html" (blogCtx 1 pages categories tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx 
      >>= indexCompiler
      >>= relativizeUrls
~~~

### Generating table of contents

For longer blog posts or pages it can be very convenient to include a table of contains. This is simply achieved by defining the `withtoc` field in the metadata. The compiler now uses `<h1></h1>` and `<h2></h2>' tags as the table entries.

~~~markdown
---
title: This is a long blog post
withtoc: true
---
~~~

### Writing math equations

Math equations can be written in LaTeX and enclosed in double dollar signs. The equations are rendered by [KaTeX](https://katex.org/).

$$ \ln x = \int_{-\infty}^x \frac 1 y \, dy  $$

### Decks

Decks and presentations are created in a similar fashion to blog files but placed in the `decks` directory instead. Decks either contain embedded presentation such as Google Slides or marked up and rendered by [reveal.js](https://revealjs.com/). To enable reveal.js in a presentation the `reveal` key has to be defined. Optionally a `theme` key controls which theme should be used to render the presentation.

~~~markdown
---
reveal: true
theme: league
featureimage: 2014/06/bitcoin.png
---

Rest of the deck
~~~

## Editing templates

Templates are found in the `templates` directory. The base layout is defined in `default.html` and its $body$ variable is replaced by the previous step in the build pipeline. Other variables include:

- `url` for the destination URL of the page
- `path` for the original filepath of the page
- `foo` where foo is specified in the metadata
- `title` for the blog title
- `page.title` for the page title
- `polish(text)` to exchange certain words in the text for an emoji

Blog lists has a `blogs` variable which can be iterated over and these additional variables:

- `pages.first.number`   
- `pages.first.url`      
- `pages.next.number`    
- `pages.next.url`       
- `pages.previous.number`
- `pages.previous.url`   
- `pages.last.number`    
- `pages.last.url`       
- `pages.current.number` 
- `pages.count`          

Single blog posts have these additions:

- `category`
- `tags`
- `dages.next.url`
- `pages.previous.url`
- `summary`
- `reading.time`

Deck lists has a `decks` variable which is a list field and can be iterated over.

Single decks have a `theme` variable.

For more information about templates including control flow, conditionals, partials and loops, refer to this [tutorial](https://jaspervdj.be/hakyll/tutorials/04-compilers.html).


## Compiling the website

I have created a script transforms the Markdown to HTML using the compiler. Relevant templates from the `templates` directory are applied in the build pipeline before the final results are stored in the `_site` directory. 

~~~
$ ./build
~~~

When working on the content itself it is quite convenient to have a HTTP server which watches which files have been saved recently and compiles the website incrementally. There is also a script for that which binds the server to `http://localhost:8000`.

~~~
$ ./watch
~~~

