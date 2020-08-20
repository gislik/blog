---
title: My personal Hakyll cheatsheet
tags: haskell
withtoc: true
---

Since I don't write Haskell code professionally anymore it takes me longer to get into the right rythm. This post is intended for my future self more or less and should serve as a cheatsheet for Hakyll development. I've already written a high level overview of how to edit content and build the website in this [README](https://github.com/gislik/gisli.hamstur.is/blob/master/README.md). Here I want to go deeper into how to construct new compilers and how to apply them in a context to templates. 

Under the hood Hakyll integrates natively with [Pandoc](http://johnmacfarlane.net/pandoc/) -- the swiss-army knife of file converters. Pandoc is also written in Haskell and can convert files between a wide variety of file formats and can be extended with custom [Lua](http://www.lua.org/) filters. All this has some configuration complexities associated with it and below I also discuss the various configurations and extensions used to enable the auto-generation of table of contents and LaTeX math support on this website.


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

Optionally the compiler may load apply a template to the input and interpolate variables from a context. We do so by chaining multiple compilers together into a more feature rich compiler. The output of a previous compiler is interpolated into the \$body\$ variable of the template. Other variables are made available to the template by defining them in the context.

~~~haskell
-- static pages
match "*.md" $ do
   route   $ pageRoute
   compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page-detail.html" defaultCtx
      >>= loadAndApplyTemplate "templates/default.html" defaultCtx
      >>= relativizeUrls
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

Metadata can be placed in the front matter of the markdown formatted as [YAML](https://yaml.org/). The metadata can be made available both to the compiler and to the template in case the `metadataField :: Context a`{haskell} is applied to the template.

~~~markdown
---
title: This is the blog title
tags: tag1, tag2
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

# Rolling your own

