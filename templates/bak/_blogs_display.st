<span$if(blog.private)$ class="private"$endif$>
<a name="$blog.id$"></a>
<h1>$blog.title$</h1> 
<h2><a class="mailto" href="hamstur.is!gislik">$blog.author$</a> þann $blog.date$ $blog.time$</h2>
<!-- <h2>+Blog, Haskell, HAppS</h2> -->
$if(user)$<h2><a href="/blog/edit/$blog.id$">Breyta</a></h2>$endif$
<div>
$blog.body$
</div>
</span>
