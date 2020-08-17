---
title: baldurkristjans.is - screen cast, code and more
author: Gísli Kristjánsson
tags: haskell
id: 16
timestamp: 20.01.2009 19:14:32
filename: 2009-01-20-baldurkristjans.is-screen-cast-code-and-more.html
---

<p>As mentioned in my <a href="http://twitter.com/gislik/statuses/1121717423">tweet</a>&nbsp;my brother's online <a href="http://baldurkristjans.is">photo album</a> went live last Thursday. Since then I've been trying to think not so much about it because this project had started to consume my brain leaving little space for anything else (including <a href="http://conqueringsleepapnea.com/images/insomnia_2.jpg">sleeping</a>). I'm happy with the responses he's been getting over the last couple of days - and the most important thing; he's happy.</p>
<p>Readers of this blog know by now that this website was entirely written in Haskell/HAppS and I loved writing it. In this post I'll attempt to discuss some of the more interesting parts of the site and show you a <a href="http://www.screencast-o-matic.com/">screen cast</a> of the administrative panel.</p>
<div style="text-align: center">
<object width="600" height="450" data="../../video/player.swf" type="application/x-shockwave-flash">
<param name="wmode" value="opaque" />
<param name="flashvars" value="controlbar=over&amp;file=/img/baldurkristjans.is.flv&amp;image=/img/baldurkristjans.is.jpg&amp;location=/img/baldurkristjans.is.jpg" />
<param name="src" value="../../video/player.swf" />
</object>
</div>
<p>Because I'm using HAppS there is no need for a database. <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HAppS-State">HAppS-State</a> simply persist my native data structure. The photos are represented as maps of photos.</p>

~~~ haskell
type Photos = Map PhotoID Photo

type PhotoID = String

data Photo = Photo { photoName          :: String
                   , photoText          :: String
                   , photoDate          :: AppDate
                   , photoCategory      :: Maybe CategoryID
                   , photoCategoryCover :: Bool -- Calculated
                   , photoCover         :: Bool -- Calculated
                   , photoSeriesCover   :: Bool -- Calculated
                   , photoOrder         :: Int
                   , photoCategoryOrder :: Int
                   , photoSeriesOrder   :: Int -- Calculated
                   , photoCreated       :: AppDate
                   , photoModified      :: Maybe AppDate
                   } deriving (Show, Read, Eq, Data, Typeable)
~~~

<p>Some of the fields are have a `Calculated` comment. These fields do not depend on being persisted (this is of course not enforced by HAppS in any way) but they are rather calculated when needed and handed over to the templates where they control how the photo is rendered.</p>
<p>To warm up let's take a look at one of the simplest controller action (Controller.Photo.list):</p>

~~~ haskell
list :: WebT IO LayoutResponse
list = liftIO $ do
     env           <- appEnv :: IO (AppEnv String String)
     photos        <- query ListPhotos
     photo         <- liftM (pidToPhoto photos) $ query GetCoverPhoto
     photoTemplate <- liftM (attr "photo" photo) $ parseTemplate env "photos_show"
     returnLayout Nothing (attrSession "body" photoTemplate)
~~~

<p>Normally a controller action would have a type of `WebT IO Response` but I've created the `LayoutResponse` type to be able to wrap the master layout around the template (in this case `photos_show`). The variable <span style="font-style: italic;">photos</span> has the the `Photos` (a map of `Photo)` and `GetCoverPhoto ` returns the `PhotoID` (pid) of the cover photo which is then looked up with the function `pidToPhoto` in the photos map. The cover photo then gets assigned to the <span style="font-style: italic;">photo</span>&nbsp;template variable. When the photo template has been parsed it self gets assigned to the <span style="font-style: italic;">body</span>&nbsp;template variable which is returned from the controller action and handed to the `wrapLayout` function which purpose is to transform `LayoutResponse` to `Response`. This is a very common practice in my code.</p>
<p>One of the more interesting controller actions is the one serving the photos. When a <a href="http://baldurkristjans.is/photos/2009/h540/4d2c1cb884f327e9fc2adcccda4f97fd.jpg">photo</a> is requested the request gets handed to the `Photo ` controller. If the photo does exist it is served directly but if it does not exist it is resized according to the directory name it's contained in, i.e. `h540` (meaning fix the height at 540 pixels), provided that the requested size is allowed (appears in `sizes.txt`)</p>

~~~ haskell
legalSizes :: IO [String]
legalSizes = liftM lines (readFile "sizes.txt")

serveFile' :: FilePath -> [FilePath] -> FilePath -> ServerPartT IO Response
serveFile' orgsDir _ dir = let resize size f f' im = do
                                      (w, h) <- imageSize im
                                      let (fix, l) = splitAt 1 size
                                      let l' = read l :: Int
                                      let (w', h') = if fix == "w"
                                                        then (l', (l'*h)`div`w)
                                                        else ((l'*w)`div`h, l')
                                      let geometry = (P.show w')++"x"++(P.show h')
                                      status <- liftIO $ rawSystem "convert" 
                                                                      ["-geometry"
                                                                      ,geometry
                                                                      ,"-quality"
                                                                      ,(P.show quality)
                                                                      ,f
                                                                      ,f']
                                      case status of
                                           ExitFailure code -> fail.P.show $ code
                                           ExitSuccess      -> return ()                                           
                           in 
                           let resizePhoto size f f' = do
                                case map toLower (takeExtension f) of
                                     ".jpg" -> loadJpegFile f &gt;&gt;= resize size f f' 
                                     ".png" -> loadPngFile f &gt;&gt;= resize size f f'
                                     ".gif" -> loadGifFile f &gt;&gt;= resize size f f' 
                           in
       withRequest $ \req -> do
           currentDir <- liftIO getCurrentDirectory
           let (year:size:pid:_) = rqPaths req
           let file = joinPath [currentDir, orgsDir, year, pid]
           let file' = joinPath [currentDir, dir, drop 1.rqUri $ req]
           doesExist <- liftIO.doesFileExist $ file'
           sizes <- liftIO $ legalSizes
           let fixedRqPaths = map (drop 1).groupBy (\_ y -> y/='/') $ rqUri req
           let serveFile'' = do
               modifyResponse serveHeaders
               unServerPartT (fileServe [] dir) $ req { rqPaths=fixedRqPaths }
           if doesExist
              then serveFile''
              else if size `elem` sizes
                      then do
                           liftIO $ createDirectoryIfMissing True (takeDirectory file')
                           liftIO $ resizePhoto size file file'               
                           serveFile''
                      else noHandle
~~~

<p>As you can see I spawn&nbsp;<span style="font-style: italic;">convert </span>from <span style="font-style: italic;"><a href="http://www.imagemagick.org">ImageMagick</a></span> in a different process<span style="font-style: italic;">&nbsp;</span>to resize my photos. There are&nbsp;<a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hsmagick">bindings</a> to <span style="font-style: italic;">ImageMagick</span>&nbsp;&nbsp;available for <span style="font-style: italic;">Haskell </span>but was unable to install them. Initially I wanted to go for a much simpler option and use the&nbsp;<a href="http://libgd.org"><span style="font-style: italic;">GD library</span></a> which is a much simpler library and its <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/gd">bindings </a>seemed much simpler. The problem was however that the <span style="font-style: italic;">GD library </span>decreased the quality of the photo.</p>
<pre>GHCi, version 6.10.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt; :m +Graphics.GD 
Prelude Graphics.GD&gt; loadJpegFile "405ba7c1fc3aefcbe3cf4471ef1b781f.jpg"
<span style="white-space: pre;">        </span>&gt;&gt;= resizeImage 600 400&nbsp;
<span style="white-space: pre;">        </span>&gt;&gt;= saveJpegFile 100 "gd-405ba7c1fc3aefcbe3cf4471ef1b781f.jpg"
</pre>

<div style="text-align: center"><img src="/img/gd-405ba7c1fc3aefcbe3cf4471ef1b781f.jpg" alt="Converted with GD" width="600" height="400" /></div>
<div><br /></div>

~~~
$ mogrify -geometry 600x400 405ba7c1fc3aefcbe3cf4471ef1b781f.jpg im-405ba7c1fc3aefcbe3cf4471ef1b781f.jpg
~~~

<div style="text-align: center"><img src="/img/im-405ba7c1fc3aefcbe3cf4471ef1b781f.jpg" alt="Converted with ImageMagick" width="600" height="400" /><br /></div>
<p>The second is much richer in colors and identical to the source while the first one created with <span style="font-style: italic;">GD library&nbsp;</span>is brighter and generally lower in quality. This was a big surprise and a let-down but I'll just be using `ImageMagick` in the future. I wish someone would update the bindings though. For the purpose of creating small <a href="http://baldurkristjans.is/images/series/10/456300/colorful-reykjavik">images containing only text</a> the GD library seemed sufficient:<span style="color: #000000; font-family: Verdana; font-size: 10px; font-style: normal; line-height: normal; white-space: pre;"> </span></p>

~~~ haskell
newImage ((width rect)-1, height'+1) 
         >>= fillImage (rgb 0 0 0)
         >>= drawString "fonts/current.otf" size 0 (x,y) title 
                        (rgb (v r1 r2) (v g1 g2) (v b1 b2)) 
         >>= savePngFile filename
~~~

<p>Lastly I'd like to point out how one handles files submitted through HTML forms. The trick is to use <span style="font-style: italic;">inputFilename</span>&nbsp;and <span style="font-style: italic;">inputValue</span>&nbsp;like this:</p>

~~~ haskell
handleData = do
           photo  <- lookInput "photo"
           cid    <- look "cid" `mplus` return ""
           series <- look "series" `mplus` return ""
           let Just name = inputFilename photo `mplus` Just ""
           let contents = inputValue photo
           return (name, contents, cid, series)
~~~

<p>I hope this post is of use for someone and if you've some questions please leave a comment.</p>



