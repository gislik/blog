---
title: Model migration
author: Gísli Kristjánsson
tags: happstack
id: 17
timestamp: 05.05.2009 21:44:04
filename: 2009-05-05-model-migration.html
---

<p>My <a href="http://baldurkristjans.is">brother</a> has been asking me for a new feature for some time now. It's a simple video feature where instead of showing a picture when clicked on a video begins to play. I had been holding this project off for a while because I knew that model migration would probably be a hazzle.</p>
<blockquote>
<p>I can recommend the <a href="http://www.longtailvideo.com/">LongTail</a> FLV video player. It can handle FLV, MP4, MP3, AAC, JPG, PNG and GIF files. It also supports RTMP, HTTP, live streaming, various playlists formats, a wide range of settings and an extensive javascript API.</p>
</blockquote>
<p>What exactly is model migration? Well, when you model your data you come up with something like this:</p>

~~~ haskell
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

<p>When you start to use your model HAppS records your data into this structure in a binary form. Now let's say that you need to add a field for the name of the video to be played instead of showing the picture you get something like this:</p>

~~~ haskell
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
                   , photoVideo         :: Maybe String -- THE ONLY DIFFERENCE!
                   , photoCreated       :: AppDate
                   , photoModified      :: Maybe AppDate
                   } deriving (Show, Read, Eq, Data, Typeable)
~~~

<p>Notice that I've only added the <em>photoVideo</em> field. The problem is however that when HAppS tries to read in the old structure it does not know how to map it to the new structure (model). That's when you need to worry about model migration.</p>
<p>Fortunatelly it's super easy in HAppS. In my case the Photo model was in a module called <em>Model.Photo</em>. To migrate you just have to execute the following steps</p>
<ol>
<li>Copy the file <em>Photo.hs</em> to <em>Photo1.hs</em> and change the module name to <em>Model.Photo1</em></li>
<li>Add this to <em>Model.Photo</em>:<br />
<pre>
import qualified Model.Photo1 as P1
instance Migrate P1.Photo Photo where
         migrate p1 = Photo { photoName          = (P1.photoName p1)
                            , photoText          = (P1.photoText p1)
                            , photoDate          = (P1.photoDate p1)
                            , photoCategory      = (P1.photoCategory p1)
                            , photoCategoryCover = (P1.photoCategoryCover p1)
                            , photoCover         = (P1.photoCover p1)
                            , photoSeriesCover   = (P1.photoSeriesCover p1)
                            , photoOrder         = (P1.photoOrder p1)
                            , photoCategoryOrder = (P1.photoCategoryOrder p1)
                            , photoSeriesOrder   = (P1.photoSeriesOrder p1)
                            , photoVideo         = Nothing
                            , photoCreated       = (P1.photoCreated p1)
                            , photoModified      = (P1.photoModified p1)
                            } 

instance Version Photo where
         mode = extension 2 (Proxy :: Proxy P1.Photo)
</pre>

</li>
<li>You're all set. Compile and restart HAppS.</li>
</ol>
<p>When HAppS starts up again it knows exactly how to transform the old structure into the new one.</p>
<p>Happy migration!</p>



