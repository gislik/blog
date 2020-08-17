---
title: From HAppS to Happstack
author: Gísli Kristjánsson
id: 19
timestamp: 13.10.2009 00:13:47
filename: 2009-10-13-from-happs-to-happstack.html
summary: test
---

<p>I have been dreading porting my Blog application from <em>HAppS</em> to <a href="happstack.com"><em>Happstack</em></a> for a long time now. It is however apparent that <em>HAppS</em> is dying and <em>Happstack</em> (a fork of <em>HAppS</em> designed to take things further) was the only way to roll.</p>
<blockquote>
  <p>... a refreshingly innovative web application server written in Haskell. Leveraging the MACID state system, Happstack offers robust and scalable data access without the headache of managing a traditional RDBMS such as MySQL.</p></blockquote>
<p>The <a href="http://www.darcsweb.com:5003/r/mae/happstack-stable/snapshot/current/content/pretty/happstack/RELEASE_NOTES">RELEASE_NOTES</a> file had some guidelines for porting existing applications to <em>Happstack</em>. In my case it turned out to be really easy:</p>
<ol>
<li>Install Happstack  
<pre>sudo cabal install happstack</pre>
</li>
<li>Change HAppS to Happstack in all .hs-files           
  <pre>find . -type f -name '*.hs' -exec sed -i.bak 's/HAppS/Happstack/g' {} \;</pre>
</li>
<li>Change&nbsp;unServerPartT to&nbsp;runServerPartT           
  <pre>
find . -type f -name '*.hs' -exec sed -i.bak 's/unServerPartT/runServerPartT/g' {} \;
  </pre>
</li>
<li>Change all instances of <code>ServerPartT m a</code> to <code>ServerPartT</code> with <code>msum</code></li>
<li>Compile my code again</li>
</ol>
<p>Well this sounds almost too easy. And it is. I alway got the error <code>Unsupported socket</code> - both on my Mac (development machine) and on my Linux box (the production machine). On <a href="http://groups.google.com/group/HAppS/msg/0c9a0d0fd7c6aff0">Google Groups</a> I found someone saying that it's a problem with how <em>Haskell's Templating System</em> handles &nbsp;<em>IPv6. </em>The solution was to change <code>Happstack.Server.HTTP.Socket.acceptLite</code> to:</p>
<div>


~~~ haskell
-- | alternative implementation of accept to work around EAI_AGAIN errors 
acceptLite :: S.Socket -> IO (Handle, S.HostName, S.PortNumber) 
acceptLite sock = do 
  (sock', addr) <- S.accept sock 
  h <- S.socketToHandle sock' ReadWriteMode 
  (N.PortNumber p) <- N.socketPort sock' 

  let peer = case addr of 
               (S.SockAddrInet _ ha)      showHostAddress ha 
               (S.SockAddrInet6 _ _ ha _) showHostAddress6 ha 
               _                          error "Unsupported socket"
  return (h, peer, p) 
~~~

<p>Actually I used the time to cabalize my code. Outlining the dependencies means that <em><a href="http://www.haskell.org/cabal">Cabal</a></em> takes care of downloading all the necessary packages. To date my cabal file looks like this:</p>

    name:                gisliblog<br/>
    version:             0.0
    synopsis:            Personal website
    description:         
    category:            Web
    license:             BSD3
    license-file:        LICENSE
    author:              G&iacute;sli Kristj&aacute;nsson
    maintainer:          gislik hamstur.is
    build-depends:       base,feed,nano-md5,hscolour,json,curl,HStringTemplate,happstack
    build-type:          Simple
    hs-source-dirs:      src
    executable:          gisli.hamstur.is
    main-is:             src/Main.hs
    ghc-options:         -isrc        

<p>Also having everything in a <a href="http://git-scm.com/">git</a>&nbsp;repository meant that I could port the code in a special branch and merge the branches when the code compiled successfully. The next steps are to port my brothers photo site to <em>Happstack</em> and then look into taking the advantage of the new API which has been simplified in many ways. For example</p>
<p>This:</p>

~~~ haskell
uriRest :: Monad m => (String -> ServerPartT m a) -> ServerPartT m a
uriRest handle = withRequest $ \rq -> unServerPartT (handle (rqURL rq)) rq
~~~

<p>Becomes this:</p>

~~~ haskell
    uriRest :: (ServerMonad m, Monad m) => (String -> m a) -> m a
    uriRest handle = askRq >>= handle . rqURL
~~~

<p>That's it for now.</p>

P.S. On a different note I just placed an order for the [Arduino Duemilanove](/robotics/) microcontroller board. It should be arriving this week. I'm all excited so the next blog could be about that.



