---
title: Network programming in Haskell
author: Gísli Kristjánsson
tags: haskell
id: 22
---

At work we use [Clojure](http://clojure.org) as our primary implementation language. I wrote a [blog post](http://appvise.me/2011/03/why-we-are-choosing-clojure-as-our-main-programming-language/) about the decision which got a lot of attention from the [Hacker News](http://news.ycombinator.com/item?id=2350381) community. While we are extremely happy with our decision we have started to deploy some [Haskell](http://www.haskell.org/) code on some low memory VPS we deploy.

Some of the machines we deploy are 256M and 512M instances and with multiple instances of the JVM the machines start swapping to disk sooner or later. The other day we needed a simple way to forward all HTTP traffic received by our DNS servers to our website. It seemed like the perfect project to introduce Haskell since the code is independent of everything else and extremely simple; just output a [HTTP 301 redirect](http://en.wikipedia.org/wiki/List_of_HTTP_status_codes#3xx_Redirection) response no matter what was received on the socket.

The resulting code is as close to a networking skeleton written in Haskell which I hope others will benefit from when starting a Haskell project involving network communication.

The following is the complete code and below I will step through some of the more interesting parts:

    module Main where

    {- A simple HTTP rebounder which sends a HTTP 301 response regardless of the request:
    
    HTTP/1.1 301 Moved Permanently
    Location: <url>
    Content-Length: 0
     
    -} 

    import Network              (PortID(PortNumber), withSocketsDo, listenOn, accept)
    import Network.Socket       (Socket, close)
    import Control.Concurrent   (forkIO)
    import Control.Applicative  ((<$>))
    import Control.Exception    (bracket)
    import System.Posix         (Handler(Ignore), installHandler, sigPIPE)
    import System.Environment   (getArgs)
    import Data.Maybe           (maybe, listToMaybe)
    import System.IO            (Handle, hPutStrLn, hFlush, hClose)

    -- configuration
    defaultPort = 8080
    defaultUrl  = "http://example.com/"

    -- main
    main :: IO ()
    main = withSocketsDo $ do
    installHandler sigPIPE Ignore Nothing
    url <- maybe defaultUrl id <$> listToMaybe <$> getArgs
    bracket 
        (listenOn $ PortNumber defaultPort)
        (close)
        (flip acceptConnection $ redirectConnection url)

    redirectConnection :: String -> Handle -> IO ()
    redirectConnection url h = hPutStrLn h (constructResponse url) >> hFlush h >> hClose h

    -- helpers
    constructResponse :: String -> String
    constructResponse url = unlines ["HTTP/1.1 301 Moved Permanently"
                                    ,"Location: " ++ url
                                    ,"Content-Length: 0"]

    acceptConnection :: Socket -> (Handle -> IO ()) -> IO ()
    acceptConnection socket handler = do
    (h,_,_) <- accept socket
        forkIO (handler h)
        acceptConnection socket handler


If you care about your code being able to run on Windows care must be taken to initialize the Windows network stack.

        main = withSocketsDo $ do

And POSIX based systems will send the [SIGPIPE](http://en.wikipedia.org/wiki/SIGPIPE#SIGPIPE) signal when trying to write to a closed socket. The default behaviour when a SIGPIPE is received is to terminate the program silently, which can be somewhat confusing if you haven't encountered this before. Although it is highly unlikely to happen in such a simple network server I am including it for future reference.

        installHandler sigPIPE Ignore Nothing

The [bracket](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception-Base.html#v:bracket) function is suitable when a computation needs to acquire a resource and release it after the computation.

        bracket 
            (listenOn $ PortNumber defaultPort)	-- acquire
            (close)					-- release
            (flip acceptConnection $ redirectConnection url)



The `acceptConnection` function begins by performing an IO action by calling [accept](http://hackage.haskell.org/packages/archive/network/latest/doc/html/Network.html#v:accept) which blocks the execution until a client connects to the socket at which time the action will return a 3-tuple containing a ([Handle](http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/GHC-IO-Handle.html#t:Handle), [HostName](http://hackage.haskell.org/packages/archive/network/latest/doc/html/Network.html#t:HostName), [PortNumber](http://hackage.haskell.org/packages/archive/network/latest/doc/html/Network.html#t:PortNumber)). In this simple case we neither need the host name nor the port number of the client so we will prevent them from being captured by using the underscore (\_).

        acceptConnection socket handler = do
        (h,_,_) <- accept socket
            forkIO (handler h)
            acceptConnection socket handler

One thing to note about the handle `h` returned from `accept` is block-buffered by default. For an interactive application you may want to set the buffering mode on the `Handle` to [LineBuffering](http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/GHC-IO-Handle.html#v:LineBuffering) or [NoBuffering](http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/GHC-IO-Handle.html#v:NoBuffering), like so:

        hSetBuffering h LineBuffering


The request is handled in a new thread for concurrency. It's worth mentioning that Haskell forks so-called [green threads](http://en.wikipedia.org/wiki/Green_threads) which are much lighter than normal OS threads and forking [hundreds of thousands](http://stackoverflow.com/questions/5847642/haskell-lightweight-threads-overhead-and-use-on-multicores) or even [millions](http://stackoverflow.com/questions/1900165/how-long-does-it-take-to-create-1-million-threads-in-haskell) is very doable. This way we don't need to be concerned with [thread pools](http://en.wikipedia.org/wiki/Thread_pool_pattern) even for highly concurrent servers.

With recursion at the last line of `acceptConnection` we make sure the server continues to serve new clients after serving the first one.

I always find it best to compile multi-threaded Haskell code with the (-threaded) command parameter which makes sure that if my code is linked to a C code with foreign function interface you don't lose concurrency.

        $ ghc -O2 -threaded --make network-server

By using Haskell you get multi-core support for free, so if you want to distribute the request handling over the cores in your machine you simply execute the server like so.  Substitute `x` in `-Nx` with the number of cores you want to use.


        $ ./network-server +RTS -Nx

As can be seen from the above Haskell is a worthy tool in the concurrent developer's toolbox and like Clojure it features many paradigms like [Software Transactional Memory](http://www.haskell.org/haskellwiki/Software_transactional_memory) which help the programmer in the battle with multi-threaded, concurrent programs.
