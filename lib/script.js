google.load("feeds", 1);

function printRSSFeed(feedUrl, container, numFeeds, funcSuccess) {
    printFeed(feedUrl, numFeeds, function(result) {
        $.each(result.feed.entries, function(i, entry) {
            $("<li>").append($("<a>").attr("href", entry.link).append(entry.title)).append($("<li>").addClass("snippet").append($("<i>").append(entry.contentSnippet))).appendTo($(container));					
        });
        if (funcSuccess) {
            funcSuccess();
        }
    });
}

function printTwitterFeed(feedUrl, container, numFeeds, funcSuccess) {
    printFeed(feedUrl, numFeeds, function(result) {
        $.each(result.feed.entries, function(i, entry) {
            var len = "gislik: ".length;
            $("<li>").append($("<a>").attr("href", entry.link).append(entry.title.substring(len))).append($("<li>").addClass("snippet")).appendTo($(container));					
        });
        if (funcSuccess) {
            funcSuccess();
        }
    });
}

function printAlbumFeed(feedUrl, container) {
    printFeed(feedUrl, 100, function(result) {
        $(container).empty();
        $.each(result.feed.entries, function(i, entry) {
            $(container).append(entry.content);	
        });
    });
}

function printFeed(feedUrl, numFeeds, func) {
    var feed = new google.feeds.Feed(feedUrl);
    feed.setNumEntries(numFeeds);
    return feed.load(function(result) {
        if (!result.error) {
            func(result);
        } else {
            err = result.error
        return result.error
        }
    });
}

function bootstrapClick(t, callback) {
    $("a").removeClass("currentPage");
    $(t).addClass("currentPage");
    $("#heading").text($(t).attr("heading"));	
    var href = $(t).attr("href");
    return callback(href);
}

function proxyURI(uri) {
    if (uri.search(/http:\/\//) != -1) {
        uri = "/proxy/" + uri.substring(7);
}
return uri;
}

function baseHandler(t, dataHandler) {
    return bootstrapClick(t, function(href) {
        $.get(proxyURI(href), function(data) {
            dataHandler(data);
            $("a[heading]").each(href2url);
        });
        return false;
    });
}


function staticHandler() { 
    return baseHandler(this, function(data) {
        $("#content").html($("div.PubNote", data));
    });
}

function blogHandler() {
    return baseHandler(this, function(data) {
        $("#content").html(data.replace(/\n/, "").replace(/\n/g, "<br/>"));
    });
}

function albumHandler() {
    return bootstrapClick(this, function(href) {
        printAlbumFeed(href, "#content");
        return false;
    });
}

function mailHandler() {
    var emailEncoded = $(this).attr("href");
    var mailto = "mailto:";
    var isMailto = emailEncoded.indexOf(mailto) == -1 ? false : true;
    if (isMailto)
        emailEncoded = emailEncoded.substring(mailto.length);
    var exclLoc = emailEncoded.indexOf("!");
    if (exclLoc == -1) {
        return false;
    }
    var emailDecoded = emailEncoded.substring(exclLoc+1)+"@"+emailEncoded.substring(0, exclLoc);
    location.href = "mailto:"+emailDecoded;
    return false;
}

function pseudoHandler() {
    $("a").removeClass("currentPage");
    $(this).addClass("currentPage");
}

function loadWrapper(url) {
    $.get(url, function(data) {
        $("#content").html(data);
        $("a[heading]").each(href2url);
    });
}

function id2url(id) {
    if (typeof(id)!="string") {
        id = $(this).attr("id");
    }
    //	location.href = location.pathname+"?"+id;
    location.href = "/?"+id;
    return false;
}

function href2url() {
    var href = $(this).attr("href");
    var link = $("a[id=''][href='"+href+"']",$("#content"));
    if(link.attr("href")!="undefined") {
        var id = $(this).attr("id");
        link.click(function() {
            return id2url(id);
        });
    }
}
