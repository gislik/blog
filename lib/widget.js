if ((typeof RPXNOW == "undefined") || !RPXNOW) {
    var RPXNOW = {
        'loaded': false,
        'show': function() {
            RPXNOW.show_on_load = true;
        },
        'always_open': false,
        'overlay': false,
        'language_preference': null,
        'default_provider': null,
        'lso_submit_action': null,
        'token_url': null,
        'realm': null,
        'domain': null,
        'flags': null
    };
}

(function () {
    token_url_regex = /^https?:\/\/([a-z0-9]([-a-z0-9]*[a-z0-9])?\.)*[a-z0-9]([-a-z0-9]*[a-z0-9])?(:[0-9]+)?\/[^?#]*(\?[^#]*)?$/i
    lso_submit_action_regex = /^https:\/\/([a-z0-9]([-a-z0-9]*[a-z0-9])?\.)*[a-z0-9]([-a-z0-9]*[a-z0-9])?(:[0-9]+)?\/[^?#]*(\?[^#]*)?$/i
    
    function log(msg) {
        if (window.console) {window.console.log('RPXNow: ' + msg);}
    }

    function detectPlatform() {
        var o={
            ie:0,
            opera:0,
            gecko:0,
            webkit: 0,
            mobile: null,
            air: 0
        };
        var ua=navigator.userAgent, m;

        // Modern KHTML browsers should qualify as Safari X-Grade
        if ((/KHTML/).test(ua)) {
            webkit=1;
        }
        // Modern WebKit browsers are at least X-Grade
        m=ua.match(/AppleWebKit\/([^\s]*)/);
        if (m&&m[1]) {
            o.webkit=parseFloat(m[1]);

            // Mobile browser check
            if (/ Mobile\//.test(ua)) {
                o.mobile = "Apple"; // iPhone or iPod Touch
            } else {
                m=ua.match(/NokiaN[^\/]*/);
                if (m) {
                    o.mobile = m[0]; // Nokia N-series, ex: NokiaN95
                }
            }

            m=ua.match(/AdobeAIR\/([^\s]*)/);
            if (m) {
                o.air = m[0]; // Adobe AIR 1.0 or better
            }

        }

        if (!o.webkit) { // not webkit
            // @todo check Opera/8.01 (J2ME/MIDP; Opera Mini/2.0.4509/1316; fi; U; ssr)
            m=ua.match(/Opera[\s\/]([^\s]*)/);
            if (m&&m[1]) {
                o.opera=parseFloat(m[1]);
                m=ua.match(/Opera Mini[^;]*/);
                if (m) {
                    o.mobile = m[0]; // ex: Opera Mini/2.0.4509/1316
                }
            } else { // not opera or webkit
                m=ua.match(/MSIE\s([^;]*)/);
                if (m&&m[1]) {
                    o.ie=parseFloat(m[1]);
                } else { // not opera, webkit, or ie
                    m=ua.match(/Gecko\/([^\s]*)/);
                    if (m) {
                        o.gecko=1; // Gecko detected, look for revision
                        m=ua.match(/rv:([^\s\)]*)/);
                        if (m&&m[1]) {
                            o.gecko=parseFloat(m[1]);
                        }
                    }
                }
            }
        }

        return o;
    }

    var platform = detectPlatform();

    function getViewportHeight() {
        var height = self.innerHeight; // Safari, Opera
        var mode = document.compatMode;
        
        if ( (mode || isIE) && !platform.opera ) { // IE, Gecko
            height = (mode == 'CSS1Compat') ?
                document.documentElement.clientHeight : // Standards
                document.body.clientHeight; // Quirks
        }

        return height;
    }
        
    function getViewportWidth() {
        var width = self.innerWidth;  // Safari
        var mode = document.compatMode;
            
        if (mode || isIE) { // IE, Gecko, Opera
            width = (mode == 'CSS1Compat') ?
                document.documentElement.clientWidth : // Standards
                document.body.clientWidth; // Quirks
        }
        return width;
    }

    function getDocumentHeight() {
        var scrollHeight = (document.compatMode != 'CSS1Compat') ? document.body.scrollHeight : document.documentElement.scrollHeight;
        var h = Math.max(scrollHeight, getViewportHeight());
        return h;
    }
        
    function getDocumentWidth() {
        var scrollWidth = (document.compatMode != 'CSS1Compat') ? document.body.scrollWidth : document.documentElement.scrollWidth;
        var w = Math.max(scrollWidth, getViewportWidth());
        return w;
    }

    function append_close_link(container, text) {
        var closelink = document.createElement("div");
        closelink.appendChild(document.createTextNode(text));

        s = closelink.style;
        s.color = "#fff";
        //s.fontWeight = "bold";
        s.fontSize = "13px";
        s.fontFamily = "arial, sans-serif";
        s.textAlign = "right";
        s.height = "16px";
        s.cursor = "pointer";
        s.position = "absolute";
        s.right = "0px";

        container.appendChild(closelink);
    }

    function gen_signin_url() {
        var token_url = RPXNOW.token_url;
        if (!token_url) {
            console.log("Error - RPXNOW.token_url is undefined.");
        } else if (!token_url_regex.test(token_url)) {
            console.log("Error - RPXNOW.token_url must be an absolute URL with no fragment.");
        }

        var lso_submit_action = null;
        if (RPXNOW.lso_submit_action) {
            if (lso_submit_action_regex.test(RPXNOW.lso_submit_action)) {
                lso_submit_action = RPXNOW.lso_submit_action;
            } else {
                console.log("Error - RPXNOW.lso_submit_action must be an absolute HTTPS URL with no fragment.");
            }
        }

        var host = null;
        var rp_id = null;

        

        if (RPXNOW.domain) {
            host = RPXNOW.domain;
        } else if (RPXNOW.realm) {
            if (RPXNOW.realm.match(/\./)) {
                host = RPXNOW.realm;
            } else {
                host = RPXNOW.realm + "." + "rpxnow.com";
            }
        } else {
            host = "rpxnow.com";
            rp_id = RPXNOW.rp_id;
        }

        var signin_url = null;
        if (lso_submit_action) {
            signin_url = "https://" + host + "/openid/lso_popup?token_url=" + encodeURIComponent(token_url) + "&lso_submit_action=" + encodeURIComponent(lso_submit_action);
        } else {
            signin_url = "https://" + host + "/openid/popup?token_url=" + encodeURIComponent(token_url);
        }

        if (rp_id) {
            signin_url += "&rp_id=" + encodeURIComponent(rp_id);
        }

        var optional_fields = ['user_identifier', 'email', 'language_preference', 'default_provider', 'flags'];

        var ofi = 0;
        for (ofi = 0; ofi < optional_fields.length; ofi++) {
            var fieldname = optional_fields[ofi];
            if (RPXNOW[fieldname]) {
                signin_url += '&' + fieldname + '=' + encodeURIComponent(RPXNOW[fieldname]);
            }
        }

        return signin_url;
    }

    function gen_popup() {
        var IE6 = false /*@cc_on || @_jscript_version <= 5.6 @*/;

        function LoginBox() {
            var s = null;

            var ifrm = this.ifrm = document.createElement('iframe');
            ifrm.frameBorder = 0; ifrm.scrolling = 'no';
            ifrm.src = "javascript:'<html style=\"margin: 0px; padding: 0px;\"><body style=\"margin: 0px; padding: 0px;background-color: #888; \"><h3 style=\"font-family: sans-serif; margin: 0px; padding: 0.65em; *padding: 0.45em; color: #fff; \">Loading...</h3></body></html>'";

            s = ifrm.style;
            s.width = '400px'; s.height = '40px'; s.margin = '0px';
            s.padding = '0px'; s.border = '0px';

            this.container = document.createElement("div");
            this.container.style.position = "relative";
            this.container.style.height = "40px";
            if (!RPXNOW.always_open) {
                append_close_link(this.container, "X");
            }
            this.container.appendChild(ifrm);

            /* top border */
            var topLeftTd = document.createElement("td");
            s = topLeftTd.style;
            if (IE6) {s.behavior = "url(https:\/\/rpxnow.com\/stylesheets\/iepngfix.htc)";}
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_top_left.png)";
            s.width = "10px"; s.height = "10px"; s.padding = "0px"; s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.color ="#FFF";

            var topMiddleTd = document.createElement("td");
            s = topMiddleTd.style;
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_border.png)";
            s.height = "10px"; s.padding = "0px"; s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";

            s.color ="#FFF";

            var topRightTd = document.createElement("td");
            s = topRightTd.style;
            if (IE6) {s.behavior = "url(https:\/\/rpxnow.com\/stylesheets\/iepngfix.htc)";}
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_top_right.png)";
            s.width = "10px"; s.height = "10px"; s.padding = "0px";
            s.margin = "0px"; s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var topTr = document.createElement("tr");
            topTr.appendChild(topLeftTd);
            topTr.appendChild(topMiddleTd);
            topTr.appendChild(topRightTd);

            /* middle border */
            var middleLeftTd = document.createElement("td");
            s = middleLeftTd.style;
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_border.png)";
            s.width = "10px"; s.padding = "0px"; s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var middleMiddleTd = document.createElement("td");
            s = middleMiddleTd.style;
            s.backgroundColor = "#FFF";
            s.padding = "0px";
            s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";
            middleMiddleTd.appendChild(this.container);

            var middleRightTd = document.createElement("td");
            s = middleRightTd.style;
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_border.png)";
            s.width = "10px"; s.padding = "0px"; s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var middleTr = document.createElement("tr");
            middleTr.appendChild(middleLeftTd);
            middleTr.appendChild(middleMiddleTd);
            middleTr.appendChild(middleRightTd);


            /* bottom border */
            var bottomLeftTd = document.createElement("td");
            s = bottomLeftTd.style;
            if (IE6) {s.behavior = "url(https:\/\/rpxnow.com\/stylesheets\/iepngfix.htc)";}
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_bottom_left.png)";
            s.width = "10px"; s.height = "10px"; s.padding = "0px";
            s.margin = "0px"; s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var bottomMiddleTd = document.createElement("td");
            s = bottomMiddleTd.style;
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_border.png)";
            s.height = "10px"; s.padding = "0px"; s.margin = "0px";
            s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var bottomRightTd = document.createElement("td");
            s = bottomRightTd.style;
            if (IE6) {s.behavior = "url(https:\/\/rpxnow.com\/stylesheets\/iepngfix.htc)";}
            s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/pop_dialog_bottom_right.png)";
            s.width = "10px"; s.height = "10px"; s.padding = "0px";
            s.margin = "0px"; s.border = "0px";
            s.borderCollapse = "collapse";
            s.borderSpacing = "0";
            s.borderColor ="#FFF";
            s.color ="#FFF";

            var bottomTr = document.createElement("tr");
            bottomTr.appendChild(bottomLeftTd);
            bottomTr.appendChild(bottomMiddleTd);
            bottomTr.appendChild(bottomRightTd);


            this.tbody = document.createElement("tbody");
            this.tbody.appendChild(topTr);
            this.tbody.appendChild(middleTr);
            this.tbody.appendChild(bottomTr);

            this.table = document.createElement("table");
            s = this.table.style;
            s.borderCollapse = "collapse";
            s.margin = "auto";
            this.table.appendChild(this.tbody);

            this.outer = document.createElement("div");
            s = this.outer.style;
            s.zIndex = 10000;

            var fromTop = 125 + "px";
            s.position = "absolute"; s.overflow = "visible";
            s.width = "100%"; s.display = "none";s.textAlign = "center";
            s.top = "0px"; s.left = "0px";
            s.height = getDocumentHeight() + "px";
            s.width = getDocumentWidth() + "px";

            s.paddingTop = fromTop;
            if (RPXNOW.overlay) {
                s.backgroundImage = "url(https:\/\/rpxnow.com\/images\/overlay.png)";
                if (IE6) {
                    s.behavior = "url(https:\/\/rpxnow.com\/stylesheets\/iepngfix.htc)";
                }
            }

            this.outer.appendChild(this.table);

            if (document.body.firstChild) {
                document.body.insertBefore(this.outer,document.body.firstChild);
            } else {
                document.body.appendChild(this.outer);
            }

            var outer = this.outer;
        }

        _ = LoginBox.prototype = {}
        _.show = function () {
            var quirksMode = document.compatMode != 'CSS1Compat';

            var outer = this.outer;
            var ifrm = this.ifrm;
            var container = this.container;

            function grow(i, fin_height) {
                if (RPXNOW.show_on_load || RPXNOW.always_open) {
                    container.style.height = ifrm.style.height = fin_height + 'px';
                } else {
                    container.style.height = ifrm.style.height = i + 'px';
                    if (i < fin_height) {
                        setTimeout(function() {
                                grow(i + 20, fin_height);
                            }, 3);
                    }
                }
            }

            function fireShow() {
                outer.style.display = 'block';
            }

            setTimeout(fireShow, 1);
            if (!RPXNOW.loaded) {
                RPXNOW.loaded = true;
                setTimeout(function() {
                        var url = gen_signin_url();
                        if (!url) {
                            return true;
                        }
                        ifrm.contentWindow.location.replace(url);

                        var fin_height = (RPXNOW.lso_submit_action && lso_submit_action_regex.test(RPXNOW.lso_submit_action)) ? 320 : 260;
                        grow(40, fin_height);
                    }, 1);
            }


            return false;
        }

        _.hide = function () {
            this.outer.style.display = "none";
        }

        _.resize = function () {
            var outer = this.outer;
            outer.style.height = getViewportHeight() + "px";
            outer.style.width = getViewportWidth() + "px";
            
            setTimeout(function () {
                    outer.style.height = getDocumentHeight() + "px";
                    outer.style.width = getDocumentWidth() + "px";
            }, 1);
        }

        var login_box = new LoginBox();
        RPXNOW.show = function () {
            login_box.show();
        }

        var arrElements = document.getElementsByTagName("a");
        var oRegExp = new RegExp("(^|\\s)rpxnow(\\s|$)");
        var oElement;
        for(var i=0; i<arrElements.length; i++) {
            oElement = arrElements[i];
            if(oRegExp.test(oElement.className)) {
                oElement.onclick = function () {
                    return login_box.show();
                }
            }
        }

        if (!RPXNOW.always_open) {
            var close_hook = function() { login_box.hide(); };

            if (window.ActiveXObject) {
                document.body.parentNode.attachEvent('onclick', close_hook);
            } else {
                document.body.parentNode.addEventListener('click',
                                                          close_hook, false);
            }
        }

        if (RPXNOW.show_on_load || RPXNOW.always_open) {
            login_box.show();
        }

        var oldOnResize = window.onresize;
        window.onresize = function(evt) {
            try {
                login_box.resize();
            } finally {
                return oldOnResize && oldOnResize(evt);
            }
        }
    }

    var oldOnLoad = window.onload;
    window.onload = function(evt) {
        try {
            gen_popup();
        } finally {
            return oldOnLoad && oldOnLoad(evt);
        }
    }

})();
