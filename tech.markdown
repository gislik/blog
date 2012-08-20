---
title: The tech driving this blog
---

This blog was originally created to capture my journey into the world of Haskell. As such it made perfect sense to take the first baby steps by creating a blog in Haskell.

My first attempt was to develop the blog using HAppS later migrating to Happstack. Happstack is known for its type-safe noSQL database which was used for the blog posts themselves which were editable with a simple CRUD administration interface. Instead of creating yet another CMS system for the other parts of the website content was created, edited and stored in Google Notebook. Content from Google Notebook was embedded on this website by requesting the XML version from Google via JavaScript and replace a div-tag in the HTML. This worked amazingly well until Google decided to discontinue Notebook in 2011. 

This meant that a new strategy had to be devised. Since not many hosting services allow you to run Haskell web sites and full blown VPS is XXX a simpler strategy was needed. GitHub offers free static hosting and with Hakyll, the static web generator library for Haskell, blog can be written locally with your editor of choice and pushed to GitHub for publishing. As a bonus all the code and content for your website are automatically version controlled in a Git repository.

---



  <span id="content"><p>Eins og fram kemur á <a href="./blog/show/1">blogginu</a> mínu ætla ég enn fremur að nota þetta sem umhverfi til þess að læra nýtt <a href="http://haskell.org">forritunarmál</a>. Það sem hefur nýst mér hvað best til þess að læra þetta nýja mál er <em>Haskell</em>-kóði frá öðrum. Því hef ég ákveðið að <a href="./src/">kóðinn minn</a> verði aðgengilegur öllum þeim sem vilja skoða. Áhugasamir geta líka með <a href="./blog/"><em>Haskell</em></a>-ævintýrinu ] mínu og hvaða <a href="http://www.google.com/notebook/public/01738234982158720746/BDSe5QgoQ0NzJ8vsi">aðferðir</a> ég kem til með að nota.</p>
<blockquote>
<em>Haskell</em> er latt, hreint, fallaforritunarmál sem gerir það að flestu leyti einstakt. Fyrir vikið verður nálgunin mun stærðfræðilegri, áhugaverðari og meira abstrakt.
</blockquote>


Vefurinn er knúinn áfram af HAppS, sem er forritaþjónn (e. Application server) skrifaður í Haskell, sem keyrir á Linux-þjóni. Hægt er að skoða Haskell-kóðann á bak við þessa síðu hér. 

Hluti virkninnar er í HTML-laginu en þar notast ég við hið frábæra JavaScript-safn, jQuery. Helst vil ég sleppa því að búa til stórt vefkerfi þannig að ég reyni að notast við Web 2.0-vefi, sem veita þá þjónustu sem ég vil hafa á þessum vef, og flytja gögnin yfir Atom-straum hingað á þennan vef. Þannig nota ég:

Google Notebook (statískar vefsíður), Google Picasaweb (til þess að setja myndir á vefinn, sem ég sendi beint úr iPhoto), Google Reader (til þess að deila áhugaverðu efni með ykkur), Twitter (til þess að deila því sem ég er að bralla með ykkur), FeedBurner  (til þess að RSS-væða bloggið), JS-Kit (til þess að leyfa athugasemdir á blogginu), Dropbox (til þess að deila skrám með ykkur), Twitterfeed (til þess að fæða blogginu inn á Twitter og Facebook) og RPX (OpenID stuðningur)

Með tímanum, eftir því sem sjálfstraust mitt í Haskell og HAppS eykst, mun ég eflaust flytja hluta virkninnar aftur yfir á þjóninn. Þannig er bloggið nú skrifað í Haskell og til þess að breyta blogginu nota ég TinyMCE, sem er WYSIWYG ritill.

Þessi vefur er einskonar eilífðarverkefni svo líklegt er að hann komi til með að breytast, bæði í útliti, virkni og þeim aðferðum sem hér er lýst. Þess vegna vil ég frekar varpa efninu fram á einfaldan hátt, til að byrja með, í stað þess að láta það alveg sitja á hakanum vegna þess að ég er ekki ánægður með framsetninguna.

