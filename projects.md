---
title: Projects
---

Þessi síða er í vinnslu en partur af nýju stefnunni minni er að koma efni frá mér oftar og hraðar þrátt fyrir að það þýði ófullkominn frágang til þess að byrja með. Ég hef nefnilega bæði séð það hjá öðrum og fundið það hjá sjálfum mér að þegar fullkomnunaráráttan nær yfirhöndinni skilar vinnan oft engu sjáanlegu í mjög langan tíma. Þetta þýðir óneitanlega að síðan á eftir að breytast en eins og staðan er núna er þetta eingöngu formaður texti. Myndum og ítarlegri lýsingu verður bætt við. Röð, fyrirsagnir og framsetningin í heild sinni á eftir að breytast en einhvers staðar verður maður að byrja.

Hugmyndin að þessum hluta er að halda utan um það sem ég hef gert. Á næstu vikum (mánuðum) ætla ég að reyna að grafa upp upplýsingar um eftirfarandi verkefni/hugmyndir. Þær eru allt frá forritunarverkefni yfir í ritgerðir og stuttmyndir.


### Flotavakt RM

1. Inngangur
Flotavakt Radiomiðunar sá um að vakta staðsetningu og innihalda skipa fyrir útgerðarfélög. Þannig gátu útgerðarfélögin verið meðvituð um stöðu og kostnað við rekstur þeirra.

2. Tæknilýsing
Staðsetningargögnum, sem og öðrum gögnum, var safnað í INmobil-samskiptastöðinni. Gögnin voru áframsend um gervihnetti í land þar sem þeim var komið fyrir í gagnagrunnum. Gögnunum var síðan breytt í upplýsingar með því að varpa þeim á heimskort þar sem útgerðarmönnum var gert kleyft að þysja inn og út færa kortið og kalla fram birgðastöðu skipa.



### Þjónustubanki RM

1. Inngangur
Þjónustubanki Radiomiðunar hefur það hlutverk að þjónusta sjávarútveginn með fersku efni frétta og afþreyingar. Skip á úthöfum eru háð samskiptum um gervihnetti sem enn eru dýr og hægvirk. Radiomiðun hóf því þjónustu sem stuðlaði að betri nýtingu þeirrar bandvíddar sem til staðar er.

Þjónustuan virkar þannig að, í stað þess að skip séu að sóa dýrum tíma í að flakka um veradarvefinn, eru þeim sendar þær fréttir og sú afþreying sem skipverjar óska sér. Það sem gerði Þjónustubanka RM frábrugðinn öðrum þjónustum er að um borð er netþjónn, INmobil, uppsettur sem sér um að deila upplýsingunum með áhöfnunni án frekari samskiptakostnaðar.

2. Tæknilýsing
Þjónustan skiptist í 3 megin liði:
Söfnun
Gögnum er safnað frá samstarfsvefjum. Þessum hluta má líkja við svo kallað vefköngulær sem flakka um vefinn og safna nýjustu upplýsingum. Þessar upplýsingar voru ýmist á XML-formi, HTML-formi eða jafnvel á óstöðluðu formi.
Pöntun
Skipverjar óskuðu eftir efni í gefnum vefsíðu og gátu breytt áskrift sinni og tíðni sendinga í hvert skipi sem þeir voru í landi.
Sending
Þjöppuð skeyti voru send um borð með þeim upplýsingum sem viðkomandi áhöfn hafði pantað sér og Birtir sá um að birta upplýsingarnar smekklega.

2.1 Söfnun
Þjónustuna keyrði á Linux-þjóninum, ulfur.radiomidun.is, Snerpu á Ísafirði. Úr Cron-þjónustu spurði Perl-skrifta alla samstarfsvefi um uppfærslur og geymdi viðbæturnar í gagnagrunni. Mismunandi uppfærslu tíðni var á þjónustunum eftir vægi gagnanna.

2.2 Pöntun
Skipverjar höfðu möguleika á því að panta sér, og gera breytingar á pöntun sinni á, fréttum og afþreyinguí gegnum einfalt vefviðmót (sjá mynd). Skipverjar höfðu möguleika á því að panta sér gögnin og ákvarða hvaða daga vikunnar og á hvaða tíma dagsins þeir vildu fá gögnin send.

2.3 Sending
Skeytin voru send yfir UUCP yfir Iridium-gervihnettina. Þegar  skeytin komu um borð tók INmobil þjónninn við þeim og sáu til þess að vista þau í miðlægum gagnagrunni skipsins. Með þeim hætti gáti allir skipverjar notið góðs af og sparað samskiptakostnað. Birtir sá svo um að veiða gögnin upp úr gangagrunnum skipsins til birtingar fyrir skipverja.




### Iridum RM

1. Inngangur
Bylting varð í þjónustu bið útgerðarfélögin þegar Radiomiðun hóf að gera reikninga sýnilega á netinu. Radiomiðun varð þar með fyrsta símafélagið (fjarskiptafélagið) til þess að gera útgerðarfélögum kleyft að
hafa yfirsýn með samskiptakostnaði skipa sinna. Hægt var að fá yfirlit, sundurliðun reikninga niður á skip, skipverja, daga, o.s.frv.

2. Tæknilýsing
Gögn bárust frá Iridium að utan á sérformi. Þau gögn voru móttekin af Perl-skriftum sem sáu um að koma þeim í gagnagrunna. Þaðan var síðan hægt að veita aðgang að þeim á netinu fyrir útgerðarfélögin.




### Útvarp Verzló

1. Inngangur
Nemendafélag Verzlunarskóla Íslands (NFVÍ) starfrækir á hverju ári útvarpsstöð samfara kostningarvikunni. Á daginn eru ýmist frambjóðendaþættir eða skemmtiþættir fyrir nemendur. Á kvöldin og nóttunni voru svo dagskráin á enda og því spiluð tónlist af handahófi.

NFVÍ hefur ávallt verið framsækið og þegar ég var í 5. bekk datt mér í hug að stofna til fyrsta gagnvirka útvarps Íslands. Þetta fór þannig fram að notendur gátu skráð sinn inn á vef útvarpsins og kosið um lög sem spila átti. Þannig höfðu hlustendur beint áhrif á það sem spilað var.

Fjallað var um okkur í fréttum Morgunblaðsins þar sem fyrsta gagnvirka útvarpsstöðin hóf göngur sínar.

2. Tæknilýsing
Öll lög voru geymd á MP3-formi á þjónum útvarpsstöðvarinnar. Upplýsingar um lögin voru geymd miðlæg í gangagrunni. Notendur síðunnar höfðu sínðan áhrif á spilunarlistann með því að kjósa sitt lag. OpenSource-spilari spilaði lögin en við skrifuðum vefju (e. wrapper) sem stýrði því hvaða lag var næst.




### Margmiðlunarspilari með snertiskjá

Pabbi hefur alltaf verið græjukarl og þess vegna hefur alltaf verið auðvelt fyrir mig að plata hann í allskonar vitleysu. Við höfum brallað ýmislegt á heimilinu; brotið upp veggi til þess að koma skjám fyrir í veggjum og fleira.

Á tímabili starfræktum við þjón fyirr heimilið enda voru 8 tölvur starfræktar þar og umsjónin í mínum höndum. Við settum upp miðlægan þjón sem geymdi gögn, hýsti vefi okkar, póst-, nafna-, mynda- og tónlistarþjón. Nokkur vinna var lögð í að lesa geisladiska á stafrænt form inn á þjóninn. Á þennan hátt gátu allar tölvur heimilisins bæði náð í gögn deilt þeim. Sjálfvirkt afrit út í bæ gerði okkur örugg um að gögnin myndu ekki tapast.

Í tilraunskyni settum við upp margmiðlunarspilara, sem var þunnbiðlari (e. thin client). Í honum var enginn harður diskur en hann ræsti sig upp af miðlæga þjóninum og kom sér í fjarrænan gluggaham. Í gegnum þessa tölvu var hægt að spila tónlistina, sem geymd var á þjóninum, í aðalhátalarakerfi hússins.

Fjallað var um lausnina í fréttum Morgunblaðsins.




### Skúli

 Vinahópurinn hefur kennt sig við Skúla allt frá því að nokkrir einstaklingar úr honum tóku á leigu atvinnuhúsnæði við Skúlagötu í Reykjavík. Þetta húsnæði var nýtt í partýhald um helgar og varð hópnum því mjög kært. Sett var upp blogg fyrir hópinn sem notað var í allt frá því að skrifa nýjustu dagrenningarnar yfir í að bjóða í veislur og kynna atburði - og síðast en ekki síst að leyfa Skúlum erlendis að fylgjast með.

Í nokkur ár lentum við í tæknilegum hremmingum og tímaleysi sem ollu því að vefurinn lagðist niður. Hins vegar er það núna nýverið sem hugmyndir hafa kviknað um að endurvekja bloggið og hefur nýr og endurbættur vefur verið skrifaður í Ruby on Rails - sérsniðinn fyrir Skúlana. Vefurinn er hýstur á Butler í húsakynnum Kapital.




### Loki

Í HÍ tók ég m.a. kúrsinn Þýðendur, sem dr. Snorri Agnarsson kenndi, og ég lærði mikið af. Lokaverkefni kúrsins var að smíða þýðanda fyrir okkar eigið forritunarmál. Snorri samdi íslenska forritunarmálið Fjölni fyrir mörgum árum og okkar forritunarmál var hlutmengi í Fjölni. Mitt mál fékk nafnið Loki (með vísan í goðafræðina eins og Fjölnir og einnig með vísan í að þetta var lokaverkefnið).

Stór hluti verkefnisins gekk út á að skrifa handbók fyrir þýðandann.




### Vindmælir

Í HÍ báðu vinir mínir (Andri Guðmundsson og Gunnar Sigvaldi) mig um að aðstoða þá við að forrita vindmæli sem þeir áttu að smíða í fagi sem þeir voru í. Vindmælirinn átti að geta mælt vindhraða og vindstefnu. Lausn þeirra fólst í að smíða þriggja arma vindskeið sem snérist. Á einn arminn festu þeir segul og gátu því framkallað púls fyrir hvern heilan hring sem skeiðin snerist. Áfastur var líka stefnuör með segli og hægt var að reikna stefnuna út frá tímanum sem það tók vindskeiðina að snúast.

Við bjuggum til prótótýpu af forritinu á einni nóttu og lagfærðum síðan og stilltum reikningana næstu daga. Forritið sem keyrði mælinn hélt líka utan um mælingar í gagnagrunni svo hægt var að varpa niðurstöðunum á netið, en það var ein af kröfum verkefnis strákanna. Forritið var skrifað í C og spannaði um 500 línur með samskiptamódúlnum við gagnagrunninn.




### Banach-Tarski þversögnin

Eitt af skyldufögunum í stærðfræði í HÍ er samæfingar. Tilgangurinn er að rannsaka eitthvert stærfræðifyrirbæri og kynna það fyrir samnemendum. Við Anna Hrund Másdóttir og Grímur Hjörleifsson fjölluðum um Banach-Tarski þversögnina:

Árið 1924 sýndu þeir Banach og Tarski fram á að hægt sé að skipta kúlu upp í (endanlega marga) hluta og raða þeim saman aftur þannig að út komi tvær kúlur sem hvor um sig er nákvæm eftirmynd upprunalegu kúlunnar. Þetta hyggjumst við sanna hér. Í ljós kemur reyndar að þetta gildir ekki einungis um kúlur, heldur um öll mengi sem ekki hafa tómt innmengi.

Upphaflegur tilgangur þeirra félaga með því að setja fram þessa fráleitu niðurstöðu var sá að sýna fram á að valfrumsendan væri röng.

Það mistókst, stærðfræðingar eru enn almennt ekki sannfærðir.

Á einum stað í útleiðslunni myndum við fulltrúamengi, þ.e. við höfum gefna fjölskyldu af mengjum og myndum fulltrúamengi fyrir þá fjölskyldu með því að taka eitt stak úr hverju mengi í fjölskyldunni. Þetta er nauðsynlegt fyrir sönnun þessarar setningar og hægt ef við gefum okkur að valfrumsendan gildi. Því gerum við það.

Ritgerðina má nálgast hér.




### KOX

Ég hef í raun búið til tvö kerfi undir þessu nafni en nafnið er dregið af kosningum NFVÍ en nafn þeirra er oft stytt á þennan hátt.

NFVÍ
Eitt af síðustu verkum mínum sem forseti NFVÍ var að sjá um kosningar félagsins. Til þess að fólk geti gefið kost á sér í embætti þarf það að sýna fram á að það hafi lágmarksstuðning með því að skila inn meðmælendalistum. Það gefur auga leið að meðmælandi má eingöngu vera á lista hjá einum frambjóðanda í hvert embætti en það er næsta ómögulegt að ganga úr skugga um það í höndunum - enda mjög sjaldgæft meðmælandi sé strikaður út af þessarri ástæðu.

Ég skrifaði vefforrit í Mason þar sem kjörstjórnarmenn gátu parað frambjóðendur og meðmælendur og á yfirlitssíðu var hægt að sjá þá meðmælendur sem höfðu gerst sekir um að rita nafn sitt á lista margra frambjóðenda. Þetta árið voru tugir meðmælenda strikaðir út - og sannaði forritið gildi sitt þar með.

Kapital
Haft var samband við Kapital í aðdraganda borgarstjórnarkosninganna og þeir beðnir um að smíða kosningakerfi, þ.e. kerfi sem heldur utan um stuðningsmenn og atkvæði. Ég smíðaði þetta kerfi með þeim. Við notuðumst við PHP, Propel (ORM), Smarty og PostgreSQL.




### Magg - Casting

Ljósmyndafyrirtækinu Magg vantaði kerfi til þess að halda utan um þann arargrúa af módelum (í dag telja þau um 4000). Kröfur kerfisins voru einfaldar; halda þurfti utan um myndir (eina eða fleiri) og ákveðnar upplýsingar eins og nafn, síma, hárlit og fleira.

Ruby on Rails hentaði sérlega vel í þetta verk og eru upplýsingarnar geymdar í PostgreSQL-gagnagrunni. Vefurinn er nú hýstur í vefsal Kapital.




### Orkustofnun

Á meðan ég stundaði nám við HÍ vann ég verkefni fyrir Orkustofnun sem gekk undir nafni Orkutölur. Þetta er vefur sem birtir helstu orkustærðir landsins. Vefurinn er skrifaður í Mason og notaði ég opna hugbúnaðinn xlhtml sem breytir Excel-skjölum í XML-skjöl og slíku skjali er einfalt að varpa yfir í töflu og búa til gröf úr. Notendur vefjarins gátu því sett in gögn á formi sem þeir þekktu og vefurinn birti þau sjálfkrafa í vefútgáfu.



### Ísor

Ég vann sem ráðgjafi fyrir Jón Örn Bjarnason hjá Íslenskum orkurannsóknum við smíði á Java-hugbúnaði. Hann var að endurskrifa gamalt forrit sem hann hafði skrifað í Fortran.



Vefir
Vefir sem ég hef forritað. Nánari lýsing væntanleg síðar.
selecta.is
radiomidun.is
lumex.is
gisli.hamstur.is
mbl.is
hamstur.is
skip.radiomidun.is
iridium.radiomidun.is

 


### Forritunarmál

FoxPro/dBase
Þegar ég var 12 ára gamall sniglaðist ég oft með pabba niðri í vinnu. Hann hafði skrifað viðskiptakerfi fyrirtækisins og sat oft við á kvöldin. Ég fékk áhuga á því sem hann var að gera og bað hann um að kenna mér. Hann fann fyrir mig bók um dBase (undanfara FoxPro) og lét mig lesa og á kvöldin gat ég komið til hans og prófað nýáunna kunnáttu mína í tölvunum hans.

PHP
Forritunarmálið PHP er einfalt og hannað með vefforitun í huga. Þegar ég fór að vaxa úr grasi og vefurinn að blómstra var það rökrétt skref að færa sig yfir í PHP. Frá 15 ára aldri skrifaði ég nokkra vefi og svo nokkur vefkerfi í þessu forritunarmáli. Þegar ég var 17 sótti ég um vinnu hjá mbl.is á grundvelli þess sem ég hafði gert í þessu máli.

Perl
Mbl.is var skrifaður í Informix WebDatablades ásamt nokkrum hlutum í Perl. Síðar var vefurinn umskrifaður í Perl og því var það næsta forritunarmál sem heltók mig. Á Morgunblaðinu skrifuðum við inn í vefumgjörð sem heitir Mason og hentaði það ákaflega vel fyrir það sem við vorum að gera. Síðar meir þegar ég starfaði hjá Radiomiðun skrifaði ég nánast eingöngu í Perl, bæði skriftur og heilu kerfin.

Python
Python hefur alltaf verið mér huglægt. Ég hef smíðað lítil og meðalstór kerfi fyrir H.F. Verðbréf hf. í Python og er það eitt af uppáhaldsforritunarmálunum mínum. Sem dæmi má nefna græju sem sér um halda öllum verðbréfum sem miðlað er í kauphöllum þar sem við erum ekki kauphallaraðilar samstilltum milli miðlunarkerfisins okkar og kauphallarinnar. Einnig er kerfið sem meðhöndlar upphaf viðskipta skrifað í Python en það sér um að flokka og skrá kúnnann í gagnagrunn og smíða sérsniðna samninga fyrir kúnnann.

Ruby
Í kjölfarið af allri umfjölluninni um Ruby on Rails ákvað ég að kynna mér þetta forritunarmál örlítið betur. Í ljós kom að það er einstaklega skemmtilegt að forrita í þessu forritunarmáli og margir nýir, skemmtilegir vinklar blasa við - reyndar fæstir nýir því þeir koma úr öðrum fallamálum. Ég skrifaði Skúlann í Ruby on Rails og viðheld honum enn. Mér finnst Ruby hins vegar of hægt til þess að ég vilji setja það í eitthvert alvöru verkefni og hef því nú einbeitt mér meir að Haskell.

C/C++/C#
Forritunarmálin C og C++ eru náskyld og teljast til forritunarmála sem þykja nálægt vélbúnaðinum og henta því til stýrikerfaforritunar en C# er þó meir í ætt við Java. Ég hef notað C og C++ í ýmis verkefni og tók tildæmis þátt í þróun xlHtml sem færði Excel-skjöl yfir í HTML/XML. Vindhraðamælirinn (sem hægt er að lesa um á verkefnasíðunni) var svo alfarið skrifaður í C. C# er hluti af .NET og hef ég notað það í Excel Code Behind-verkefnum.

Lisp
Í kúrsinum, Forritunarmál, skoðuðum við mörg óvenjuleg forritunarmál og var Lisp eitt af þeim. Það er margt hægt að læra af þessu forritunarmáli og þá sérstaklega ,,code as data´´-mottóið.

Java
Java var notað í Radiomiðun til þess að útfæra Birti sem sá um að birta upplýsingar sem Þjónustubankinn sendi. Að geta pakkað hugbúnaðinum inn í JAR-skrá kom sér sérlega vel þar sem krafa var um að geta uppfært hugbúnað í fjarvinnslu.

Erlang
Þetta er eitt mest spennandi forritunarmálið þessa stundina. Það var hannað af símaframleiðandanum Ericson og það sem gerir það frábrugðið flestum forritunarmálum er hæfileikinn til þess að smíða hugbúnað sem keyrir á mörgum örgjörvum/tölvum samtímis með mikinn uppitíma.

Haskell
Þetta er uppáhaldsforritunarmálið mitt þessa dagana. Það sem mér finnst heillandi við þetta forritunarmál er að það er, latt fallamál sem gerir það í raun einstakt í alla staði og sú staðreynd þvingar mann til þess að hugsa um vandamálið á allt annan hátt - í raun meira abstrakt. Nýja bloggið mitt er knúið áfram af HAppS sem er forritaþjónn (e. Application server) sem skrifaður er í Haskell.



