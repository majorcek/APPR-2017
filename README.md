# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika
Odločil sem se, da bom v projektu analiziral smučarske skoke, saj je to šport, ki je meni eden izmed najlubših za spremljanje, hkrati pa še ni narejenih veliko analiz na tem področju. Še posebej rad spremljam smučarske polete, zato sem se omejil in zbral podatke zgolj za tekme na letalnicah (kar pomeni, da je velikost skakalnice večja od 150m) tako v svetovnem pokalu kot na svetovnih prvenstvih v poletih. Podatki so od leta 2011 naprej, saj se pred tem še ni uporabljalo vetrne izravnave. 

Podatke sem pridobil iz uradne baze podatkov in iz wikipedie. Iz uradne baze sem dobil rezultate posameznega tekmovanja (denimo Planica, 23. marec 2017), medtem ko sem iz Wikipedie dobil podatke o državnih rekordih, poleg tega pa še nekaj tabel, ki sem jih sproti potreboval za analizo (npr. 2-mestne kratice držav). Največjo težave so mi predstavljali podatki iz FIS-a, saj so na voljo samo v PDF, tako da sem jih s programom najprej preoblikoval v xlsx ter nato uvozil v RStudio.
Najpomembnejši strani:
    * baza FIS-a: https://data.fis-ski.com/ski-jumping/results.html
    * wikipedia: https://en.wikipedia.org/wiki/Ski_jumping

Poizkušal bom ugotoviti, ali držijo nekatere moje domneve (da imajo tekmovalci iz Slovenije nižje hitrost ob doskoku), preveriti si želim tudi, ali držijo trditve slovenskih kometatorjev o nepoštenemu slogovnemu ocenjevanju. Poleg tega bom skušal zanimivo prikazati še nekatere druge podatke, denimo razvoj državnih rekordov in ocentiti njihovo gibanje v prihodnje.
#### tabele
Kot je navedeno zgoraj, sem večino podatkov o skokih dobil iz baze FIS-a (https://data.fis-ski.com/dynamic/event-details.html?event_id=39344&cal_suchsector=JP). Za vsako tekmovanje sem dobil svojo tabelo, ki je imela naslednjo obliko:
kable(head(Planica2011_sobota))
    * 1. stolpec: IME in PRIIMEK skakalca , factor
    * 2. stolpec: HITROST ob odskoku (enota m/s) , številska spremenljivka 
    * 3. stolpec: DOLŽINA (enota m) , številska spremenljivka 
    * 4. stolpec: TOČKE (za dolžino) , številska spremenljivka
    * 5.-9. stolpec: POSAMEZNE SODNIŠKE OCENE , številska spremenljivka 
    * 10. stolpec: OCENE SKUPAJ (skupne sodniške ocene) , številska spremenljivka 
    * 11. stolpec: ZALET (zaletno mesto) , številska spremenljivka 
    * 12. stolpec: TOČKE ZA ZALET (pribitek/odbitek za spremenjeno , številska spremenljivka zaletno mesto) 
    * 13. stolpec: VETER (enota m/s) , številska spremenljivka 
    * 14. stolpec: IZRAVNAVA (pribitek/odbitek zaradi vetra) , številska spremenljivka 
    * 15. stolpec: SKUPAJ TOČKE ( TOČKE + OCENE SKUPAJ + TOČKE ZA ZALET + IZRAVNAVA) , številska spremenljivka 
    * 16. stolpec: REZULTAT V SERIJI , številska spremenljivka 
    * 17. stolpec: LETO (kdaj je potekalo tekmovanje) , številska spremenljivka 
    * 18. stolpec: SKAKALNICA (kje je potekalo tekmovanje) , character 
    
Tabele sem nato najprej združil po letih (Planica2011 vsebuje podatke o tekmah v Planici v petek, soboto in nedeljo 2011). Nato sem jih združil po skakalnicah (Planica je sestavljena iz Planica2011, Planica2012, ... , Planica 2017). Na koncu pa sem naredil še dataframe, kje so zbrani vsi skoki in jo poimenoval vsi_skoki.

Podatke sem pridobil tudi iz Wikipedie, predvsem s strani https://en.wikipedia.org/wiki/Ski_jumping.
Tabela nad240 vsebuje podatke o vseh poletih, dolgih vsaj 240m . Izločil sem tiste skoke, ki niso bili doseženi na tekmi (treningi, poskusni skoki) in vzel samo uspešne skoke (izločil padce), saj sem želel opraviti analizo uspešnih skokov. Padec pomeni izgubo točk v višini približno 20 metrov, zaradi česar tak skok ne prinese vrhunske uvrstitve.
Tabela ima nasldnjo obliko: 
kable(head(nad240))
    1. stolpec: LETO , številska spremenljivka \n
    2. stolpec: SKAKALEC (ime in priimek), factor \n
    3. stolpec: DRŽAVA (od kod je skakalec), factor \n
    4. stolpec: LOKACIJA (na kateri skakalnici), factor \n
    5. stolpec: DOLŽINA (enota m) , številska spremenljivka \n   
Druga tabela, ki sem jo pridobil iz Wikipedie, je tabela državnih rekordov in je dostopna na istem naslovu. To tabelo sem moral malenkost povpraviti, saj zaradi čudne oblike tabele nekaj vrstic ni pravilno uvozilo.

Tretja tabela iz wikipedije je tabela dvomestinih kratic za države (https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2). 
    kable(head(okrajsave2))
    1. stolpec: Code2 , factor  \n
    2. stolpec: Country , character  \n
Četrta tabela iz wikipedije je tabela tromestinih kratic za države (https://en.wikipedia.org/wiki/ISO_3166-1). \n
    kable(head(okrajsave3))
    1. stolpec: Country , factor \n
    2. stolpec: Code3 , factor \n

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
