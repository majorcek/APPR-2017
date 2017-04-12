# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika
Odločil sem se, da bom analiziral podatke o uspešnih smučarski poletih, dolgih vsaj 240m. Podatke sem dobil iz wikipedie(https://en.wikipedia.org/wiki/List_of_the_longest_ski_jumps) in pa iz uradne strani mednarodne smučaske zveze(http://medias4.fis-ski.com/pdf/2013/JP/3859/2013JP3859RL.pdf).
Ugotoviti želim, kako različne razmere vplivajo na dolžino skokov in kako se z leti spreminja število poletov čez 240m.

Ko združim podatke v eno tabelo, zgledajo tako: http://prntscr.com/evfx18

Na koncu bom še razvrstil skoke v skupine s podobnimi razmerami in pa poskušal oceniti, kako se bo število skokov čez 240m povečevalo na posamezni skaklnici.
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
