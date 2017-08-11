# 2. faza: Uvoz podatkov


### uvoz tabele skokov nad 240 metrov
url <- "https://en.wikipedia.org/wiki/List_of_the_longest_ski_jumps"
webpage <- getURL(url)
nad240 <- readHTMLTable(webpage, header = TRUE,
                        colClasses = NULL, skip.rows = integer(), trim = TRUE,
                        elFun = xmlValue, as.data.frame = TRUE, which = 5,
                        encoding = "UTF-8")
nad240$No. <- NULL
nad240$Size <- NULL
nad240$Hill <- NULL
colnames(nad240) <- c("LETO", "SKAKALEC", "DRŽAVA", "LOKACIJA","DOLŽINA")
nad240$LETO <- gsub(".* ","",nad240$LETO) %>%  as.character %>% as.numeric()
nad240$OPOMBE <- nad240$DOLŽINA
nad240$OPOMBE <- sub(".* m","",nad240$OPOMBE)
nad240$DOLŽINA <- gsub(" m.*","",nad240$DOLŽINA ) %>%  as.character %>% as.numeric()
nad240 <- nad240[(nad240$OPOMBE == ""),]
nad240$OPOMBE <- NULL

rezultati <- nad240[,c("LETO","SKAKALEC","LOKACIJA", "DOLŽINA")]
dodaj1 <- c("2017","Andreas Wellinger","Vikersund, Norway", "242 m") #pomanjkljiva tabela na wikipedii
dodaj2 <- c("2017","Andreas Stjernen","Vikersund, Norway",	"242 m")
rezultati <- rbind(rezultati, dodaj1, dodaj2)

razmere <- read.xlsx("podatki/skoki1.xlsx","V17",encoding = "UTF-8")
colnames(razmere) <- c("LETO", "SKAKALEC", "DOLŽINA", "HITROST", "VETER", "IZRAVNAVA", "LOKACIJA")
razmere <- razmere[!(razmere$HITROST == ""),]
razmere <- razmere[rowSums(is.na(razmere)) == 0,]
razmere <- razmere[, !(names(razmere) == "VETER")]
razmere$LETO <- as.integer(razmere$LETO)

nad240$LETO <- as.integer(nad240$LETO)
nad240$DOLŽINA <- as.numeric(nad240$DOLŽINA)
drops <- c("SKAKALNICA","VELIKOST SKAKALNICE(HS)", "OPOMBE")
nad240 <- nad240[, !(names(nad240) %in% drops)]


### uvoz tabele državnih rekordov

url2 <- "https://en.wikipedia.org/wiki/Ski_jumping"
webpage2 <- getURL(url2)
drzavni_rekordi <- readHTMLTable(webpage2, header = TRUE,
                                 skip.rows = integer(), trim = TRUE,
                                 elFun = xmlValue, as.data.frame = TRUE, which = 8, 
                                 encoding = "UTF-8")

drzavni_rekordi <- drzavni_rekordi[,c("Nation","Metres","Place")]
names(drzavni_rekordi) <- c("Country","metres","place")
drzavni_rekordi$Country <- gsub("\\[.*", "", drzavni_rekordi$Country)

# popravek zaradi čudne oblike tabele na wikipedii
drzavni_rekordi <- filter(drzavni_rekordi, !(drzavni_rekordi$Country %in% c("Chris Hellerud","Ireland","Latvia","South Korea")))
nove_drzave <- c("Ireland","Latvia","South Korea")
nove_razdalje <- c(35.0,102.0,207.5)
nove_lokacije <- c("Gothenburg", "Liberec", "Planica")
l <- c(nove_drzave,nove_razdalje,nove_lokacije)
nove <- data.frame(matrix(unlist(l), nrow=3))
names(nove) <- c("Country","metres","place")

drzavni_rekordi <- rbind(drzavni_rekordi,nove)
drzavni_rekordi$place <- gsub("Tauplitz","Kulm",drzavni_rekordi$place) # Tauplitz in Kulm pomenita isto skakalnico

ostale_skakalnice <- filter(drzavni_rekordi, !(drzavni_rekordi$place %in% c("Planica","Vikersund", "Kulm", "Harrachov", "Obersdorf")))
skakalnice_rekordi <- unique(ostale_skakalnice$place)
drzavni_rekordi$place[drzavni_rekordi$place %in% skakalnice_rekordi] <- "ostalo"
drzavni_rekordi$metres <- gsub("\\..*","",drzavni_rekordi$metres)
drzavni_rekordi$metres <- as.numeric(drzavni_rekordi$metres)

drzavni_rekordi$Country <- drzavni_rekordi$Country %>% parse_character() %>%
  strapplyc("^[^A-Z]*([^[]*)") %>% unlist()

### uvoz tabele uradnih dvomestnih oznak držav 

url3 <- "https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2"
webpage3 <- getURL(url3)
okrajsave2 <- readHTMLTable(webpage3, header = TRUE,
                            colClasses = NULL, skip.rows = integer(), trim = TRUE,
                            elFun = xmlValue, as.data.frame = TRUE, which = 3, 
                            encoding = "UTF-8")

okrajsave2 <- okrajsave2[1:2]
colnames(okrajsave2) <- c("Code2", "Country")
okrajsave2$Country <- gsub("Czechia", "Czech Republic", okrajsave2$Country)
okrajsave2$Country <- gsub("Russian Federation", "Russia", okrajsave2$Country)
okrajsave2$Country <- gsub("Korea, Republic of", "South Korea", okrajsave2$Country)
okrajsave2$Country <- gsub("United States of America", "United States", okrajsave2$Country)
okrajsave2$Country <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", okrajsave2$Country)
okrajsave2$Country <- gsub("Macedonia, the former Yugoslav Republic of", "Macedonia", okrajsave2$Country)

### uvoz tabele uradnih tromestnih oznak držav 
url4 <- "https://en.wikipedia.org/wiki/ISO_3166-1"
webpage4 <- getURL(url4)
okrajsave3 <- readHTMLTable(webpage4, header = TRUE,
                           colClasses = NULL, skip.rows = integer(), trim = TRUE,
                           elFun = xmlValue, as.data.frame = TRUE, which = 2,
                           encoding = "UTF-8")
okrajsave3 <- okrajsave3[,c(1,3)]
colnames(okrajsave3) <- c("Country", "Code3")
okrajsave3$Country <- gsub("!.*", "", okrajsave3$Country)
okrajsave <- merge(okrajsave2,okrajsave3, by = "Country")


#priprava za graf števila tekem 
leta <- c(2011:2017)
pla <- data.frame(leta, "Planica")
colnames(pla) <- c("LETO", "SKAKALNICA")
vik <- data.frame(leta, "Vikersund")
colnames(vik) <- c("LETO", "SKAKALNICA")
tekmovanje <- rbind(pla, vik)
har <- data.frame(leta, "Harrachov")
colnames(har) <- c("LETO", "SKAKALNICA")
tekmovanje <- rbind(tekmovanje, har)
obe <- data.frame(leta, "Obersdorf")
colnames(obe) <- c("LETO", "SKAKALNICA")
tekmovanje <- rbind(tekmovanje, obe)
kul <- data.frame(leta, "Kulm")
colnames(kul) <- c("LETO", "SKAKALNICA")
tekmovanje <- rbind(tekmovanje, kul)
tekmovanje$st_tekem <- NULL
### uvoz podatkov o skokih iz vseh tekem na letalnicah od leta 2011 naprej


vikersund2017_nedelja <- read.table("podatki/vikersund17_19marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), 
                                    encoding = "UTF-8")
vikersund2017_sobota <- read.csv("podatki/vikersund17_18marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), 
                                 encoding = "UTF-8")
vikersund2017_nedelja$IME <- paste(vikersund2017_nedelja$PRIIMEK, vikersund2017_nedelja$IME, sep=" ")
vikersund2017_sobota$IME <- paste(vikersund2017_sobota$PRIIMEK, vikersund2017_sobota$IME, sep=" ")
vikersund2017_nedelja$IME <- gsub(".", " ", vikersund2017_nedelja$IME, fixed=TRUE)
vikersund2017_sobota$IME <- gsub(".", " ", vikersund2017_sobota$IME, fixed=TRUE)
vikersund2017_sobota <- vikersund2017_sobota[,!(names(vikersund2017_sobota) == "PRIIMEK")]
vikersund2017_nedelja <- vikersund2017_nedelja[,!(names(vikersund2017_nedelja) == "PRIIMEK")]
names(vikersund2017_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2017_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
vikersund2017_sobota$"LETO" <- 2017
vikersund2017_nedelja$"LETO" <- 2017
vikersund2017_sobota$"SKAKALNICA" <- "Vikersund"
vikersund2017_nedelja$"SKAKALNICA" <- "Vikersund"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2017] <- 2

vikersund2017 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2017_nedelja, vikersund2017_sobota))

vikersund2016_petek <- read.csv("podatki/vikersund16_12februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2016_sobota <- read.csv("podatki/vikersund16_13februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2016_nedelja <- read.csv("podatki/vikersund16_14februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2016_nedelja$IME <- paste(vikersund2016_nedelja$PRIIMEK, vikersund2016_nedelja$IME, sep=" ")
vikersund2016_sobota$IME <- paste(vikersund2016_sobota$PRIIMEK, vikersund2016_sobota$IME, sep=" ")
vikersund2016_petek$IME <- paste(vikersund2016_petek$PRIIMEK, vikersund2016_petek$IME, sep=" ")
vikersund2016_nedelja$IME <- gsub(".", " ", vikersund2016_nedelja$IME, fixed=TRUE)
vikersund2016_sobota$IME <- gsub(".", " ", vikersund2016_sobota$IME, fixed=TRUE)
vikersund2016_petek$IME <- gsub(".", " ", vikersund2016_petek$IME, fixed=TRUE)
vikersund2016_petek <- vikersund2016_petek[,!(names(vikersund2016_petek) == "PRIIMEK")]
vikersund2016_sobota <- vikersund2016_sobota[,!(names(vikersund2016_sobota) == "PRIIMEK")]
vikersund2016_nedelja <- vikersund2016_nedelja[,!(names(vikersund2016_nedelja) == "PRIIMEK")]
names(vikersund2016_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2016_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2016_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
vikersund2016_sobota$"LETO" <- 2016
vikersund2016_nedelja$"LETO" <- 2016
vikersund2016_petek$"LETO" <- 2016
vikersund2016_petek$"SKAKALNICA" <- "Vikersund"
vikersund2016_sobota$"SKAKALNICA" <- "Vikersund"
vikersund2016_nedelja$"SKAKALNICA" <- "Vikersund"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2016] <- 3

vikersund2016 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2016_nedelja, vikersund2016_sobota, vikersund2016_petek))

vikersund2015_sobota <- read.csv("podatki/vikersund15_14februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET","VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2015_nedelja <- read.csv("podatki/vikersund15_15februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2015_nedelja$IME <- paste(vikersund2015_nedelja$PRIIMEK, vikersund2015_nedelja$IME, sep=" ")
vikersund2015_sobota$IME <- paste(vikersund2015_sobota$PRIIMEK, vikersund2015_sobota$IME, sep=" ")
vikersund2015_nedelja$IME <- gsub(".", " ", vikersund2015_nedelja$IME, fixed=TRUE)
vikersund2015_sobota$IME <- gsub(".", " ", vikersund2015_sobota$IME, fixed=TRUE)
vikersund2015_sobota <- vikersund2015_sobota[,!(names(vikersund2015_sobota) == "PRIIMEK")]
vikersund2015_nedelja <- vikersund2015_nedelja[,!(names(vikersund2015_nedelja) == "PRIIMEK")]
names(vikersund2015_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2015_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
vikersund2015_sobota$"LETO" <- 2015
vikersund2015_nedelja$"LETO" <- 2015
vikersund2015_sobota$"SKAKALNICA" <- "Vikersund"
vikersund2015_nedelja$"SKAKALNICA" <- "Vikersund"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2015] <- 2


vikersund2015 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2015_nedelja, vikersund2015_sobota))

vikersund2013_nedelja <- read.table("podatki/vikersund13_26januar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2013_sobota <- read.csv("podatki/vikersund13_27januar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2013_nedelja$IME <- paste(vikersund2013_nedelja$PRIIMEK, vikersund2013_nedelja$IME, sep=" ")
vikersund2013_sobota$IME <- paste(vikersund2013_sobota$PRIIMEK, vikersund2013_sobota$IME, sep=" ")
vikersund2013_nedelja$IME <- gsub(".", " ", vikersund2013_nedelja$IME, fixed=TRUE)
vikersund2013_sobota$IME <- gsub(".", " ", vikersund2013_sobota$IME, fixed=TRUE)
vikersund2013_sobota <- vikersund2013_sobota[,!(names(vikersund2013_sobota) == "PRIIMEK")]
vikersund2013_nedelja <- vikersund2013_nedelja[,!(names(vikersund2013_nedelja) == "PRIIMEK")]
names(vikersund2013_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2013_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
vikersund2013_sobota$"LETO" <- 2013
vikersund2013_nedelja$"LETO" <- 2013
vikersund2013_sobota$"SKAKALNICA" <- "Vikersund"
vikersund2013_nedelja$"SKAKALNICA" <- "Vikersund"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2013] <- 2


vikersund2013 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2013_nedelja, vikersund2013_sobota))

vikersund2011_nedelja <- read.csv("podatki/vikersund11_12februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2011_sobota <- read.csv("podatki/vikersund11_13februar.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
vikersund2011_nedelja$IME <- paste(vikersund2011_nedelja$PRIIMEK, vikersund2011_nedelja$IME, sep=" ")
vikersund2011_sobota$IME <- paste(vikersund2011_sobota$PRIIMEK, vikersund2011_sobota$IME, sep=" ")
vikersund2011_nedelja$IME <- gsub(".", " ", vikersund2011_nedelja$IME, fixed=TRUE)
vikersund2011_sobota$IME <- gsub(".", " ", vikersund2011_sobota$IME, fixed=TRUE)
vikersund2011_sobota <- vikersund2011_sobota[,!(names(vikersund2011_sobota) == "PRIIMEK")]
vikersund2011_nedelja <- vikersund2011_nedelja[,!(names(vikersund2011_nedelja) == "PRIIMEK")]
names(vikersund2011_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(vikersund2011_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
vikersund2011_sobota$"LETO" <- 2011
vikersund2011_nedelja$"LETO" <- 2011
vikersund2011_sobota$"SKAKALNICA" <- "Vikersund"
vikersund2011_nedelja$"SKAKALNICA" <- "Vikersund"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2011] <- 2


vikersund2011 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2011_nedelja, vikersund2011_sobota))

vikersund <- Reduce(function(x, y) merge(x, y, all=TRUE), list(vikersund2011, vikersund2013, vikersund2015, vikersund2016, vikersund2017))

#PLANICA

planica2017_petek <- read.csv("podatki/planica17_24marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME" ,"HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
planica2017_sobota <- read.csv("podatki/planica17_25marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
planica2017_nedelja <- read.csv("podatki/planica17_26marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "DRŽAVA" ,"HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
planica2017_petek$IME <- paste(planica2017_petek$PRIIMEK, planica2017_petek$IME, sep=" ")
planica2017_sobota$IME <- paste(planica2017_sobota$PRIIMEK, planica2017_sobota$IME, sep=" ")
planica2017_nedelja$IME <- paste(planica2017_nedelja$PRIIMEK, planica2017_nedelja$IME, sep=" ")
planica2017_petek$IME <- gsub(".", " ", planica2017_petek$IME, fixed=TRUE)
planica2017_sobota$IME <- gsub(".", " ", planica2017_sobota$IME, fixed=TRUE)
planica2017_nedelja$IME <- gsub(".", " ", planica2017_nedelja$IME, fixed=TRUE)
planica2017_petek <- planica2017_petek[,!(names(planica2017_petek) == "PRIIMEK")]
planica2017_sobota <- planica2017_sobota[,!(names(planica2017_sobota) == "PRIIMEK")]
planica2017_nedelja <- planica2017_nedelja[,!(names(planica2017_nedelja) == "PRIIMEK")]
planica2017_nedelja <- planica2017_nedelja[,!(names(planica2017_nedelja) == "DRŽAVA")]
names(planica2017_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(planica2017_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(planica2017_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")

planica2017_petek$"LETO" <- 2017
planica2017_sobota$"LETO" <- 2017
planica2017_nedelja$"LETO" <- 2017
planica2017_petek$"SKAKALNICA" <- "Planica"
planica2017_sobota$"SKAKALNICA" <- "Planica"
planica2017_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2017] <- 3


planica2017 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2017_petek, planica2017_sobota, planica2017_nedelja))

planica2016_cetrtek <- read.xlsx("podatki/planica16_17marec.xlsx",sheetIndex = 1,header = FALSE)
names(planica2016_cetrtek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2016_petek <- read.xlsx("podatki/planica16_18marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2016_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2016_sobota <- read.csv("podatki/planica16_19marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
planica2016_nedelja <- read.csv("podatki/planica16_20marec.csv",header = FALSE, dec = ".", sep = " ", col.names = c("PRIIMEK", "IME", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI"), encoding = "UTF-8")
planica2016_sobota$IME <- paste(planica2016_sobota$PRIIMEK, planica2016_sobota$IME, sep=" ")
planica2016_nedelja$IME <- paste(planica2016_nedelja$PRIIMEK, planica2016_nedelja$IME, sep=" ")
planica2016_sobota <- planica2016_sobota[,!(names(planica2016_sobota) == "PRIIMEK")]
planica2016_nedelja <- planica2016_nedelja[,!(names(planica2016_nedelja) == "PRIIMEK")]
planica2016_sobota$IME <- gsub(".", " ", planica2016_sobota$IME, fixed=TRUE)
planica2016_nedelja$IME <- gsub(".", " ", planica2016_nedelja$IME, fixed=TRUE)
names(planica2016_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
names(planica2016_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2016_cetrtek$"LETO" <- 2016
planica2016_petek$"LETO" <- 2016
planica2016_sobota$"LETO" <- 2016
planica2016_nedelja$"LETO" <- 2016
planica2016_cetrtek$"SKAKALNICA" <- "Planica"
planica2016_petek$"SKAKALNICA" <- "Planica"
planica2016_sobota$"SKAKALNICA" <- "Planica"
planica2016_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2016] <- 4

planica2016 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2016_cetrtek, planica2016_petek, planica2016_sobota, planica2016_nedelja))

planica2015_petek <- read.xlsx("podatki/planica15_20marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2015_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2015_sobota <- read.xlsx("podatki/planica15_21marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2015_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2015_nedelja <- read.xlsx("podatki/planica15_22marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2015_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2015_petek$"LETO" <- 2015
planica2015_sobota$"LETO" <- 2015
planica2015_nedelja$"LETO" <- 2015
planica2015_petek$"SKAKALNICA" <- "Planica"
planica2015_sobota$"SKAKALNICA" <- "Planica"
planica2015_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2015] <- 3


planica2015 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2015_petek, planica2015_sobota, planica2015_nedelja))

planica2013_petek <- read.xlsx("podatki/planica13_22marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2013_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2013_sobota <- read.xlsx("podatki/planica13_23marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2013_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2013_nedelja <- read.xlsx("podatki/planica13_24marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2013_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2013_petek$"LETO" <- 2013
planica2013_sobota$"LETO" <- 2013
planica2013_nedelja$"LETO" <- 2013
planica2013_petek$"SKAKALNICA" <- "Planica"
planica2013_sobota$"SKAKALNICA" <- "Planica"
planica2013_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2013] <- 3

planica2013 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2013_petek, planica2013_sobota, planica2013_nedelja))

planica2012_petek <- read.xlsx("podatki/planica12_16marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2012_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2012_sobota <- read.xlsx("podatki/planica12_17marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2012_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2012_nedelja <- read.xlsx("podatki/planica12_18marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2012_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2012_petek$"LETO" <- 2012
planica2012_sobota$"LETO" <- 2012
planica2012_nedelja$"LETO" <- 2012
planica2012_petek$"SKAKALNICA" <- "Planica"
planica2012_sobota$"SKAKALNICA" <- "Planica"
planica2012_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2012] <- 3

planica2012 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2012_petek, planica2012_sobota, planica2012_nedelja))

planica2011_petek <- read.xlsx("podatki/planica11_18marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2011_petek) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2011_sobota <- read.xlsx("podatki/planica11_19marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2011_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2011_nedelja <- read.xlsx("podatki/planica11_20marec.xlsx",sheetIndex = 1, header = FALSE)
names(planica2011_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
planica2011_petek$"LETO" <- 2011
planica2011_sobota$"LETO" <- 2011
planica2011_nedelja$"LETO" <- 2011
planica2011_petek$"SKAKALNICA" <- "Planica"
planica2011_sobota$"SKAKALNICA" <- "Planica"
planica2011_nedelja$"SKAKALNICA" <- "Planica"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2011] <- 3

planica2011 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2011_petek, planica2011_sobota, planica2011_nedelja))

planica <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica2011, planica2012, planica2013, planica2015, planica2016, planica2017))


#OBERSDORF

obersdorf2017_sobota <- read.xlsx("podatki/obersdorf17_4februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2017_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2017_nedelja <- read.xlsx("podatki/obersdorf17_5februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2017_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2017_sobota$"LETO" <- 2017
obersdorf2017_nedelja$"LETO" <- 2017
obersdorf2017_sobota$"SKAKALNICA" <- "Obersdorf"
obersdorf2017_nedelja$"SKAKALNICA" <- "Obersdorf"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2017] <- 2


#obersdorf2017 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(obersdorf2017_sobota, obersdorf2017_nedelja))
obersdorf2017 <- rbind(obersdorf2017_sobota, obersdorf2017_nedelja)

obersdorf2013_sobota <- read.xlsx("podatki/obersdorf13_16februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2013_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2013_nedelja <- read.xlsx("podatki/obersdorf13_17februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2013_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2013_sobota$"LETO" <- 2013
obersdorf2013_nedelja$"LETO" <- 2013
obersdorf2013_sobota$"SKAKALNICA" <- "Obersdorf"
obersdorf2013_nedelja$"SKAKALNICA" <- "Obersdorf"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2013] <- 2

obersdorf2013 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(obersdorf2013_sobota, obersdorf2013_nedelja))

obersdorf2012_sobota <- read.xlsx("podatki/obersdorf12_18februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2012_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2012_nedelja <- read.xlsx("podatki/obersdorf12_19februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2012_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2012_sobota$"LETO" <- 2012
obersdorf2012_nedelja$"LETO" <- 2012
obersdorf2012_sobota$"SKAKALNICA" <- "Obersdorf"
obersdorf2012_nedelja$"SKAKALNICA" <- "Obersdorf"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2012] <- 2

obersdorf2012 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(obersdorf2012_sobota, obersdorf2012_nedelja))

obersdorf2011_sobota <- read.xlsx("podatki/obersdorf11_5februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2011_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2011_nedelja <- read.xlsx("podatki/obersdorf11_6februar.xlsx",sheetIndex = 1, header = FALSE)
names(obersdorf2011_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
obersdorf2011_sobota$"LETO" <- 2011
obersdorf2011_nedelja$"LETO" <- 2011
obersdorf2011_sobota$"SKAKALNICA" <- "Obersdorf"
obersdorf2011_nedelja$"SKAKALNICA" <- "Obersdorf"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2011] <- 2


obersdorf2011 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(obersdorf2011_sobota, obersdorf2011_nedelja))

obersdorf <- Reduce(function(x, y) merge(x, y, all=TRUE), list(obersdorf2011, obersdorf2012, obersdorf2013, obersdorf2017))

#KULM

kulm2016_sobota <- read.xlsx("podatki/kulm16_16januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2016_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2016_nedelja <- read.xlsx("podatki/kulm16_17januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2016_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2016_sobota$"LETO" <- 2016
kulm2016_nedelja$"LETO" <- 2016
kulm2016_sobota$"SKAKALNICA" <- "Kulm"
kulm2016_nedelja$"SKAKALNICA" <- "Kulm"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2016] <- 2

kulm2016 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(kulm2016_sobota, kulm2016_nedelja))

kulm2015_sobota <- read.xlsx("podatki/kulm15_10januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2015_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2015_sobota$"LETO" <- 2015
kulm2015_sobota$"SKAKALNICA" <- "Obersdorf"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2015] <- 1

kulm2015 <- kulm2015_sobota

kulm2014_sobota <- read.xlsx("podatki/kulm14_11januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2014_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2014_nedelja <- read.xlsx("podatki/kulm14_12januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2014_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2014_sobota$"LETO" <- 2014
kulm2014_nedelja$"LETO" <- 2014
kulm2014_sobota$"SKAKALNICA" <- "Kulm"
kulm2014_nedelja$"SKAKALNICA" <- "Kulm"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2014] <- 2

kulm2014 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(kulm2014_sobota, kulm2014_nedelja))

kulm2012_sobota <- read.xlsx("podatki/kulm12_15januar.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2012_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2012_nedelja <- read.xlsx("podatki/kulm12_15januar2.xlsx",sheetIndex = 1, header = FALSE)
names(kulm2012_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
kulm2012_sobota$"LETO" <- 2012
kulm2012_nedelja$"LETO" <- 2012
kulm2012_sobota$"SKAKALNICA" <- "Kulm"
kulm2012_nedelja$"SKAKALNICA" <- "Kulm"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2012] <- 2

kulm2012 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(kulm2012_sobota, kulm2012_nedelja))

kulm <- Reduce(function(x, y) merge(x, y, all=TRUE), list(kulm2012, kulm2014, kulm2015, kulm2016))

#HARRACHOV

harrachov2014_sobota <- read.xlsx("podatki/harrachov14_14marec.xlsx",sheetIndex = 1, header = FALSE)
names(harrachov2014_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
harrachov2014_sobota$"LETO" <- 2014
harrachov2014_sobota$"SKAKALNICA" <- "Harrachov"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2014] <- 1

harrachov2014 <- harrachov2014_sobota

harrachov2013_sobota <- read.xlsx("podatki/harrachov13_2februar.xlsx",sheetIndex = 1, header = FALSE)
names(harrachov2013_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
harrachov2013_nedelja <- read.xlsx("podatki/harrachov13_3februar.xlsx",sheetIndex = 1, header = FALSE)
names(harrachov2013_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
harrachov2013_sobota$"LETO" <- 2013
harrachov2013_nedelja$"LETO" <- 2013
harrachov2013_sobota$"SKAKALNICA" <- "Harrachov"
harrachov2013_nedelja$"SKAKALNICA" <- "Harrachov"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2013] <- 2


harrachov2013 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(harrachov2013_sobota, harrachov2013_nedelja))

harrachov2011_sobota <- read.xlsx("podatki/harrachov11_8januar.xlsx",sheetIndex = 1, header = FALSE)
names(harrachov2011_sobota) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
harrachov2011_nedelja <- read.xlsx("podatki/harrachov11_9januar.xlsx",sheetIndex = 1, header = FALSE)
names(harrachov2011_nedelja) = c("SKAKALEC", "HITROST", "DOLŽINA","TOČKE", "OCENA1", "OCENA2", "OCENA3", "OCENA4", "OCENA5","OCENE SKUPAJ", "ZALET", "TOČKE ZA ZALET", "VETER", "IZRAVNAVA", "SKUPAJ TOČKE","REZULTAT V SERIJI")
harrachov2011_sobota$"LETO" <- 2011
harrachov2011_nedelja$"LETO" <- 2011
harrachov2011_sobota$"SKAKALNICA" <- "Harrachov"
harrachov2011_nedelja$"SKAKALNICA" <- "Harrachov"
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2011] <- 2


harrachov2011 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(harrachov2011_sobota, harrachov2011_nedelja))

harrachov <- Reduce(function(x, y) merge(x, y, all=TRUE), list(harrachov2011, harrachov2013, harrachov2014))

#skupna tabela 

vsi_skoki <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica, vikersund, obersdorf, kulm, harrachov))
gsub("[[:punct:]]", " ", vsi_skoki)

# skakalci
skakalci_drzave <- read.xlsx("podatki/SKAKALCI.xlsx",sheetIndex = 1, header = FALSE)
names(skakalci_drzave) <- c("SKAKALEC","DRŽAVA")
skakalci_drzave$SKAKALEC <- gsub("-"," ",skakalci_drzave$SKAKALEC)
skakalci_drzave <- unique(skakalci_drzave)



tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2011] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2013] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2015] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2016] <- 3
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Vikersund" & tekmovanje$LETO == 2017] <- 2

tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2011] <- 3
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2012] <- 3
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2013] <- 3
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2015] <- 3
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2016] <- 4
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Planica" & tekmovanje$LETO == 2017] <- 3

tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2011] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2012] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2013] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Obersdorf" & tekmovanje$LETO == 2017] <- 2

tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2012] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2014] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2015] <- 1
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Kulm" & tekmovanje$LETO == 2016] <- 2

tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2011] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2013] <- 2
tekmovanje$st_tekem[tekmovanje$SKAKALNICA == "Harrachov" & tekmovanje$LETO == 2014] <- 1

tekmovanje[is.na(tekmovanje)] <- 0
