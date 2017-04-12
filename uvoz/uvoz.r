# 2. faza: Uvoz podatkov


url <- "https://en.wikipedia.org/wiki/List_of_the_longest_ski_jumps"
webpage <- getURL(url)
tabela <- readHTMLTable(webpage, header = TRUE,
                   colClasses = NULL, skip.rows = integer(), trim = TRUE,
                   elFun = xmlValue, as.data.frame = TRUE, which = 5)
tabela$No. <- NULL
colnames(tabela) <- c("LETO", "SKAKALEC", "DRŽAVA", "SKAKALNICA", "VELIKOST SKAKALNICE(HS)", "LOKACIJA","DOLŽINA")
tabela$LETO <- gsub(".* ","",tabela$LETO)

dodaj1 <- c("2017","Andreas Wellinger","Germany","Vikersundbakken","HS225","Vikersund, Norway",	"242 m")
dodaj2 <- c("2017","Andreas Stjernen","Norway","Vikersundbakken","HS225","Vikersund, Norway",	"242 m")
tabela <- rbind(tabela, dodaj1, dodaj2)

razmere <- read.xlsx("podatki/skoki1.xlsx","V17",encoding = "UTF-8")
colnames(razmere) <- c("LETO", "SKAKALEC", "DOLŽINA", "HITROST", "VETER", "IZRAVNAVA")

tabela$OPOMBE <- tabela$DOLŽINA
tabela$OPOMBE <- sub(".* m","",tabela$OPOMBE)
tabela$`VELIKOST SKAKALNICE(HS)` <- tabela$`VELIKOST SKAKALNICE(HS)` %>% as.character() %>% strapplyc("([0-9]+)") %>% as.numeric()
tabela$DOLŽINA <- gsub(" m.*","",tabela$DOLŽINA )

tabela <- tabela[(tabela$OPOMBE == ""),]
razmere <- razmere[!(razmere$HITROST == ""),]
razmere <- razmere[rowSums(is.na(razmere)) == 0,]

tabela$LETO <- as.integer(tabela$LETO)
tabela$DOLŽINA <- as.numeric(tabela$DOLŽINA)
tabela$OPOMBE <- NULL

razmere$LETO <- as.integer(razmere$LETO)
koncna <- left_join(tabela, razmere, by = c("LETO", "SKAKALEC", "DOLŽINA"))

koncna <- koncna[rowSums(is.na(koncna)) == 0,]

#tabela$LETO <- tabela$LETO %>% as.character() %>% strapplyc("([0-9]+)") %>% as.numeric()



# Funkcija, ki uvozi občine iz Wikipedije
# uvozi.obcine <- function() {
#   link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
#   stran <- html_session(link) %>% read_html()
#   tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#     .[[1]] %>% html_table(dec = ",")
#   colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
#                         "ustanovitev", "pokrajina", "regija", "odcepitev")
#   tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
#   tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
#   tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
#   for (col in colnames(tabela)) {
#     tabela[tabela[[col]] == "-", col] <- NA
#   }
#   for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
#     if (is.numeric(tabela[[col]])) {
#       next()
#     }
#     tabela[[col]] <- gsub("[.*]", "", tabela[[col]]) %>% as.numeric()
#   }
#   for (col in c("obcina", "pokrajina", "regija")) {
#     tabela[[col]] <- factor(tabela[[col]])
#   }
#   return(tabela)
# }
# 
# # Funkcija, ki uvozi podatke iz datoteke druzine.csv
# uvozi.druzine <- function(obcine) {
#   data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
#                     locale = locale(encoding = "Windows-1250"))
#   data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
#     strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
#   data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
#   data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
#                         value.name = "stevilo.druzin")
#   data$velikost.druzine <- as.numeric(data$velikost.druzine)
#   data$obcina <- factor(data$obcina, levels = obcine)
#   return(data)
# }
# 
# # Zapišimo podatke v razpredelnico obcine
# obcine <- uvozi.obcine()
# 
# # Zapišimo podatke v razpredelnico druzine.
# druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
