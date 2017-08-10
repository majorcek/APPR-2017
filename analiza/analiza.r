# 3. faza: Analiza podatkov

# vsi skoki
vsi_skoki <- Reduce(function(x, y) merge(x, y, all=TRUE), list(planica, vikersund, obersdorf, kulm, harrachov))
vsi_skoki <- merge(vsi_skoki,skakalci_drzave,by = "SKAKALEC")
vsi_skoki$RAZREDI <- floor(vsi_skoki$DOLŽINA/10) *10
names(vsi_skoki)[names(vsi_skoki) == 'REZULTAT V SERIJI'] <- 'REZULTAT'

vsi_skoki$POGOJI <- floor(vsi_skoki$IZRAVNAVA/10) * 10
vsi_skoki$POGOJI <- gsub("-40", "zelo dobri",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub("-30", "zelo dobri",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub("-20", "zelo dobri",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub("-10", "dobri",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub(10, "zelo slabi",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub(20, "zelo slabi",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub(30, "zelo slabi",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub(40, "zelo slabi",vsi_skoki$POGOJI)
vsi_skoki$POGOJI <- gsub(0, "slabi",vsi_skoki$POGOJI)

vsi_skoki$POGOJI_IZRAVNAVA <- round(vsi_skoki[,12] + vsi_skoki[,14])

za_grafe <- vsi_skoki %>% group_by(RAZREDI, SKAKALNICA) %>% summarise(povprečne_razmere =  mean(IZRAVNAVA))

# vsi skoki slovenskih skakalcev
skoki_slovenija <- filter(vsi_skoki, DRŽAVA == "SVN")
# vsi skoki avstrijskih skakalcev
skoki_avstrija <- filter(vsi_skoki, DRŽAVA == "AUT")
# vsi skoki nemških skakalcev
skoki_nemcija <- filter(vsi_skoki, DRŽAVA == "DEU")
# vsi skoki poljskih skakalcev
skoki_poljska <- filter(vsi_skoki, DRŽAVA == "POL")
# vsi skoki norveških skakalcev
skoki_norveska <- filter(vsi_skoki, DRŽAVA == "NOR")

# skoki nad 200m
nad200 <- filter(vsi_skoki, DOLŽINA >= 200)
nad200_drzave <- nad200 %>% group_by(DRŽAVA, LETO, SKAKALNICA) %>% summarise(n = n())
# skoki nad 220m
nad220 <- filter(vsi_skoki, DOLŽINA >= 220)
nad220_drzave <- nad220 %>% group_by(DRŽAVA, LETO) %>% summarise(n = n())

# pogoji slabši od 10 dodatka
pogoji_s3 <- filter(vsi_skoki, IZRAVNAVA >= 10)
# pogoji med 5 in 10
pogoji_s2 <- filter(vsi_skoki, IZRAVNAVA >= 5 & IZRAVNAVA < 10 )
# pogoji med 5 in 0
pogoji_s1 <- filter(vsi_skoki, IZRAVNAVA >= 0 & IZRAVNAVA < 5)
# pogoji med 0 in -5
pogoji_d1 <- filter(vsi_skoki, IZRAVNAVA >= -5 & IZRAVNAVA < 0)
# pogoji med -5 in -10
pogoji_d2 <- filter(vsi_skoki, IZRAVNAVA >= -10 & IZRAVNAVA < -5)
# pogoji boljši od -10
pogoji_d3 <- filter(vsi_skoki, IZRAVNAVA < -10)

#št. različnih tekmovalcev po posamezni državi
unikatni <- unique(vsi_skoki[,c("SKAKALEC","LETO","DRŽAVA")])
tekmovalci_po_drzavah  <- unikatni %>% group_by(DRŽAVA, LETO) %>% summarise(n = n())
tekmovalci_vecje_drzave <- filter(tekmovalci_po_drzavah, DRŽAVA %in% c("AUT", "SVN", "DEU", "NOR", "JPN"))

ggplot(tekmovalci_vecje_drzave,aes(x=LETO, y = n, col=as.factor(DRŽAVA))) +
  geom_line(size=1) +
  labs(x="leto", y="število različnih skakalcev", col="DRŽAVA")


######################
ocene_dolzine <- vsi_skoki[,c("DOLŽINA","OCENE SKUPAJ","HITROST")]
names(ocene_dolzine) <- c("DOLŽINA","OCENE","HITROST") 
ocene_dolzine$zaokroženo <- floor(ocene_dolzine$HITROST/4) * 4
ocene_dolzine$ponovitev <- ocene_dolzine$zaokroženo + 4
ocene_dolzine$ponovitev <- paste(ocene_dolzine$zaokroženo,ocene_dolzine$ponovitev, sep = " - ", collapse = NULL)

st_skokov <- vsi_skoki[, c("SKAKALEC", "DOLŽINA")]
skok_drzave <- merge(x <- st_skokov, skakalci_drzave, by = "SKAKALEC", all = TRUE)
skok_drzave <- cbind(skok_drzave, c(1))
names(skok_drzave) <- c("SKAKALEC", "DOLŽINA","DRŽAVA","ŠTEVILO")

skupna <- aggregate(DOLŽINA ~ DRŽAVA, data = skok_drzave, mean)

#vsota1 = vsi_skoki %>% group_by(SKAKALNICA, LETO) %>% summarise(skupaj = sum(DOLŽINA))
#vsota2 = vsi_skoki %>% group_by(SKAKALNICA, LETO) %>% summarise(n = n())
#tabela = merge(vsota1, vsota2, by= c("SKAKALNICA","LETO"))
#tabela$povprecje <- tabela$skupaj/tabela$n
tabela <- aggregate(DOLŽINA ~ SKAKALNICA + LETO,data = vsi_skoki, mean)

drzavni_rekordi2 <- merge(drzavni_rekordi,okrajsave2, by = "Country")
names(drzavni_rekordi2) <- c("Country", "metres", "place","iso_a2")

drzavni_rekordi_europa <- drzavni_rekordi2[,1:4]
drzave_celine <- unique(pred_for_map1[,c("sovereignt","continent","iso_a2")]) %>% filter(continent == "Europe")
drzavni_rekordi_europa <- merge(drzave_celine,drzavni_rekordi_europa, by = "iso_a2") 
drzavni_rekordi_europa <- drzavni_rekordi_europa[,4:6]
