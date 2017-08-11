# 4. faza: Analiza in vizualizacija podatkov

######  Analiza podatkov

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


pogoji_s3 <- filter(vsi_skoki, IZRAVNAVA >= 10) # pogoji slabši od 10 dodatka
pogoji_s2 <- filter(vsi_skoki, IZRAVNAVA >= 5 & IZRAVNAVA < 10 ) # pogoji med 5 in 10
pogoji_s1 <- filter(vsi_skoki, IZRAVNAVA >= 0 & IZRAVNAVA < 5) # pogoji med 5 in 0
pogoji_d1 <- filter(vsi_skoki, IZRAVNAVA >= -5 & IZRAVNAVA < 0) # pogoji med 0 in -5
pogoji_d2 <- filter(vsi_skoki, IZRAVNAVA >= -10 & IZRAVNAVA < -5) # pogoji med -5 in -10
pogoji_d3 <- filter(vsi_skoki, IZRAVNAVA < -10) # pogoji boljši od -10

#št. različnih tekmovalcev po posamezni državi
unikatni <- unique(vsi_skoki[,c("SKAKALEC","LETO","DRŽAVA")])
tekmovalci_po_drzavah  <- unikatni %>% group_by(DRŽAVA, LETO) %>% summarise(n = n())
tekmovalci_vecje_drzave <- filter(tekmovalci_po_drzavah, DRŽAVA %in% c("AUT", "SVN", "DEU", "NOR", "JPN"))

graf.tekmovalci <- ggplot(tekmovalci_vecje_drzave,aes(LETO, n, fill = DRŽAVA)) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  labs(x="leto", y="število različnih skakalcev", col="DRŽAVA") +
  scale_fill_brewer("DRŽAVA", palette = "BrBG") +
  scale_x_continuous(breaks=seq(2011, 2017, 1)) + 
  theme(panel.background = element_rect(fill = 'khaki', colour = 'black'))


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



### Vizualizacija


# graf1 porazdelitev vseh skokov
porazdelitev <- ggplot(vsi_skoki,aes(x = DOLŽINA)) +
  geom_histogram(aes(y = ..density..), binwidth = 5,colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(x = "dolžina",y = "delež skokov") +
  scale_x_continuous(breaks  = seq(80,260,20))

# graf2 povprečen skok po letih in skakalnici
povprecen <- ggplot(tabela,aes(x = LETO, y = DOLŽINA, col = as.factor(SKAKALNICA))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "leto", y = "povrečna dolžina", col = "skakalnica")

#graf3 dolžina glede na pogoje
dolzina <- ggplot(data = za_grafe,aes(x = RAZREDI, y = povprečne_razmere, col = za_grafe$SKAKALNICA),alpha = 0.01) +
  scale_color_manual(name = "Skakalnica", values = c("Vikersund" = "royalblue", "Planica" = "green4","Kulm" = "orange","Obersdorf" = "gold","Harrachov" = "red")) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "dolžina", y = "pribitek/odbitek točk za razmere", col = "skakalnica")

# graf4 števila tekmovanj po letih
st_tekem <- ggplot(tekmovanje,aes(LETO, st_tekem, fill = SKAKALNICA)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_fill_brewer("SKAKALNICA", palette = "RdGy") +
  ylab("število tekem v sezoni")+ 
  scale_y_continuous(breaks = c(0,2,4,6,8)) +
  scale_x_continuous(breaks = seq(2011,2017,1))

# zemljevidi
# Državni rekordi po skakalnicah

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", 
                          encoding = "UTF-8") %>%
  pretvori.zemljevid() 

svet$iso_a2 <- as.character(svet$iso_a2)
drzavni_rekordi2$iso_a2 <- as.character(drzavni_rekordi2$iso_a2)
pred_for_map1 <- left_join(svet, drzavni_rekordi2, by = "iso_a2")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank())

# svet

zemljevid1 <- ggplot() +
  geom_polygon(data = pred_for_map1, aes(x = long, y = lat, group = group, fill = place)) +
  scale_fill_brewer(name = "SKAKALNICA", palette = "Spectral", na.value = "grey50") +
  coord_quickmap(xlim = c(-153,180), ylim = c(-55,90)) +
  ditch_the_axes


# samo evropa, tudi glede na dolžino skoka
# obroba pove skakalnico, transparentnost pa daljavo

#drzavni_rekordi_europa <- drzavni_rekordi2[,1:4]
#drzave_celine <- unique(pred_for_map1[,c("sovereignt","continent","iso_a2")]) %>% filter(continent == "Europe")
#drzavni_rekordi_europa <- merge(drzave_celine,drzavni_rekordi_europa, by = "iso_a2") 
#drzavni_rekordi_europa <- drzavni_rekordi_europa[,4:6]


zemljevidEuropa <- ggplot(data = pred_for_map1) +
  geom_polygon(aes(x = long, y = lat, fill = metres, group = group)) +   
  scale_fill_gradient2(name = "DRŽAVNI REKORD",
                       low = "cadetblue2", mid = "white", high = "green4", midpoint = 170, 
                       space = "Lab", na.value = "grey50", guide = "colourbar") + 
  geom_path(data = pred_for_map1,  
            aes(x = long, y = lat, group = group, colour=pred_for_map1$place), 
            size = 1) +
  scale_color_manual(name = "SKAKALNICA",
                     breaks = c("Planica", "Vikersund", "Kulm", "Harrachov", "ostalo"),
                     values = c("Planica" = "chocolate1", "Vikersund" = "orangered", "Kulm" = "gold1", "Harrachov" = "green", "ostalo" = "orchid4"))+ 
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72)) 
  

  

