# 3. faza: Vizualizacija podatkov


# graf1 porazdelitev vseh skokov
porazdelitev <- ggplot(vsi_skoki,aes(x = DOLŽINA)) +
  geom_histogram(aes(y = ..density..), binwidth = 5,colour = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666") +
  labs(x = "dolžina",y = "delež skokov") +
  scale_x_continuous(breaks = number_ticks(10))

# graf2 povprečen skok po letih in skakalnici
povprecen <- ggplot(tabela,aes(x = LETO, y = DOLŽINA, col = as.factor(SKAKALNICA))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "leto", y = "povrečna dolžina", col = "skakalnica")

#graf3 dolžina glede na pogoje
dolzina <- ggplot(data = za_grafe,aes(x = RAZREDI, y = povprečne_razmere, col = za_grafe$SKAKALNICA),alpha = 0.01) +
  scale_color_manual(name = "Skakalnica", values = c("Vikersund" = "royalblue", "Planica" = "green4","Kulm" = "orange","Obersdorf" = "gold","Harrachov" = "red")) +
  geom_line() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "dolžina", y = "pribitek/odbitek točk za razmere", col = "skakalnica")

# graf4 števila tekmovanj po letih
st_tekem <- ggplot(tekmovanje,aes(LETO,st_tekem, fill=SKAKALNICA)) +
  geom_bar(stat="identity",width = 0.4) +
  scale_fill_brewer("SKAKALNICA", palette = "RdGy") +
  ylab("število tekem v sezoni")+ 
  scale_y_continuous(breaks= c(0,2,4,6,8)) +
  scale_x_continuous(breaks=seq(2011, 2017, 1))

#graf5 rezultat v seriji glede na pogoje

#graf6 ocene glede na pogoje

# zemljevidi
# Državni rekordi po skakalnicah

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", 
                          encoding = "UTF-8") %>%
      pretvori.zemljevid() #%>% 
      #filter(continent == "Europe")

pred_for_map1 <- left_join(svet, drzavni_rekordi2, by = c("iso_a2" = "iso_a2"))

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

zemljevid1


# samo evropa, tudi glede na dolžino skoka
# obroba pove skakalnico, transparentnost pa daljavo

zemljevidEuropa <- ggplot() +
  geom_polygon(data = pred_for_map1, 
               aes(x = long, y = lat, group = group, fill = metres)) + 
  scale_fill_gradient2(name = "DRŽAVNI\nREKORD",
                      low = "cadetblue2", mid = "white", high = "green4", midpoint = 170, 
                      space = "Lab", na.value = "grey50", guide = "colourbar") +
                      
  geom_path(data = pred_for_map1,  
            aes(x = long, y = lat, group = group, colour=pred_for_map1$place), 
            size = 1) +
  scale_color_manual(name = "SKAKALNICA",
                    breaks = c("Planica", "Vikersund", "Kulm", "Harrachov", "ostalo"),
                    values = c("Planica" = "chocolate1", "Vikersund" = "orangered", "Kulm" = "gold1", "Harrachov" = "green", "ostalo" = "orchid4"))+ 
  coord_quickmap(xlim = c(-25, 40), ylim = c(32, 72))
  
zemljevidEuropa 

  

