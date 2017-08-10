library(shiny)

shinyServer(function(input, output) {
  
  # 1. napoved bodočih državnih rekordov
  output$napoved <- renderPlot({
    if (input$skupno1 == FALSE){
      data1 <- filter(nad200_drzave, 
                    DRŽAVA %in% input$drzava1, 
                    SKAKALNICA %in% input$skakalnica1)
                   
      g1 <- ggplot(data1, aes(x = LETO, y = n, color = DRŽAVA)) +
        geom_point()
    
      g1 + xlim(2011, 2025) +
        geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
        xlab("Leto") + ylab("št. poletov čez 220m")
    } else {
      data1 <- filter(nad200_drzave, SKAKALNICA %in% input$skakalnica1) 
      g1 <- ggplot(data1, aes(x = LETO, y = n)) +
        geom_point()
      
      g1 + xlim(2011, 2025) +
        geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
        xlab("Leto") + ylab("št. poletov čez 220m")
    }
  })
  
  # 2. planica
  output$ocene <- renderPlot({
    data2 <- filter(vsi_skoki,
                    SKAKALNICA == input$skakalnica2,
                    DRŽAVA %in% input$drzava2,
                    LETO == input$leto2)

    data2$DOLŽINE_RAZREDI <- round(data2$DOLŽINA/5)*5
    names(data2)[names(data2) == "OCENE SKUPAJ"] <- "OCENE"
    data2 <- aggregate(OCENE ~ DOLŽINE_RAZREDI + DRŽAVA , data2, mean)
    
    ggplot(data2, aes(x = DOLŽINE_RAZREDI, y = OCENE, color = DRŽAVA)) +
      geom_line() +
      #geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
      geom_point()
  })
  
  # 3. rezultat glede na razmere + zalet
  output$rezultat <- renderPlot({
    data3 <- filter(vsi_skoki,
                    LETO == input$leto3,
                    SKAKALNICA == input$skakalnica3)
    data3 <- aggregate(REZULTAT ~ POGOJI_IZRAVNAVA + SKAKALNICA, data3, mean)
    
    ggplot(data3,aes(x = POGOJI_IZRAVNAVA, y = REZULTAT, color = SKAKALNICA))+
      geom_point() +
      geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
      geom_line()
  })
  
  # 4. hitrost pri odskoku v odvisnosti od zaleta
  output$hitrost <- renderPlot({
    if (input$skupno4 == FALSE){
      data4 <- filter(vsi_skoki, 
                      SKAKALNICA == input$skakalnica4,
                      DRŽAVA %in% input$država4,
                      LETO == input$leto4)
      data4 <- aggregate(HITROST ~ DRŽAVA + ZALET, data4, mean)
      ggplot(data = data4, aes(x = ZALET, y = HITROST, color = DRŽAVA)) +
        geom_line()+
        coord_equal()
    } else {
      data4 <- filter(vsi_skoki, 
                      SKAKALNICA == input$skakalnica4,
                      LETO == input$leto4)
      data4 <- aggregate(HITROST ~ SKAKALNICA + ZALET, data4, mean)
      ggplot(data = data4, aes(x = ZALET, y = HITROST)) +
        geom_line()+
        coord_equal()
    }
  })
  
  # 5. zemljevid
  output$rekordi <- renderPlot({
    
    data5 <- filter(pred_for_map1,
                    metres <= input$rekord,
                    continent == "Europe")
    ggplot() +
      geom_polygon(data = pred_for_map1, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
      geom_polygon(data = data5,aes(x = long, y = lat, group = group,fill = place), color = "black")+
      scale_fill_brewer(name = "place", palette = "Spectral", na.value = "grey50")+
      coord_quickmap(xlim = c(-20,40), ylim = c(35,70))  
  })
})



  # output$planica <- renderPlot({
  # data2 <- filter(vsi_skoki,
  #                 SKAKALNICA %in% input$skakalnica2,
  #                 POGOJI == input$pogoji2,
  #                 ZALET == input$zalet2)
  # #povprecje dolžin 30 mest, 29. mest ...
  # data2$POVPRECJE <- summarise(data2,by = DOLŽINA) / length(data2$IME)
  # g2 <- ggplot(data2,aes(x = LETO, y = DOLŽINA)) + 
  #   geom_point()