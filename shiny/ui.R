library(shiny)

shinyUI(fluidPage(
  titlePanel("Grafični prikaz smučarskih skokov"),
  
  tabsetPanel(
    tabPanel("napovedi državnih rekordov",
             sidebarPanel(#'Na spodnjem grafu lahko vidimo predikcijo glede skokov čez 200m na podlagi linearne regresije.' 
               radioButtons(inputId = 'skakalnica1', 
                            label = 'Skakalnica:', 
                            choices = unique(vsi_skoki$SKAKALNICA),
                            selected = 'Vikersund'),
               
               checkboxGroupInput(inputId = 'drzava1',
                                  label = 'Država:',
                                # choices = as.character(unique(vsi_skoki$DRŽAVA)),
                                  choiceValues = list("SVN", "AUT", "DEU","NOR","JPN","POL", "CZE"),
                                  choiceNames = list("Slovenia","Austria","Germany","Norway","Japan","Poland","Czech Republic"),
                                  selected = c("AUT","SVN")),
                                
               checkboxInput(inputId = "skupno1",
                             label = "vse države skupaj",
                             value = FALSE)
             ),
             mainPanel(plotOutput("napoved"))),

    
     tabPanel("sodniške ocene in njihov standardni odklon glede na dolžino skoka",
              sidebarPanel(
                #sliderInput('dolžina2', label='Izberi dolžino skoka (zaokroženo na 5m)',
                #            min=1, max=253.5, step=10, value = 150),
                sliderInput("leto2", label = "Izberi leto",
                            min = 2011, max = 2017, value = 2011),
                checkboxGroupInput(inputId = 'drzava2',
                                   label = 'Država',
                                   choiceValues = list("SVN", "AUT", "DEU","NOR","JPN","POL", "CZE"),
                                   choiceNames = list("Slovenia","Austria","Germany","Norway","Japan","Poland","Czech Republic"),
                                   selected = c("AUT","SVN")),
                radioButtons(inputId = 'skakalnica2', 
                             label = 'Skakalnica', 
                             choices = unique(vsi_skoki$SKAKALNICA),
                             selected = 'Vikersund')
                
                #checkboxInput(inputId = "skupno2",
                 #             label = "vse države skupaj",
                  #            value = FALSE)
              ),
              mainPanel(plotOutput("ocene"))),
    
    
     tabPanel("rezultat v seriji glede na skakalnico in pogoje + izravnavo",
              hr(),
              sidebarPanel(
                sliderInput('leto3', label='Izberi leto',
                            min=2011, max=2017, step=1, value = 2011),
                checkboxGroupInput(inputId ='skakalnica3', label = 'Izberi skakalnico', 
                                   choices = unique(vsi_skoki$SKAKALNICA),
                                   selected = 'Planica')
              ),
              mainPanel(plotOutput("rezultat"))),
     
     
     tabPanel("hitrost glede na skakalnico, državo in zaletno mesto",
              hr(),
              sidebarPanel(
                radioButtons(inputId = "skakalnica4",label = 'Izberi skakalnico', choices = list("Planica", "Vikersund", "Obersdorf", "Harrachov", "Kulm"),#unique(vsi_skoki$SKAKALNICA),
                             selected = 'Planica'),
                sliderInput(inputId = "leto4", label = "Izberi leto", 
                            min = 2011, max = 2017, value = 2011),
                checkboxGroupInput(inputId ='država4', label = 'Izberi državo', 
                                   choices = unique(vsi_skoki$DRŽAVA),
                                   selected = 'SVN'),
                checkboxInput(inputId = "skupno4",
                              label = "vse države skupaj",
                              value = FALSE)
              ),
              mainPanel(plotOutput("hitrost"))),
    

    
    tabPanel("državni rekordi po državah",
             hr(),
             sidebarPanel(
               sliderInput('rekord', label='Izberi dolžino',
                           min=0, max=253.5, step=0.5, value = 0)
             ),
             mainPanel(plotOutput("rekordi")))

  #tags$head(tags$style("#text1{color: red;
   #                              font-size: 20px;
    #                   font-style: italic;
     #                  }"))
   )
))