# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script configures the user interface of the OpenDashboard

# create shiny page
shinyUI(
  fluidPage(theme = "odStyle.css",
    
    # create navigation bar page
    navbarPage("OpenDashboard", id = "nav",
               
               # create tab panel
               tabPanel("Datenvisualisierung", 
               
                 # data selection division
                 div(class = "controls", align = "center",
                     
                     # data selection panel
                     absolutePanel(id = "dat_sel", class = "panel panel-default", fixed = TRUE,
                                   draggable = T, top = 60, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 250,
                                   
                                   # select spatial entity
                                   h5("Räumliche Ebene", align = "center"),
                                   selectInput(inputId = "sp_sel", label = NULL, choices = conf_sp[,2], selectize = T),
                                   
                                   # select attribute
                                   h5("Kenngröße", align = "center"),
                                   selectInput(inputId = "att_sel", label = NULL, selectize = T, choices = sort(c(colnames(Einwohnerdaten_2017),
                                                                                                                    colnames(BundesagenturFuerArbeit_2015),
                                                                                                                    colnames(BundesagenturFuerArbeit_2017)))),
                                   
                                   # select time
                                   h5("Zeit", align = "center"),
                                   sliderInput(inputId = "time_sel", label = NULL,
                                               value = as.Date("01/01/2017", format = "%d/%m/%Y"),
                                               min = as.Date("01/01/2000", format = "%d/%m/%Y"),
                                               max = as.Date("01/01/2018", format = "%d/%m/%Y"),
                                               timeFormat = "%Y", step = 1, ticks = F, animate = T, width = "90%")
                                   
                                   
                                   
                     )
                     
                 ),
                 
                 
                 # map division
                 div(class = "controls", align = "center",
                     
                     # map panel
                     absolutePanel(id = "map_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = T, top = 320, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 600,
                                   
                                   # map output
                                   leafletOutput("map", width = "100%", height = 600)
                                   
                     )
                     
                 ),
                 
                 
                 # barplot division
                 div(class = "controls", align = "center",
                     
                     # barplot panel
                     absolutePanel(id = "bp_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = T, top = 60, left = 615, right = 20, bottom = "auto",
                                   width = 600, height = 425,
                                   
                                   # barplot output
                                   plotOutput('barplot_1')
                                   
                     )
                     
                 ),
                 
                 
                 # scatterplot division
                 div(class = "controls", align = "center",
                     
                     # scatterplot panel
                     absolutePanel(id = "scp_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = T, top = 495, left = 615, right = 20, bottom = "auto",
                                   width = 600, height = 425,
                                   
                                   # scatterplot output
                                   plotOutput('scatterplot_1')
                                   
                                   
                                   
                                   
                                   
                     )
                     
                 )
                 
               )
               
    )
    
  )
)