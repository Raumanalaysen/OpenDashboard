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
                                   draggable = F, top = 60, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 250,
                                   
                                   # select spatial entity
                                   h5("Räumliche Ebene", align = "center"),
                                   selectInput(inputId = "sp_sel", label = NULL, choices = conf_sp[,2], selectize = T),
                                   
                                   # select attribute
                                   h5("Kenngröße", align = "center"),
                                   uiOutput("att_sel_out"),
                                   
                                   # select time
                                   h5("Zeit", align = "center"),
                                   uiOutput("time_sel_out")
                                   
                                   
                                   
                     )
                     
                 ),
                 
                 
                 # map division
                 div(class = "controls", align = "center",
                     
                     # map panel
                     absolutePanel(id = "map_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 320, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 600,
                                   
                                   # map output
                                   leafletOutput("map", width = "100%", height = 600)
                                   
                     )
                     
                 ),
                 
                 
                 # barplot division
                 div(class = "controls", align = "center",
                     
                     # barplot panel
                     absolutePanel(id = "bp_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 60, left = 615, right = 20, bottom = "auto",
                                   width = 600, height = 425,
                                   
                                   # barplot output
                                   highchartOutput('barplot_1')
                                   
                     )
                     
                 ),
                 
                 
                 # scatterplot division
                 div(class = "controls", align = "center",
                     
                     # scatterplot panel
                     absolutePanel(id = "scp_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 495, left = 615, right = 20, bottom = "auto",
                                   width = 600, height = 425,
                                   
                                   # scatterplot output
                                   plotOutput('scatterplot_1')
                                   
                                   
                                   
                                   
                                   
                     )
                     
                 ),
                 
                 # logo division
                 div(class = "controls", align = "center",
                     
                     # logo panel
                     absolutePanel(id = "logo_pan", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 5, right = 20, bottom = "auto",
                                   width = 190, height = 120
                                   
                                   # logo output
                                   
                                   
                     )
                     
                 ),
                 
                 # gauges division
                 div(class = "controls", align = "center",
                     
                     # gauge panels
                     absolutePanel(id = "gauge_pan1", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 203, right = 20, bottom = "auto",
                                   width = 200, height = 120,
                                   
                                   # gauge output
                                   gaugeOutput("gauge1", width = "100%", height = 120)
                                   
                     ),
                     absolutePanel(id = "gauge_pan2", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 406, right = 20, bottom = "auto",
                                   width = 200, height = 120,
                                   
                                   # gauge output
                                   gaugeOutput("gauge2", width = "100%", height = 120)
                     ),
                     absolutePanel(id = "gauge_pan3", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 609, right = 20, bottom = "auto",
                                   width = 200, height = 120,
                                   
                                   # gauge output
                                   gaugeOutput("gauge3", width = "100%", height = 120)
                     ),
                     absolutePanel(id = "gauge_pan4", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 812, right = 20, bottom = "auto",
                                   width = 200, height = 120,
                                   
                                   # gauge output
                                   gaugeOutput("gauge4", width = "100%", height = 120)
                     ),
                     absolutePanel(id = "gauge_pan5", class = "panel panel-default", fixed = TRUE,
                                   draggable = F, top = 930, left = 1015, right = 20, bottom = "auto",
                                   width = 200, height = 120,
                                   
                                   # gauge output
                                   gaugeOutput("gauge5", width = "100%", height = 120)
                     )
                     
                 )
                 
               )
               
    )
    
  )
)