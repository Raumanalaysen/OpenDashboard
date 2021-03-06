# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script configures the user interface of the OpenDashboard

# create shiny page
shinyUI(
  
  fluidPage(theme = "odStyle.css",
            # useShinyjs(),
            
    # create navigation bar page
    navbarPage("OpenDashboard", id = "nav",
               
               # create tab panel ----
               tabPanel("Datenvisualisierung", style = 'width: 1400px; height: 1150px',
                        
                 # data selection division ----
                 div(id = "data_selection_division", class = "controls", align = "center",
                     
                     # data selection panel
                     absolutePanel(id = "dat_sel", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 60, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 250,
                                   
                                   
                                   # logo panel
                                   absolutePanel(id = "dat_sel_inner", class = "panel panel-default", fixed = F,
                                                 draggable = F, top = 35, left = 35, right = 20, bottom = "auto",
                                                 width = 108, height = 108,
                                                 
                                                 # link to webpage
                                                 a(href = "https://raumanalysen.de",
                                                   
                                                   # logo output
                                                   plotOutput('logo')
                                                   
                                                 )
                                   ),
                                   
                                   # select spatial entity
                                   h5("Räumliche Ebene & Kenngrößen", align = "center"),
                                   selectInput(inputId = "sp_sel", label = NULL, choices = conf_sp[,2], selectize = T),
                                   
                                   
                                   # select attribute
                                   uiOutput("att_sel_out"),
                                   
                                   # select time
                                   uiOutput("time_sel_out")
                                   
                                   
                                   
                                   
                     )
                     
                 ),
                 
                 
                 # map division ----
                 div(id = "map_division", class = "controls", align = "center",
                     
                     # map panel
                     absolutePanel(id = "map_pan", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 320, left = 5, right = 20, bottom = "auto",
                                   width = 600, height = 600,
                                   
                                   # map output
                                   leafletOutput("map", width = "100%", height = 600)
                     ),
                                   
                                   
                     absolutePanel(id = "trans_pan", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 332, left = 490, right = 20, bottom = "auto",
                                   width = 50, height = 47,
                                   
                                   # transparent option
                                   sliderInput(inputId = "trans_sel", label = NULL,
                                               value = 1, min = 0, max = 1, sep = "",
                                               ticks = F, animate = F, width = "90%")
                                   
                     )
                     
                 ),
                 
                 
                 # barplot division ----
                 div(id = "barplot_division", class = "controls", align = "center",
                     
                     # barplot panel
                     absolutePanel(id = "bp_pan", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 60, left = 615, right = 20, bottom = "auto",
                                   width = 800, height = 425,
                                   
                                   # barplot output
                                   plotlyOutput('barplot_1')
                                   
                     )
                     
                 ),
                 
                 
                 # scatterplot division ----
                 div(id = "scatterplot_division", class = "controls", align = "center",
                     
                     # scatterplot panel
                     absolutePanel(id = "scp_pan", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 495, left = 615, right = 20, bottom = "auto",
                                   width = 800, height = 425,
                                   
                                   # scatterplot output
                                   p(),
                                   plotlyOutput('scatterplot_1', height = "400px")
                                   
                                   
                     )
                     
                 ),
                 
                 
                 
                 # gauges selector division ----
                 div(id = "gauges_selector_division", class = "controls", align = "center",
                     
                     
                     # gauge panels
                     absolutePanel(id = "gauge_sel1", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 930, left = 5, right = 20, bottom = "auto",
                                   width = 278, height = 35,
                                   
                                   # gauge selector
                                   uiOutput("att_sel_out_gauge1")
                                   
                     ),
                     
                     absolutePanel(id = "gauge_sel2", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 930, left = 288, right = 20, bottom = "auto",
                                   width = 278, height = 35,
                                   
                                   # gauge selector
                                   uiOutput("att_sel_out_gauge2")# gauge output
                                   
                     ),
                     
                     absolutePanel(id = "gauge_pan3", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 930, left = 571, right = 20, bottom = "auto",
                                   width = 278, height = 35,
                                   
                                   # gauge selector
                                   uiOutput("att_sel_out_gauge3")
                                   
                     ),
                     
                     absolutePanel(id = "gauge_pan4", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 930, left = 854, right = 20, bottom = "auto",
                                   width = 278, height = 35,
                                   
                                   # gauge selector
                                   uiOutput("att_sel_out_gauge4")
                                   
                     ),
                     
                     absolutePanel(id = "gauge_pan5", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 930, left = 1137, right = 20, bottom = "auto",
                                   width = 278, height = 35,
                                   
                                   # gauge selector
                                   uiOutput("att_sel_out_gauge5")
                                   
                     )
                     
                 ),
                 
                 
                 # gauges division ----
                 # div(id = "gauges_division", class = "controls", align = "center",
                 #     
                 #     
                 #     # gauge panels
                 #     absolutePanel(id = "gauge_pan1", class = "panel panel-default", fixed = F,
                 #                   draggable = F, top = 990, left = 5, right = 20, bottom = "auto",
                 #                   width = 278, height = 115,
                 #                   
                 #                   # gauge output
                 #                   gaugeOutput("gauge1", width = "100%", height = 110)
                 #                   
                 #     ),
                 #     absolutePanel(id = "gauge_pan2", class = "panel panel-default", fixed = F,
                 #                   draggable = F, top = 990, left = 288, right = 20, bottom = "auto",
                 #                   width = 278, height = 115,
                 #                   
                 #                   # gauge output
                 #                   gaugeOutput("gauge2", width = "100%", height = 110)
                 #     ),
                 #     absolutePanel(id = "gauge_pan3", class = "panel panel-default", fixed = F,
                 #                   draggable = F, top = 990, left = 571, right = 20, bottom = "auto",
                 #                   width = 278, height = 115,
                 #                   
                 #                   # gauge output
                 #                   gaugeOutput("gauge3", width = "100%", height = 110)
                 #     ),
                 #     absolutePanel(id = "gauge_pan4", class = "panel panel-default", fixed = F,
                 #                   draggable = F, top = 990, left = 854, right = 20, bottom = "auto",
                 #                   width = 278, height = 115,
                 #                   
                 #                   # gauge output
                 #                   gaugeOutput("gauge4", width = "100%", height = 110)
                 #     ),
                 #     absolutePanel(id = "gauge_pan5", class = "panel panel-default", fixed = F,
                 #                   draggable = F, top = 990, left = 1137, right = 20, bottom = "auto",
                 #                   width = 278, height = 115,
                 #                   
                 #                   # gauge output
                 #                   gaugeOutput("gauge5", width = "100%", height = 110)
                 #     )
                 #     
                 # ),
                 
                 
                 # gauges weighting division ----
                 div(id = "gauges_weighting_division", class = "controls", align = "center",
                     
                     
                     # gauge weighting panels
                     absolutePanel(id = "gauge_w1", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 980, left = 5, right = 20, bottom = "auto",
                                   width = 278, height = 110,
                                   
                                   # gauge weighting
                                   sliderInput(inputId = "g_w1", label = "Gewichtung", value = 0, min = -1, max = 1, step = 0.05, width = "90%")
                                   
                     ),
                     absolutePanel(id = "gauge_w2", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 980, left = 288, right = 20, bottom = "auto",
                                   width = 278, height = 110,
                                   
                                   # gauge weighting
                                   sliderInput(inputId = "g_w2", label = "Gewichtung", value = 0, min = -1, max = 1, step = 0.05, width = "90%")
                     ),
                     absolutePanel(id = "gauge_w3", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 980, left = 571, right = 20, bottom = "auto",
                                   width = 278, height = 110,
                                   
                                   # gauge weighting
                                   sliderInput(inputId = "g_w3", label = "Gewichtung", value = 0, min = -1, max = 1, step = 0.05, width = "90%")
                     ),
                     absolutePanel(id = "gauge_w4", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 980, left = 854, right = 20, bottom = "auto",
                                   width = 278, height = 110,
                                   
                                   # gauge weighting
                                   sliderInput(inputId = "g_w4", label = "Gewichtung", value = 0, min = -1, max = 1, step = 0.05, width = "90%")
                     ),
                     absolutePanel(id = "gauge_w5", class = "panel panel-default", fixed = F,
                                   draggable = F, top = 980, left = 1137, right = 20, bottom = "auto",
                                   width = 278, height = 110,
                                   
                                   # gauge weighting
                                   sliderInput(inputId = "g_w5", label = "Gewichtung", value = 0, min = -1, max = 1, step = 0.05, width = "90%")
                     )
                     
                 )
                 
                 # div(id = "loading_div", class = "controls",
                 #     absolutePanel(id = "loading_panel", class = "loading", fixed = F,
                 #                   draggable = T, top = 0, left = 0, right = 20, bottom = "auto",
                 #                   width = 1400, height = 1150,
                 # 
                 #                   plotOutput('loading')))

               ),
               
            
               
               # create close session panel ----
               tabPanel(id = "close_session_panel", "Beenden", style = 'width: 1400px; height: 1150px',
                
                        # close button
                        tags$button(id = 'close', type = "button", class = "btn action-button",
                          onclick = "setTimeout(function(){window.close();},500);", "Sitzung beenden")
                                
               )
               
    )
    
  )
)