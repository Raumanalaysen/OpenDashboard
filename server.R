# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script executes functions and defines the dynamic behavior of the OpenDashboard

function(input, output, session){
  
  # render attribute drop down menu
  output$att_sel_out <- renderUI({
    
    # get spatial data input
    this_sp <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  
  # render time slider
  output$time_sel_out <- renderUI({
    
    # get spatial data input
    this_att <- input$att_sel
    # this_att <- "Arbeitslose SGB-II/ SGB-III"
    
    # get available times
    pos <- which(tab_ov[, "att_nice"] == this_att)
    times <- as.numeric(tab_ov[pos, "time"])
    t.min <- min(times, na.rm = T)
    t.max <- max(times, na.rm = T)
    step_seq <- seq(t.min, t.max, length.out = length(pos))
    step <- step_seq[2] - step_seq[1]
    
    sliderInput(inputId = "time_sel", label = NULL,
                value = t.max, min = t.min, max = t.max, sep = "",
                step = step, ticks = F, animate = T, width = "90%")
    
  })
  
  
  # define reactive values
  reactVals <<- reactiveValues()
  this_sp <<- get("Stadtteil") # outcomment for dynamic behavior
  reactVals$sp <- this_sp
  this_dat <<- c(3591, 2242, 1249, 1963, 4607, 6188, 7139, 14552, 5729, 531, 1837, 1309, 5673, 5460, 1859) # outcomment for dynamic behavior
  reactVals$dat <- this_dat
  this_att <<- "gesamt"
  reactVals$att <- this_att
  this_time <<- "2017"
  reactVals$time <- this_time
  
  # use the following for dynamic behavior (there seems to be a problem with run time priorities)
  # observe({
  # 
  #   # get input
  #   this_sp_char <- input$sp_sel
  #   this_dat_char <- input$att_sel
  #   this_time_char <- input$time_sel
  #   # this_sp_char <- "Stadtteil"
  #   # this_dat_char <- "gesamt"
  #   # this_time_char <- "2017"
  # 
  #   # prepare spatial data and store as reactive values
  #   this_sp <<- get(this_sp_char)
  #   this_sp <<- get("Stadtteil") # outcomment for dynamic behavior
  #   reactVals$sp <- this_sp
  # 
  #   # prepare table data and store as reactive values
  #   this_att <- paste0(this_dat_char, "_timeSep_", this_time_char)
  #   this_dat <<- this_sp@data[,this_att]
  #   this_dat <<- c(3591, 2242, 1249, 1963, 4607, 6188, 7139, 14552, 5729, 531, 1837, 1309, 5673, 5460, 1859) # outcomment for dynamic behavior
  #   reactVals$dat <- this_dat
  #   reactVals$att <- this_dat_char
  #   reactVals$time <- this_time_char
  # 
  # })



  # create map
  output$map <- renderLeaflet({

    # get reactive values
    this_sp <- reactVals$sp
    this_dat <- reactVals$dat
    

    # define colors
    clInt <- classIntervals(this_dat, n = 5, style = "pretty")
    cols <- as.character(findColours(clInt, pal = c("lightblue", "blue")))

    # define legend labels
    leg_lab <- character(length(clInt$brks)-1)
    for (b in 2:length(clInt$brks)){
      leg_lab[b - 1] <- paste0(clInt$brks[b - 1], " - ", clInt$brks[b])
    }

    # create map object
    map <- leaflet() %>%

      # define boundaries
      fitBounds(lng1 = this_sp@bbox[1,1], lng2 = this_sp@bbox[1,2],
                lat1 = this_sp@bbox[2,1], lat2 = this_sp@bbox[2,2]) %>%

      # add legend
      addLegend(position = "bottomleft", colors = rev(sort(unique(cols))), label = leg_lab)


    # add polygons
    map <- addPolygons(map = map, data = Stadtteil, fillColor = cols, color = "white",
                       opacity = 1, fillOpacity = 1, weight = 3)

    map

  })


  # create barplot
  output$barplot_1 <- renderHighchart({
    
    # get reactive values
    this_sp <- reactVals$sp
    this_dat <- reactVals$dat
    this_att <- reactVals$att
    this_time <- reactVals$time
    
    # define theme
    thm <- hc_theme(
      colors = c('red', 'green', 'blue'),
      chart = list(style = list(fontFamily = "Ebrima")))
    
    # create barplot
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = this_sp@data[,conf_join[1, 1]]) %>%
      hc_yAxis(title = list(text = paste0(this_att, " (", this_time, ")"))) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(name = this_att, data = rev(sort(this_dat))) %>% 
      hc_title(text = "Ranking") %>%
      hc_colors(c('#FF6430', '#FF6430')) %>% 
      hc_add_theme(hc_theme(thm)) %>% 
      hc_add_theme(thm)
      chart
      
  })


  # create scatterplot
  output$scatterplot_1 <- renderPlot({

    # prepare data
    plot_x <- 2000:2018
    plot_y <- rnorm(19, 4000, 200)

    # set up graphic parameters
    par(mar = c(5.1, 4.1, 4.1, 1))

    # create scatterplot
    plot(x = plot_x, y = plot_y,
         main = "Einwohnerzahlen für den Stadtteil 'Thorr' (Mock-up-Daten!!!)",
         xlab = "Jahr", ylab = "Einwohnerzahl (Mock-up-Daten!!!)",
         type = "l", col = mycol_1, lwd = 5, axes = F)

    # add axes
    axis(1, at = 2000:2018)
    axis(2)

  })


  # quit session when browser is closed
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
  
}
