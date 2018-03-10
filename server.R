# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script executes functions and defines the dynamic behavior of the OpenDashboard

function(input, output, session){
  
  # render attribute drop down menu
  output$att_sel_out <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
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
  
  
  # create map
  output$map <- renderLeaflet({

    # get reactive values
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
    this_time <- input$time_sel
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    
    # define class breaks, colors and legend labels
    if (length(this_dat) > 1){
      
      ### issues with colors for classes which do not occur in data
      ### also consider all time steps in classification
      clInt <- classIntervals(this_dat, n = 5, style = "pretty")
      cols <- as.character(findColours(clInt, pal = c("lightblue", "blue")))
      for (b in 2:length(clInt$brks)){
        leg_lab[b - 1] <- paste0(clInt$brks[b - 1], " - ", clInt$brks[b])
      }
      
    } else {
      clInt <- as.character(this_dat)
      cols <- "blue"
      leg_lab <- clInt
    }
    
    # define legend labels
    
    # create map object
    map <- leaflet() %>%

      # define boundaries
      fitBounds(lng1 = this_sp@bbox[1,1], lng2 = this_sp@bbox[1,2],
                lat1 = this_sp@bbox[2,1], lat2 = this_sp@bbox[2,2]) %>%

      # add legend
      addLegend(position = "bottomleft", colors = rev(sort(unique(cols))), label = leg_lab)


    # add polygons
    map <- addPolygons(map = map, data = this_sp, fillColor = cols, color = "white",
                       opacity = 1, fillOpacity = 1, weight = 3)

    map

  })


  # create barplot
  output$barplot_1 <- renderHighchart({
    
    # get reactive values
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
    this_time <- input$time_sel
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
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
      hc_add_series(name = this_att, data = round(rev(sort(this_dat)), digits = 3)) %>% 
      hc_title(text = "Ranking") %>%
      hc_colors(c('#FF6430', '#FF6430')) %>% 
      hc_add_theme(hc_theme(thm)) %>% 
      hc_add_theme(thm)
      chart
      
  })


  # create scatterplot
  output$scatterplot_1 <- renderPlot({

    # get reactive values
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
    this_time <- input$time_sel
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
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

  # create gauges
  output$gauge1 <- renderGauge({
    gauge(87, min = 0, max = 100, label = "Einwohner",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge2 <- renderGauge({
    gauge(32, min = 0, max = 100, label = "Familien mit mehr als 3 Kindern",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge3 <- renderGauge({
    gauge(44, min = 0, max = 100, label = "Grundsicherung im Alter",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge4 <- renderGauge({
    gauge(10, min = 0, max = 100, label = "unter 10-Jährige",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge5 <- renderGauge({
    gauge(55, min = 0, max = 100, label = "SGB-II-Empfänger",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  
  # quit session when browser is closed
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
  
}
