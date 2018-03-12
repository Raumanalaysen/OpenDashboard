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
    # this_sp <- "Stadtteil"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  # render attribute drop down menu for gauge 1
  output$att_sel_out_gauge1 <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel_gauge1", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  # render attribute drop down menu for gauge 2
  output$att_sel_out_gauge2 <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel_gauge2", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  # render attribute drop down menu for gauge 3
  output$att_sel_out_gauge3 <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel_gauge3", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  # render attribute drop down menu for gauge 4
  output$att_sel_out_gauge4 <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel_gauge4", label = NULL, selectize = T, choices = sort(av_atts))
    
  })
  
  # render attribute drop down menu for gauge 5
  output$att_sel_out_gauge5 <- renderUI({
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    selectInput(inputId = "att_sel_gauge5", label = NULL, selectize = T, choices = sort(av_atts))
    
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
    # this_sp_char <- "Stadtgebiet"
    # this_sp_char <- "Sozialraum"
    # this_sp_char <- "Stadtteil"
    this_att <- input$att_sel
    # this_att <- "Arbeitslose SGB-II/ SGB-III"
    # this_att <- "SGB-II-11 bis unter 15 Jahre" #################### Error with this parameter: Needs revision!!!
    # this_att <- "0-5Jahre"
    this_time <- input$time_sel
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    # get values for current attribute for all time steps
    pos <- which(substr(colnames(this_sp@data), 1, nchar(this_att)) == this_att)
    pos <- pos[which(substr(colnames(this_sp@data[pos]), nchar(this_att) + 1, nchar(this_att) + 1) == "_")]
    this_dat_allT <- as.vector(as.matrix(this_sp@data[,pos]))
    
    # detect and convert percentage values
    perc <- F
    if (all(all(this_dat_allT <= 1), all(this_dat_allT >= 0))){
      perc <- T
      this_dat <- this_dat * 100
      this_dat_allT <- this_dat_allT * 100
    }
    
    
    # define class breaks, colors and legend labels
    if (length(this_dat) > 1){
      
      # define map colors
      colBins <- colorBin(brewer.pal(5, "Blues"), domain = this_dat_allT, bins = 5, pretty = T)
      cols <- colBins(this_dat)
      
      # define legend labels
      bins <- attr(colBins, "colorArgs")$bins
      leg_lab <- c()
      for (b in 2:length(bins)){
        if (perc){
          leg_lab <- c(leg_lab, paste(bins[b - 1], "% -", bins[b], "%", sep = " "))
        } else {
          leg_lab <- c(leg_lab, paste(bins[b - 1], "-", bins[b], sep = " "))
        }
      }
      
      # define legend colors
      leg_cols <- colBins(bins[-1] - 0.000000001)
      
    } else {
      
      # define map colors
      cols <- "blue"
      
      # define legend labels
      leg_lab <- round(this_dat, digits = 2)
      if (perc) leg_lab <- paste(leg_lab, "%", sep = " ")
      
      # define legend colors
      leg_cols <- "blue"
    }
    
    
    # create map object
    map <- leaflet() %>%

      # define boundaries
      fitBounds(lng1 = this_sp@bbox[1,1], lng2 = this_sp@bbox[1,2],
                lat1 = this_sp@bbox[2,1], lat2 = this_sp@bbox[2,2]) %>%

      # add legend
      addLegend(position = "bottomleft", colors = leg_cols, label = leg_lab)


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
    # this_sp_char <- "Stadtteil"
    # this_att <- "Einwohner"
    # this_time <- "2017"
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    # detect and convert percentage values
    perc <- F
    this_att_show <- this_att
    if (all(all(this_dat <= 1), all(this_dat >= 0))){
      perc <- T
      this_dat <- this_dat * 100
      this_att_show <- paste(this_att, "in %", sep = " ")
    }
    
    # prepare data
    # o <- rev(order(this_dat))
    # plot_x <- paste0(1:length(this_dat), " ", this_sp@data[,conf_join[1, 1]][o])
    # plot_y <- this_dat[o]
    # 
    # # create barplot
    # b_p <- plot_ly(x = plot_x,
    #                y = plot_y,
    #                type = "bar")
    # b_p
    
    
    # define theme
    thm <- hc_theme(
      colors = c('red', 'green', 'blue'),
      chart = list(style = list(fontFamily = "Ebrima")))

    # create barplot
    chart <- highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = this_sp@data[,conf_join[1, 1]]) %>%
      hc_yAxis(title = list(text = paste0(this_att_show, " (", this_time, ")"))) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(name = this_att_show, data = round(rev(sort(this_dat)), digits = 3)) %>%
      hc_title(text = "Ranking") %>%
      hc_colors(c('#FF6430', '#FF6430')) %>%
      hc_add_theme(hc_theme(thm)) %>%
      hc_add_theme(thm)
      chart
      
  })


  # create scatterplot
  output$scatterplot_1 <- renderPlotly({

    # get reactive values
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
    this_time <- input$time_sel
    # this_sp_char <- "Stadtteil"
    # this_att <- "Einwohner"
    # this_time <- "2017"
    
    this_sp <- get(this_sp_char)
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    # prepare data
    plot_x <- 2000:2018
    plot_y <- round(rnorm(19, 4000, 200), 0)
    plot_data <- data.frame(plot_x, plot_y)

    # create scatterplot
    par(xpd = T)
    sc_p <- plot_ly(plot_data, x = plot_x) %>%
      add_lines(y = plot_y, line = list(shape = "spline")) %>% 
      layout(title = "Zeitliche Entwicklung im gewählten Gebiet",
             xaxis = list(title = "Zeit in Jahren"), yaxis = list(title = this_att))
    sc_p

  })

  # create gauges
  output$gauge1 <- renderGauge({
    gauge(87, min = 0, max = 100, label = "",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge2 <- renderGauge({
    gauge(32, min = 0, max = 100, label = "",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge3 <- renderGauge({
    gauge(44, min = 0, max = 100, label = "",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge4 <- renderGauge({
    gauge(10, min = 0, max = 100, label = "",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  output$gauge5 <- renderGauge({
    gauge(55, min = 0, max = 100, label = "",
          gaugeSectors(success = c(100, 66), warning = c(65, 33), danger = c(32, 0)))
  })
  
  
  # add logo
  output$logo <- renderImage({
    
    list(src = logoPath, alt = "Raumanalysen - Christian Müller")
    
  }, deleteFile = F)
  
  
  
  # quit session when browser is closed
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
  
}
