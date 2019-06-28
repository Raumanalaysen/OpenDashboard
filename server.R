# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script executes functions and defines the dynamic behavior of the OpenDashboard

function(input, output, session){
  
  # render attribute drop down menu
  output$att_sel_out <- renderUI({
    
    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get spatial data input
    this_sp_char <- input$sp_sel
    # this_sp <- "Stadtgebiet"
    # this_sp <- "Stadtteil"
    
    # get available attributes
    this_dat <- get(this_sp_char)@data
    av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
    av_atts <- av_atts[av_atts %in% all_atts]
    
    # create drop down menu
    if (last_att == ""){
      selectInput(inputId = "att_sel", label = NULL, selectize = T, choices = sort(av_atts))
    } else {
      selectInput(inputId = "att_sel", label = NULL, selectize = T, choices = sort(av_atts), selected = last_att)
    }
    
  })
  
  # remember last selected attribute
  observeEvent(input$att_sel, last_att <<- input$att_sel)
  
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
    # this_att <- "OpenDashboard Indikator"
    
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

    # refresh if indicator has been recalculated
    act <- ind_act$act
    
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
    if (this_att != "OpenDashboard Indikator"){
      if (all(all(this_dat_allT <= 1), all(this_dat_allT >= 0))){
        perc <- T
        this_dat <- this_dat * 100
        this_dat_allT <- this_dat_allT * 100
      }
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
    
    # make colors globally available
    cols <<- cols
    leg_cols <<- leg_cols
    
    
    
    # create map object
    map <- leaflet() %>%

      # define boundaries
      fitBounds(lng1 = this_sp@bbox[1,1], lng2 = this_sp@bbox[1,2],
                lat1 = this_sp@bbox[2,1], lat2 = this_sp@bbox[2,2]) %>%

      # add legend
      addLegend(position = "bottomleft", colors = leg_cols, label = leg_lab) %>% 


      # add polygons
      addPolygons(data = this_sp, layerId = ~id, fillColor = cols, color = "white",
                  opacity = 1, fillOpacity = 1, weight = 3, label = this_sp@data[,2],
                  group = "not_highlighted")


  })
  
  
  # observe selected objects and highlight in map
  observe({
    
    # get spatial objects
    this_sp <- get(input$sp_sel)
    
    # get selected ids, objects and colors
    if (length(sel_ob$sel) > 0){
      pos <- which(this_sp$id %in% sel_ob$sel)
      use_sp <- this_sp[pos,]
      use_cols <- cols[pos]
      nuse_sp <- this_sp[-pos,]
      nuse_cols <- cols[-pos]
    
      # create map proxy and clear shapes
      map_proxy <- leafletProxy("map")
      map_proxy %>% clearShapes()
      
      # add not highlighted polygons  
      if (nrow(nuse_sp@data) > 0){
        map_proxy %>% addPolygons(data = nuse_sp, layerId = ~id, fillColor = nuse_cols, color = "white",
                                  opacity = 1, fillOpacity = 1, weight = 3, label = nuse_sp@data[,2],
                                  group = "not_highlighted")
      }
      
      # add highlighted polygons
      map_proxy %>% addPolygons(data = use_sp, layerId = ~id, fillColor = use_cols, color = myred,
                                opacity = 1, fillOpacity = 1, weight = 3, label = use_sp@data[,2],
                                group = "highlighted")
      
    }
    
  })
  
  # redraw map if no object is selected
  observeEvent(sel_ob$sel, {
    if (length(sel_ob$sel) == 0){
      this_sp <- get(input$sp_sel)
      leafletProxy("map") %>% 
        clearShapes() %>% 
        addPolygons(data = this_sp, layerId = ~id, fillColor = cols, color = "white",
                    opacity = 1, fillOpacity = 1, weight = 3, label = this_sp@data[,2],
                    group = "not_highlighted")
    }
  })
  
  # observe spatial unit selection, clear selected objects
  observeEvent(input$sp_sel, {
    sel_ob$sel <- NULL
    a_clicked_names$names <- NULL
  })
  
  
  # observe clicked objects on the map
  observeEvent(input$map_shape_click, {
    shape_clicked <- input$map_shape_click$id
    if (shape_clicked %in% sel_ob$sel){
      hit_pos <- which(sel_ob$sel == shape_clicked)
      sel_ob$sel <- sel_ob$sel[-hit_pos]
    } else {
      sel_ob$sel <- c(sel_ob$sel, shape_clicked)
    }
  })


  # create barplot
  output$barplot_1 <- renderPlotly({
    
    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get reactive values
    this_sp <- get(input$sp_sel)
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
    this_time <- input$time_sel
    sel <- sel_ob$sel
    # this_sp <- get("Stadtteil")
    # this_att <- "Einwohner"
    # this_att <- "OpenDashboard Indikator"
    # this_time <- "2017"
    # sel <- c("1", "5", "14")
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    
    # # get category names
    # if (length(this_sp@data[,2]) == 1) cat_names <- list(this_sp@data[,2]) else cat_names <- this_sp@data[,2]
    
    # get bar colors
    bar_cols <- rep(myblue, times = nrow(this_sp@data))
    if (length(sel) > 0){
      pos <- which(as.character(this_sp$id) %in% sel)
      bar_cols[pos] <- myred
    }
    
    # detect and convert percentage values
    perc <- F
    this_att_show <- this_att
    if (this_att != "OpenDashboard Indikator"){
      if (all(all(this_dat <= 1), all(this_dat >= 0))){
        perc <- T
        this_dat <- this_dat * 100
        this_att_show <- paste(this_att, "in %", sep = " ")
      }
    }
    
    
    # gather data and order bars
    ord <- rev(order(this_dat))
    cat_names <- this_sp@data[,2]
    data <- data.frame(cat_names, this_dat, bar_cols, stringsAsFactors = FALSE)
    data$cat_names <- factor(data$cat_names, levels = unique(data$cat_names)[order(data$this_dat, decreasing = TRUE)])
    data$bar_cols <- factor(data$bar_cols, levels = unique(data$bar_cols)[order(data$this_dat, decreasing = TRUE)])
    
    # define axis range
    av_cols <- str_which(colnames(this_sp@data), paste0("^", this_att, "_?"))
    this_dat_att <- this_sp@data[,av_cols]
    ymin_t <- min(this_dat_att, na.rm = T)
    ymax_t <- max(this_dat_att, na.rm = T)
    ymin <- ymin_t - ((ymax_t - ymin_t) * 0.05)
    if (ymin > 0) ymin <- 0
    ymax <- ymax_t + ((ymax_t - ymin_t) * 0.05)
    
    
    # draw plot
    par(xpd = T)
    b_plot <- plot_ly(data, x = ~cat_names, y = ~this_dat, type = "bar",
                      text = round(this_dat, 2), textposition = "auto",
                      marker = list(color = ~bar_cols)) %>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = this_att_show, range = c(ymin, ymax)))
  })

  
  # observe click events in barplot
  observe({
    b_clicked <- event_data("plotly_click")$x
    # b_selected <- event_data("plotly_selected")$x
    # a_clicked_names$names <- unique(c(b_clicked, b_selected))
    a_clicked_names$names <- b_clicked
  })
    
  # observe reactive value for click events in barplot
  observeEvent(a_clicked_names$names, {
    a_clicked_names <- a_clicked_names$names
    this_sp <- get(input$sp_sel)
    pos <- which((this_sp@data[,2] %in% a_clicked_names))
    a_clicked <- as.character(this_sp@data[pos,1])
    if (length(a_clicked) > 0){
      if (any(a_clicked %in% sel_ob$sel)){
        hit_pos <- which(sel_ob$sel == a_clicked)
        sel_ob$sel <<- sel_ob$sel[-hit_pos]
      } else {
        sel_ob$sel <<- c(sel_ob$sel, a_clicked)
      }
    }
  })
  

  # create scatterplot
  output$scatterplot_1 <- renderPlotly({

    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel
    this_time <- input$time_sel
    # this_dat <- get("Stadtteil")@data
    # this_dat <- get("Sozialraum")@data
    # this_att <- "0-5 Jahre"
    # this_att <- "OpenDashboard Indikator"
    # this_time <- "2017"
    # sel <- c()
    # sel <- c("7", "1", "2", "5")
    
    # get all available time steps
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    av_times <- as.numeric(str_sub(av_atts, -4))
    sel <- as.numeric(sel_ob$sel)
    
    this_dat_att <- this_dat[,av_cols]
    
    
    # prepare data
    if (length(sel) == 0){
      y_dat <- as.data.frame(colMeans(this_dat_att))
      colnames(y_dat) <- "y"
      dat <- cbind(y_dat, x = av_times)
      dat <- cbind(dat, label = "Durchschnitt")
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      labels <- this_dat[pos,2]
      y_dat <- as.data.frame(t(this_dat_att[pos[1],]))
      colnames(y_dat) <- "y"
      dat <- cbind(y_dat, x = av_times)
      dat <- cbind(dat, label = labels[1])
      if (length(sel) > 1){
        lapply(2:length(sel), function(i){
          temp <- as.data.frame(t(this_dat_att[pos[i],]))
          colnames(temp) <- "y"
          temp <- cbind(temp, x = av_times)
          temp <- cbind(temp, label = labels[i])
          dat <<- rbind(dat, temp)
        })
      }
    }
    
    
    # create scatterplot
    par(xpd = T)
    sc_p <- plot_ly(dat, x = ~x,  y = ~y, color = ~label,
                    type = "scatter", mode = "lines", line = list(shape = "spline")) %>% 
      layout(title = "Zeitliche Entwicklung im gewählten Gebiet",
             xaxis = list(title = "Zeit in Jahren"), yaxis = list(title = this_att))
    sc_p

  })

  
  # create gauges
  output$gauge1 <- renderGauge({
    
    # prepare data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel_gauge1
    this_time <- input$time_sel
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    sel <- as.numeric(sel_ob$sel)
    this_dat_att <- this_dat[,av_cols]
    this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
    
    
    if (length(sel) == 0){
      g_val <- round(mean(this_dat_att_t, na.rm = T), 0)
      g_min <- round(min(this_dat_att, na.rm = T), 2)
      g_max <- round(max(this_dat_att, na.rm = T), 2)
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 0)
      g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
      g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
    }
    
    # create gauge
    gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
    
  })
  
  output$gauge2 <- renderGauge({
    
    # prepare data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel_gauge2
    this_time <- input$time_sel
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    sel <- as.numeric(sel_ob$sel)
    this_dat_att <- this_dat[,av_cols]
    this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
    
    if (length(sel) == 0){
      g_val <- round(mean(this_dat_att_t, na.rm = T), 0)
      g_min <- round(min(this_dat_att, na.rm = T), 2)
      g_max <- round(max(this_dat_att, na.rm = T), 2)
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 0)
      g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
      g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
    }
    
    # create gauge
    gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
    
  })
  
  output$gauge3 <- renderGauge({
   
    # prepare data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel_gauge3
    this_time <- input$time_sel
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    sel <- as.numeric(sel_ob$sel)
    this_dat_att <- this_dat[,av_cols]
    this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
    
    if (length(sel) == 0){
      g_val <- round(mean(this_dat_att_t, na.rm = T), 0)
      g_min <- round(min(this_dat_att, na.rm = T), 2)
      g_max <- round(max(this_dat_att, na.rm = T), 2)
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 0)
      g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
      g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
    }
    
    # create gauge
    gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
    
  })
  
  output$gauge4 <- renderGauge({
    
    # prepare data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel_gauge4
    this_time <- input$time_sel
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    sel <- as.numeric(sel_ob$sel)
    this_dat_att <- this_dat[,av_cols]
    this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
    
    if (length(sel) == 0){
      g_val <- round(mean(this_dat_att_t, na.rm = T), 0)
      g_min <- round(min(this_dat_att, na.rm = T), 2)
      g_max <- round(max(this_dat_att, na.rm = T), 2)
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 0)
      g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
      g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
    }
    
    # create gauge
    gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
    
  })
  
  output$gauge5 <- renderGauge({
    
    # prepare data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel_gauge5
    this_time <- input$time_sel
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_?"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_?"))
    sel <- as.numeric(sel_ob$sel)
    this_dat_att <- this_dat[,av_cols]
    this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
    
    if (length(sel) == 0){
      g_val <- round(mean(this_dat_att_t, na.rm = T), 0)
      g_min <- round(min(this_dat_att, na.rm = T), 2)
      g_max <- round(max(this_dat_att, na.rm = T), 2)
    } else {
      pos <- which(as.character(this_dat$id) %in% sel)
      g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 0)
      g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
      g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
    }
    
    # create gauge
    gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
    
  })
  
  
  # build indicator
  observe({
    
    if (all(!is.null(input$sp_sel), !is.null(input$att_sel_gauge1), !is.null(input$att_sel_gauge2),
            !is.null(input$att_sel_gauge3), !is.null(input$att_sel_gauge4), !is.null(input$att_sel_gauge5),
            !is.null(input$g_w1), !is.null(input$g_w2), !is.null(input$g_w3), !is.null(input$g_w4), !is.null(input$g_w5))){
    
      # get reactive values
      this_sp <- get(input$sp_sel)
      # this_sp <- get("Stadtteil")
      this_att1 <- input$att_sel_gauge1
      this_att2 <- input$att_sel_gauge2
      this_att3 <- input$att_sel_gauge3
      this_att4 <- input$att_sel_gauge4
      this_att5 <- input$att_sel_gauge5
      # this_att1 <- "Einwohner"
      # this_att2 <- "Haushalte"
      # this_att3 <- "Frauen"
      # this_att4 <- "U18"
      # this_att5 <- "Single-Haushalte"
      w1 <- input$g_w1
      w2 <- input$g_w2
      w3 <- input$g_w3
      w4 <- input$g_w4
      w5 <- input$g_w5
      # w1 <- 0
      # w2 <- 0.3
      # w3 <- 0.7
      # w4 <- -0.2
      # w5 <- -0.8
  
      # get data for all years, normalize and remove column names
      lapply(1:5, function(i) {
        assign(paste0("av_atts", i), str_subset(colnames(this_sp@data), paste0("^", get(paste0("this_att", i)), "_?")), envir = .GlobalEnv)
        assign(paste0("av_cols", i), str_which(colnames(this_sp@data), paste0("^", get(paste0("this_att", i)), "_?")), envir = .GlobalEnv)
        assign(paste0("this_dat_att", i), this_sp@data[,get(paste0("av_cols", i))], envir = .GlobalEnv)
        dat <- as.matrix(get(paste0("this_dat_att", i)))
        dat_min <- min(dat, na.rm = T)
        dat_max <- max(dat, na.rm = T)
        assign(paste0("this_dat_att_n", i), rescale(x = dat, from = c(dat_min, dat_max), to = c(0, 1)), envir = .GlobalEnv)
      })
     
      
      # calculate indicator (for all years)
      ind_dat <- (this_dat_att_n1 * w1) + (this_dat_att_n2 * w2) + (this_dat_att_n3 * w3) +
        (this_dat_att_n4 * w4) + (this_dat_att_n5 * w5)
      
      # rename indicator data
      av_times <- str_sub(colnames(this_dat_att1), -4)
      colnames(ind_dat) <- paste0("OpenDashboard Indikator_timeSep_", av_times)
      
      # add data to spatial object
      if (all(colnames(ind_dat) %in% colnames(this_sp@data))){
        pos <- which(colnames(this_sp@data) == colnames(ind_dat)[1])
        this_sp@data[,pos:(pos + ncol(ind_dat) - 1)] <- ind_dat
      } else {
        this_sp@data <- cbind(this_sp@data, ind_dat)
      }
      assign(input$sp_sel, this_sp, envir = .GlobalEnv)
      
      # add entry to attribute overview
      if (!("OpenDashboard Indikator" %in% all_atts)) all_atts <<- c(all_atts, "OpenDashboard Indikator")
      if (!("OpenDashboard Indikator" %in% tab_ov[,1])){
        temp <- matrix(NA, nrow = length(av_times), ncol = 4)
        temp[,1] <- "OpenDashboard Indikator"
        temp[,2] <- "OpenDashboard Indikator"
        temp[,3] <- av_times
        temp[,4] <- paste0("OpenDashboardIndikator_", av_times)
        tab_ov <<- rbind(tab_ov, temp)
      }
      
      # trigger events through reactive value
      ind_act$act <- Sys.time()
      
    }
    
  })
  
  # add logo
  output$logo <- renderImage({
    
    list(src = logoPath, alt = "Raumanalysen - Christian Müller")
    
  }, deleteFile = F)
  
  
  
  # quit session when browser is closed
  observe({
    if (input$close > 0){
      
      # clear temp-files
      projFiles <- list.files(dirname(getwd()), full.names = T)
      tempFilesPos <- grepl("^[0-9],[0-9]", basename(projFiles))
      tempFiles <- projFiles[tempFilesPos]
      for (x in tempFiles) file.remove(x)
      
      # stop session
      stopApp()
    }
  })
  
}
