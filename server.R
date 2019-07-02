# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script executes functions and defines the dynamic behavior of the OpenDashboard

function(input, output, session){
  
  # User input ----
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
  
  
  
  # render time slider
  output$time_sel_out <- renderUI({
    
    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get spatial data input
    this_att <- input$att_sel
    # this_att <- "Arbeitslose SGB-II/ SGB-III"
    # this_att <- "OpenDashboard Indikator"
    
    # get available times
    pos <- which(tab_ov[, "att_nice"] == this_att)
    times <- as.numeric(tab_ov[pos, "time"])
    # t.min <- min(times, na.rm = T)
    # t.max <- max(times, na.rm = T)
    # step_seq <- seq(t.min, t.max, length.out = length(pos))
    # step <- step_seq[2] - step_seq[1]
    
    # sliderInput(inputId = "time_sel", label = NULL,
    #             value = t.max, min = t.min, max = t.max, sep = "",
    #             step = step, ticks = F, animate = T, width = "90%")
    
    sliderTextInput(inputId = "time_sel", label = NULL,
                choices = times, animate = T, width = "90%")
    
  })
  
  
  
  ### Map ----
  # create map
  output$map <- renderLeaflet({

    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get reactive values
    this_sp_char <- input$sp_sel
    this_att <- input$att_sel
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
      
      # background maps
      addWMSTiles("none", layers = "wms_nw_dtk", group = "kein Hintergrund") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap Mapnik") %>%
      addProviderTiles("OpenStreetMap.HOT", group = "OpenStreetMap HOT") %>%
      addProviderTiles("Hydda.Full", group = "Hydda Full") %>%
      addProviderTiles("Hydda.Base", group = "Hydda Base") %>%
      addProviderTiles("Stamen.Watercolor", group = "Stamen Watercolor") %>%
      addProviderTiles("Stamen.Terrain", group = "Stamen Terrain") %>%
      addProviderTiles("Stamen.TerrainBackground", group = "Stamen TerrainBackground") %>%
      
      addWMSTiles("http://www.wms.nrw.de/geobasis/wms_nw_dtk?", layers = "wms_nw_dtk", group = "Topographische Karte (DTK)") %>%
      addWMSTiles("http://www.wms.nrw.de/geobasis/wms_nw_dgm-schummerung?", layers = "wms_nw_dgm-schummerung", group = "Schummerung") %>%
      
      addLayersControl(
        baseGroups = c("kein Hintergrund", "OpenStreetMap Mapnik", "OpenStreetMap HOT",
                       "Hydda Full", "Hydda Base",
                       "Stamen Watercolor", "Stamen Terrain", "Stamen TerrainBackground",
                       "Topographische Karte (DTK)", "Schummerung"), 
        options = layersControlOptions(collapsed = T),
        position = "topright") %>% 
      

      # define boundaries
      fitBounds(lng1 = this_sp@bbox[1,1], lng2 = this_sp@bbox[1,2],
                lat1 = this_sp@bbox[2,1], lat2 = this_sp@bbox[2,2]) %>%

      # add legend
      addLegend(position = "bottomleft", colors = leg_cols, label = leg_lab) %>% 


      # add polygons
      addPolygons(data = this_sp, layerId = ~id, fillColor = cols, color = "white",
                  opacity = 1, fillOpacity = input$trans_sel, weight = 3, label = this_sp@data[,2],
                  group = "not_highlighted")


  })
  
  
  # observe selected objects and highlight in map
  observe({
    
    # get spatial objects
    this_sp <- get(input$sp_sel)
    
    # react to parameter changes
    this_att <- input$att_sel
    this_time <- input$time_sel
    
    # react to indicator weightings
    this_att1 <- input$att_sel_gauge1
    this_att2 <- input$att_sel_gauge2
    this_att3 <- input$att_sel_gauge3
    this_att4 <- input$att_sel_gauge4
    this_att5 <- input$att_sel_gauge5
    w1 <- input$g_w1
    w2 <- input$g_w2
    w3 <- input$g_w3
    w4 <- input$g_w4
    w5 <- input$g_w5
    
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
                                  opacity = 1, fillOpacity = input$trans_sel, weight = 3, label = nuse_sp@data[,2],
                                  group = "not_highlighted")
      }
      
      # add highlighted polygons
      map_proxy %>% addPolygons(data = use_sp, layerId = ~id, fillColor = use_cols, color = myred,
                                opacity = 1, fillOpacity = input$trans_sel, weight = 3, label = use_sp@data[,2],
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
                    opacity = 1, fillOpacity = input$trans_sel, weight = 3, label = this_sp@data[,2],
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


  
  # Bar plot ----
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
    # this_att <- "Anteil SGB-II-unter 15 Jahre an Altersklasse"
    # this_att <- "OpenDashboard Indikator"
    # this_time <- "2017"
    # sel <- c("1", "5", "14")
    this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
    
    
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
        this_att_show <- paste0(this_att, "in %")
      }
    }
    
    
    # gather data and order bars
    ord <- rev(order(this_dat))
    cat_names <- this_sp@data[,2]
    data <- data.frame(cat_names, this_dat, bar_cols, stringsAsFactors = FALSE)
    colnames(data)[2] <- "this_dat"
    data$cat_names <- factor(data$cat_names, levels = unique(data$cat_names)[order(data$this_dat, decreasing = TRUE)])
    data$bar_cols <- factor(data$bar_cols, levels = unique(data$bar_cols)[order(data$this_dat, decreasing = TRUE)])
    
    # define axis range
    av_cols <- str_which(colnames(this_sp@data), paste0("^", this_att, "_"))
    this_dat_att <- data[,"this_dat"]
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
    b_plot
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
    # a_clicked_names <- "Zieverich"
    this_sp <- get(input$sp_sel)
    # this_sp <- get("Stadtteil")
    pos <- which((this_sp@data[,2] %in% a_clicked_names))
    a_clicked <- as.character(this_sp@data[pos,"id"])
    if (length(a_clicked) > 0){
      if (any(a_clicked %in% sel_ob$sel)){
        hit_pos <- which(sel_ob$sel == a_clicked)
        sel_ob$sel <<- sel_ob$sel[-hit_pos]
      } else {
        sel_ob$sel <<- c(sel_ob$sel, a_clicked)
      }
    }
  })
  

  
  # Scatterplot ----
  # create scatterplot
  output$scatterplot_1 <- renderPlotly({

    # refresh if indicator has been recalculated
    act <- ind_act$act
    
    # get data
    this_dat <- get(input$sp_sel)@data
    this_att <- input$att_sel
    this_time <- input$time_sel
    # this_dat <- get("Stadtgebiet")@data
    # this_dat <- get("Stadtteil")@data
    # this_dat <- get("Sozialraum")@data
    # this_att <- "SGB-II-Personen-insgesamt"
    # this_att <- "Arbeitslose SGB-II/ SGB-III"
    # this_att <- "0-5 Jahre"
    # this_att <- "OpenDashboard Indikator"
    # this_att <- "Anteil SGB-II-an EW-gesamt"
    # this_att <- "Anteil SGB-II-3 bis unter 6 Jahre an Altersklasse"
    # this_time <- "2017"
    # sel <- c()
    # sel <- c(7, 1, 2, 5)
    # sel <- c(5, 6)
    # sel <- 5
    
    # get all available time steps
    av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
    av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
    av_times <- as.numeric(str_sub(av_atts, -4))
    sel <- as.numeric(sel_ob$sel)
    
    this_dat_att <- as.data.frame(this_dat[,av_cols])
    
    
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
    
    
    # detect and convert percentage values
    perc <- F
    this_att_show <- this_att
    if (this_att != "OpenDashboard Indikator"){
      if (all(all(dat$y <= 1), all(dat$y >= 0))){
        perc <- T
        dat$y <- dat$y * 100
        this_att_show <- paste0(this_att, "in %")
      }
    }
    
    # create scatterplot
    par(xpd = T)
    sc_p <- plot_ly(as.data.frame(dat), x = ~x,  y = ~y, color = ~label,
                    type = "scatter", mode = "lines") %>% 
      layout(title = "Zeitliche Entwicklung im gewählten Gebiet",
             xaxis = list(title = "Zeit in Jahren",
                          ticks = "outside", 
                          tick0 = av_times[1], dtick = 1,
                          range = c(av_times[1] - 0.01, av_times[length(av_times)] + 0.02)),
             yaxis = list(title = this_att_show))
    sc_p

  })

  
  
  # Gauges seleciton----
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
  
  
  # Gauges ----
  # create gauges
  # output$gauge1 <- renderGauge({
  #   
  #   # prepare data
  #   this_dat <- get(input$sp_sel)@data
  #   # this_dat <- Stadtteil@data
  #   this_att <- input$att_sel_gauge1
  #   # this_att <- "Anteil SGB XII an-ueber 65"
  #   # this_att <- "Svpfl.-Beschäftigte"
  #   this_time <- input$time_sel
  #   av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
  #   av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
  #   sel <- as.numeric(sel_ob$sel)
  #   this_dat_att <- this_dat[,av_cols]
  #   this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
  #   
  #   
  #   if (length(sel) == 0){
  #     g_val <- round(mean(this_dat_att_t, na.rm = T), 2)
  #     g_min <- round(min(this_dat_att, na.rm = T), 2)
  #     g_max <- round(max(this_dat_att, na.rm = T), 2)
  #   } else {
  #     pos <- which(as.character(this_dat$id) %in% sel)
  #     g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 2)
  #     g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
  #     g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
  #   }
  #   
  #   # create gauge
  #   gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
  #   
  # })
  # 
  # output$gauge2 <- renderGauge({
  #   
  #   # prepare data
  #   this_dat <- get(input$sp_sel)@data
  #   # this_dat <- Stadtteil@data
  #   this_att <- input$att_sel_gauge2
  #   # this_att <- "Anteil SGB XII an-ueber 65"
  #   this_time <- input$time_sel
  #   av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
  #   av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
  #   sel <- as.numeric(sel_ob$sel)
  #   this_dat_att <- this_dat[,av_cols]
  #   this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
  #   
  #   
  #   if (length(sel) == 0){
  #     g_val <- round(mean(this_dat_att_t, na.rm = T), 2)
  #     g_min <- round(min(this_dat_att, na.rm = T), 2)
  #     g_max <- round(max(this_dat_att, na.rm = T), 2)
  #   } else {
  #     pos <- which(as.character(this_dat$id) %in% sel)
  #     g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 2)
  #     g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
  #     g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
  #   }
  #   
  #   # create gauge
  #   gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
  #   
  # })
  # 
  # output$gauge3 <- renderGauge({
  #  
  #   # prepare data
  #   this_dat <- get(input$sp_sel)@data
  #   # this_dat <- Stadtteil@data
  #   this_att <- input$att_sel_gauge3
  #   # this_att <- "Anteil SGB XII an-ueber 65"
  #   this_time <- input$time_sel
  #   av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
  #   av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
  #   sel <- as.numeric(sel_ob$sel)
  #   this_dat_att <- this_dat[,av_cols]
  #   this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
  #   
  #   
  #   if (length(sel) == 0){
  #     g_val <- round(mean(this_dat_att_t, na.rm = T), 2)
  #     g_min <- round(min(this_dat_att, na.rm = T), 2)
  #     g_max <- round(max(this_dat_att, na.rm = T), 2)
  #   } else {
  #     pos <- which(as.character(this_dat$id) %in% sel)
  #     g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 2)
  #     g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
  #     g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
  #   }
  #   
  #   # create gauge
  #   gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
  #   
  # })
  # 
  # output$gauge4 <- renderGauge({
  #   
  #   # prepare data
  #   this_dat <- get(input$sp_sel)@data
  #   # this_dat <- Stadtteil@data
  #   this_att <- input$att_sel_gauge4
  #   # this_att <- "Anteil SGB XII an-ueber 65"
  #   this_time <- input$time_sel
  #   av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
  #   av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
  #   sel <- as.numeric(sel_ob$sel)
  #   this_dat_att <- this_dat[,av_cols]
  #   this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
  #   
  #   
  #   if (length(sel) == 0){
  #     g_val <- round(mean(this_dat_att_t, na.rm = T), 2)
  #     g_min <- round(min(this_dat_att, na.rm = T), 2)
  #     g_max <- round(max(this_dat_att, na.rm = T), 2)
  #   } else {
  #     pos <- which(as.character(this_dat$id) %in% sel)
  #     g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 2)
  #     g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
  #     g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
  #   }
  #   
  #   # create gauge
  #   gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
  #   
  # })
  # 
  # output$gauge5 <- renderGauge({
  #   
  #   # prepare data
  #   this_dat <- get(input$sp_sel)@data
  #   # this_dat <- Stadtteil@data
  #   this_att <- input$att_sel_gauge5
  #   # this_att <- "Anteil SGB XII an-ueber 65"
  #   this_time <- input$time_sel
  #   av_atts <- str_subset(colnames(this_dat), paste0("^", this_att, "_"))
  #   av_cols <- str_which(colnames(this_dat), paste0("^", this_att, "_"))
  #   sel <- as.numeric(sel_ob$sel)
  #   this_dat_att <- this_dat[,av_cols]
  #   this_dat_att_t <- this_dat[,paste0(this_att, "_timeSep_", this_time)]
  #   
  #   
  #   if (length(sel) == 0){
  #     g_val <- round(mean(this_dat_att_t, na.rm = T), 2)
  #     g_min <- round(min(this_dat_att, na.rm = T), 2)
  #     g_max <- round(max(this_dat_att, na.rm = T), 2)
  #   } else {
  #     pos <- which(as.character(this_dat$id) %in% sel)
  #     g_val <- round(mean(this_dat_att_t[pos], na.rm = T), 2)
  #     g_min <- round(min(this_dat_att[pos,], na.rm = T), 2)
  #     g_max <- round(max(this_dat_att[pos,], na.rm = T), 2)
  #   }
  #   
  #   # create gauge
  #   gauge(g_val, min = g_min, max = g_max, label = "", gaugeSectors(colors = myblue))
  #   
  # })
  
  
  # Indicator ----
  # build indicator
  observe({
    
    if (all(!is.null(input$sp_sel), !is.null(input$att_sel_gauge1), !is.null(input$att_sel_gauge2),
            !is.null(input$att_sel_gauge3), !is.null(input$att_sel_gauge4), !is.null(input$att_sel_gauge5),
            !is.null(input$g_w1), !is.null(input$g_w2), !is.null(input$g_w3), !is.null(input$g_w4), !is.null(input$g_w5))){
    
      # get reactive values
      this_sp <- get(input$sp_sel)
      # this_sp <- get("Stadtteil")
      # this_sp <- get("Stadtgebiet")
      this_att1 <- input$att_sel_gauge1
      this_att2 <- input$att_sel_gauge2
      this_att3 <- input$att_sel_gauge3
      this_att4 <- input$att_sel_gauge4
      this_att5 <- input$att_sel_gauge5
      # this_att1 <- "0-5Jahre"
      # this_att1 <- "Einwohner"
      # this_att2 <- "Haushalte"
      # this_att3 <- "Frauen"
      # this_att4 <- "U18"
      # this_att5 <- "Single-Haushalte"
      # this_att1 <- "Anteil SGB XII an-ueber 65"
      # this_att2 <- "Anteil ALO SGB-II/-III-ueber 55 an EW 55-64 Jahre"
      # this_att3 <- "Anteil ALO SGB-II/-III-ueber 55 an EW 55-64 Jahre"
      # this_att4 <- "Anteil ALO SGB-II/-III-ueber 55 an EW 55-64 Jahre"
      # this_att5 <- "SGB-II-Personen-insgesamt"
      # this_att1 <- "Svpfl.-Beschäftigte"
      # this_att2 <- "Svpfl.-Beschäftigte"
      # this_att3 <- "Svpfl.-Beschäftigte"
      # this_att4 <- "Svpfl.-Beschäftigte"
      # this_att5 <- "Svpfl.-Beschäftigte"
      # this_att1 <- "Bevölkerungsprognose 2017"
      # this_att2 <- "Bevölkerungsprognose 2017"
      # this_att3 <- "Bevölkerungsprognose 2017"
      # this_att4 <- "Bevölkerungsprognose 2017"
      # this_att5 <- "Bevölkerungsprognose 2017"
      # this_att1 <- "Bevölkerungsprognose 2017"
      # this_att2 <- "0-5Jahre"
      # this_att3 <- "0-5Jahre"
      # this_att4 <- "0-5Jahre"
      # this_att5 <- "0-5Jahre"
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
      # w1 <- 0
      # w2 <- 0
      # w3 <- 0
      # w4 <- 0
      # w5 <- 0
  
      # get data for all years, normalize and remove column names
      lapply(1:5, function(i) {
        temp_att <- paste0("^", get(paste0("this_att", i)), "_")
        assign(paste0("av_atts", i), str_subset(colnames(this_sp@data), temp_att), envir = .GlobalEnv)
        assign(paste0("av_cols", i), str_which(colnames(this_sp@data), temp_att), envir = .GlobalEnv)
        assign(paste0("this_dat_att", i), this_sp@data[,get(paste0("av_cols", i))], envir = .GlobalEnv)
        dat <- as.matrix(get(paste0("this_dat_att", i)))
        class(dat) <- "numeric"
        colnames(dat) <- get(paste0("av_atts", i))
        dat_min <- min(dat, na.rm = T)
        dat_max <- max(dat, na.rm = T)
        re_dat <- rescale(x = dat, from = c(dat_min, dat_max), to = c(0, 1))
        assign(paste0("this_dat_att_n", i), re_dat, envir = .GlobalEnv)
        assign(paste0("av_times_", i), str_sub(colnames(re_dat), -4), envir = .GlobalEnv)
      })
      
      # get years for which all parameters are available
      av_times <- Reduce(intersect, list(av_times_1, av_times_2, av_times_3, av_times_4, av_times_5))
      
      # filter for these years
      lapply(1:5, function(i){
        if (length(av_times) > 0){
          pos <<- str_which(str_sub(colnames(get(paste0("this_dat_att_n", i))), -4), paste0(av_times[1]))
          if (length(av_times) > 1){
            lapply(2:length(av_times), function(j){
              pos_temp <- str_which(str_sub(colnames(get(paste0("this_dat_att_n", i))), -4), paste0(av_times[j]))
              assign("pos", c(pos, pos_temp), envir = .GlobalEnv)
            })
          }
        } else {
          pos <- str_which(str_sub(colnames(get(paste0("this_dat_att_n", i))), -4), get(paste0("av_times_", i))[1])
        }
        pos_name <- colnames(get(paste0("this_dat_att_n", i)))[pos]
        temp_dat <- as.matrix(get(paste0("this_dat_att_n", i))[,pos])
        if (nrow(temp_dat) != nrow(get(paste0("this_dat_att_n", i)))) temp_dat <- t(temp_dat)
        colnames(temp_dat) <- pos_name
        assign(paste0("this_dat_att_n", i), temp_dat, envir = .GlobalEnv)
      })
     
      
      # calculate indicator (for all years)
      ind_dat <- (this_dat_att_n1 * w1) + (this_dat_att_n2 * w2) + (this_dat_att_n3 * w3) +
        (this_dat_att_n4 * w4) + (this_dat_att_n5 * w5)
      
      # rename indicator data
      if (length(av_times) > 0) dat_names <- av_times else dat_names <- av_times_1[1]
      colnames(ind_dat) <- paste0("OpenDashboard Indikator_timeSep_", dat_names)
      
      # add data to spatial object
      pos <- str_which(colnames(this_sp@data), "OpenDashboard Indikator")  
      if (length(pos) > 0) temp1 <- this_sp@data[,-pos] else temp1 <- this_sp@data
      temp2 <- ind_dat
      this_sp@data <- cbind(temp1, temp2)
      this_sp <<- this_sp
      assign(input$sp_sel, this_sp, envir = .GlobalEnv)
      
      
      # add entry to attribute overview
      if (!("OpenDashboard Indikator" %in% all_atts)) all_atts <<- c(all_atts, "OpenDashboard Indikator")
      
      if (length(av_times) > 0) av_times_set <- av_times else av_times_set <- av_times_1[1]
      pos <- which(tab_ov[,1] =="OpenDashboard Indikator")
      if (length(pos) > 0) temp1 <- tab_ov[-pos,] else temp1 <- tab_ov
      temp2 <- matrix(NA, nrow = length(av_times_set), ncol = 4)
      temp2[,1] <- "OpenDashboard Indikator"
      temp2[,2] <- "OpenDashboard Indikator"
      temp2[,3] <- av_times_set
      temp2[,4] <- paste0("OpenDashboardIndikator_", av_times_set)
      tab_ov <<- rbind(temp1, temp2)
      
      
      
      # trigger events through reactive value
      ind_act$act <- Sys.time()
      
    }
    
  })
  
  
  # Miscellaneous ----
  # add logo
  output$logo <- renderImage({
    
    list(src = logoPath, alt = "Raumanalysen - Christian Müller")
    
  }, deleteFile = F)
  
  # add loading page
  output$loading <- renderImage({
    
    list(src = loadingPath, alt = "Gleich geht's los...")
    
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
  
  # # end loading screen
  # hide_loading_page()
  
}
