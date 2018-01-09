# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script executes functions and defines the dynamic behavior of the OpenDashboard

function(input, output, session){
  
  # create map
  output$map <- renderLeaflet({
    
    # define colors
    pos <- order(Einwohnerdaten_2017[,1])
    att_dat <- Einwohnerdaten_2017$gesamt[pos]
    att_dat <- att_dat[-c(4, 5, 6, 7, 8)]
    clInt <- classIntervals(att_dat, n = 5, style = "pretty")
    cols <- as.character(findColours(clInt, pal = c("lightblue", "blue")))
    
    # define legend labels
    leg_lab <- character(length(clInt$brks)-1)
    for (b in 2:length(clInt$brks)){
      leg_lab[b - 1] <- paste0(clInt$brks[b - 1], " - ", clInt$brks[b])
    }
    
    # create map object
    map <- leaflet() %>%
      
      # define boundaries
      fitBounds(lng1 = Stadtteil@bbox[1,1], lng2 = Stadtteil@bbox[1,2],
                lat1 = Stadtteil@bbox[2,1], lat2 = Stadtteil@bbox[2,2]) %>% 
      
      # add legend
      addLegend(position = "bottomleft", colors = rev(sort(unique(cols))), label = leg_lab)
    
    
    # add polygons
    map <- addPolygons(map = map, data = Stadtteil, fillColor = cols, color = "white",
                       opacity = 1, fillOpacity = 1, weight = 3)
    
    map
    
  })
  
  
  # create barplot
  output$barplot_1 <- renderPlot({
    
    # define plot order
    pos <- rev(order(att_dat))
    
    # set up graphic parameters
    par(mar = c(10, 6, 4.1, 1))
    
    # define plot names
    names.arg <- c("Bergheim", "Rheidt-Hüchelhoven", "Zieverich", "Kenten", "Glesch", "Paffendorf", "Thorr", "Niederaußem",
                   "Oberaußem", "Auenheim", "Büsdorf", "Fliesteden", "Glessen", "Ahe", "Quadrath-Ichendorf")
    
    # create barplot
    barplot(att_dat[pos], col = mycol_2, space = 0.05, border = "white",
            main = "Stadtteil-Ranking (Mock-up-Daten!!!)",
            names.arg = names.arg, las = 2)
    
    # add lables
    mtext(side = 2, text = "Einwohnerzahl (Mock-up-Daten!!!)", line = 4)
    
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
  
}
