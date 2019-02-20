# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script sets up the global variables for the OpenDashboard

# set working directory for debugging
# setwd("/media/sf_Raumanalysen_ChristianMueller/Projekte/OpenDashboard/OpenDashboard")
# setwd("C:/Raumanalysen_ChristianMueller/Projekte/OpenDashboard/OpenDashboard")


# call libraries
library(tcltk)
library(readxl)
library(rgdal)
library(dplyr)
library(leaflet)
library(sp)
library(classInt)
library(extrafont)
library(flexdashboard)
library(png)
library(RColorBrewer)
library(plotly)
library(shiny)
library(stringr)
library(scales)

# # load fonts
# font_import()
# fonts()

# load configurations
conf_path <- paste0(getwd(), "/OpenDashboard_Configurations.xlsx")
conf_sp <- as.data.frame(read_excel(conf_path, "SpatialData"))
conf_tab <- as.data.frame(read_excel(conf_path, "TableData"))
conf_join <- as.data.frame(read_excel(conf_path, "JoinSpec"))
pr_dir <- getwd()
dat_dir <- paste0(pr_dir, "/Data")

# load spatial data
lapply(1:nrow(conf_sp), function(x){
  
  # prepare data path
  paste0(pr_dir, "/Data/")
  
  # load data
  this_obj <- readOGR(dsn = dat_dir, layer = conf_sp[x, "Filename_noExt"])
  
  # project to WGS84
  this_obj <- spTransform(this_obj, CRS("+init=epsg:4326"))
  
  # save to global variable
  assign(conf_sp[x, "DataName_com"], this_obj, envir = .GlobalEnv)
  
})



# prepare table data overview
tab_ov <- matrix(NA, nrow = 0, ncol = 4, dimnames = list(c(), c("att_com", "att_nice", "time", "tab_obj")))

# load table data and store time information
lapply(1:nrow(conf_tab), function(x){
  
  # prepare data path
  tab_path <- paste0(dat_dir, "/", conf_tab[x, "Filename_ext"])
  
  # load workbook and get sheet names
  sh_names <- excel_sheets(tab_path)
  
  # get sheet data of type 'oDa_time'
  lapply(1:length(sh_names), function(y){
    
    # filter for data sheets which were prepared for OpenDashboard
    this_split <- strsplit(sh_names[y], "_", fixed = T)[[1]]
    
    if (length(this_split) > 0){
      
      if (this_split[1] == "oDa"){
        
        this_obj <- as.data.frame(read_excel(tab_path, sh_names[y]))
        
        # prepare nice attribute names
        nice_names <- gsub(x = colnames(this_obj), pattern = '\r', replacement = "", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '\n', replacement = "-", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = ' -', replacement = "-", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '  ', replacement = " ", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '- ', replacement = "-", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '__', replacement = "_", fixed = T)
        
        # remove artefacts
        pos <- which(substr(nice_names, 1, 1) == "-")
        if (length(pos > 0)) nice_names[pos] <- substr(nice_names[pos], 2, nchar(nice_names[pos]))
        pos <- which(substr(nice_names, nchar(nice_names), nchar(nice_names)) == "-")
        if (length(pos > 0)) nice_names[pos] <- substr(nice_names[pos], 1, nchar(nice_names[pos]) - 1)
        
        # store time information and nice names in table overview
        tab_ov <<- rbind(tab_ov, cbind(colnames(this_obj),                                       # attribute names as is
                                       nice_names,                                               # nice attribute names
                                       this_split[2],                                            # time
                                       paste0(conf_tab[x, "DataName_com"], "_",this_split[2])))  # table object name
        
        # overwrite attribute names
        colnames(this_obj) <- nice_names
        
        # store table data
        assign(paste0(conf_tab[x, "DataName_com"], "_", this_split[2]), this_obj, envir = .GlobalEnv)
        
        
      }
    }
  })
})

# save all available attributes
all_atts <- tab_ov[-which(tab_ov == conf_join[1, 1]), 2]

# prepare spatial data for joining
lapply(1:nrow(conf_sp), function(x){
  
  # extract attribute table
  assign(paste0(conf_sp[x, 1], "_dat"), get(conf_sp[x,1])@data, envir = .GlobalEnv)
  
  # wrap attribute data in local data frame
  assign(paste0(conf_sp[x, 1], "_ldf"), tbl_df(get(paste0(conf_sp[x, 1], "_dat"))), envir = .GlobalEnv)
  print("Loaded sucessfully")
  
})


# prepare table data for joining
tabs <- unique(tab_ov[,"tab_obj"])
lapply(1:length(tabs), function(x){
  
  # wrap  data in local data frame
  assign(paste0(tabs[x], "_ldf"), tbl_df(get(tabs[x])), envir = .GlobalEnv)
  print("Loaded sucessfully")
  
})


# join data
lapply(1:nrow(conf_sp), function(sp){
  
  lapply(1:length(tabs), function(t){
    
    # get datasets
    this_sp <- get(paste0(conf_sp[sp, 1], "_ldf"))
    this_t <- get(paste0(tabs[t], "_ldf"))
    
    # join datasets
    suppressMessages(assign(paste0(conf_sp[sp, 1], "_ldf"),
                            left_join(x = this_sp, y = this_t, by = conf_join[1,1]),
                            envir = .GlobalEnv))
    print("Joined successfully")
    
  })
  
})



# overwrite attribute table with joined data
lapply(1:nrow(conf_sp), function(x){
  
  # convert local data frame to regular data frame
  this_dat <- data.frame(get(paste0(conf_sp[x, 1], "_ldf")))
  
  # get nice attribute names with time information
  nice_names <- all_atts
  nice_names <- paste0(nice_names, "_timeSep_", tab_ov[-which(tab_ov == conf_join[1, 1]), 3])
  
  # assign nice attribute names
  pos <- which((colnames(this_dat) %in% colnames(get(conf_sp[x, 1])@data)) == F)
  colnames(this_dat)[pos] <- nice_names
  
  # overwrite attribute table
  this_sp <- get(conf_sp[x, 1])
  this_sp@data <- this_dat
  assign(conf_sp[x, 1], this_sp, envir = .GlobalEnv)
  assign(paste0(conf_sp[x, 1], "_dat"), this_dat, envir = .GlobalEnv)
  
})

# define colors
mycol_1 <- rgb(0, 176, 240, maxColorValue = 255)
mycol_2 <- rgb(237, 125, 49, maxColorValue = 255)

# define absolute values for test purpose
# this_sp_char <- "Stadtgebiet"
# this_att <- "gesamt"
# this_time <- "2017"
# this_sp <- get(this_sp_char)
# this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
# this_sp_char <- "Stadtteil"
# this_att <- "gesamt"
# this_time <- "2017"
# this_sp <- get(this_sp_char)
# this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
# this_sp_char <- "Stadtteil"
# this_att <- "18-20 Jahre"
# this_time <- "2017"
# this_sp <- get(this_sp_char)
# this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]
this_sp_char <- conf_sp[1,2]
this_dat<- get(this_sp_char)@data
av_atts <- unique(unlist(strsplit(colnames(this_dat), "_timeSep_", fixed = T)))
av_atts <- av_atts[av_atts %in% all_atts]
this_att <- sort(av_atts)[1]
pos <- which(tab_ov[, "att_nice"] == this_att)
times <- as.numeric(tab_ov[pos, "time"])
t.min <- min(times, na.rm = T)
t.max <- max(times, na.rm = T)
this_time <- t.max
this_sp <- get(this_sp_char)
this_dat <- this_sp@data[,paste0(this_att, "_timeSep_", this_time)]

# load logo
# logo <- readPNG(paste0(getwd(), "/Logo.png"))
logoPath <- paste0(getwd(), "/logo.png")

# set colors
myred <- rgb(244,100,48, maxColorValue = 255)
myblue <- rgb(86,170,179, maxColorValue = 255)

# set last selected attribute
last_att <- ""

# create reactive value for clicked selection
sel_ob <- reactiveValues()
sel_ob$sel <- NULL
a_clicked_names <- reactiveValues()
a_clicked_names$names <- NULL
ind_act <- reactiveValues()
ind_act$act <- F
