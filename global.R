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

# load configurations
conf_path <- paste0(getwd(), "/OpenDashboard_Configurations.xlsx")
conf_sp <- as.data.frame(read_excel(conf_path, "SpatialData"))
conf_tab <- as.data.frame(read_excel(conf_path, "TableData"))


# load spatial data
lapply(1:nrow(conf_sp), function(x){
  
  # load data
  this_obj <- readOGR(dsn = conf_sp[x, "Datapath_noFilename"], layer = conf_sp[x, "Filename_noExt"])
  
  # project to WGS84
  this_obj <- spTransform(this_obj, CRS("+init=epsg:4326"))
  
  # save to global variable
  assign(conf_sp[x, "DataName_com"], this_obj, envir = .GlobalEnv)
  
})



# prepare table data overview
tab_ov <- matrix(NA, nrow = 0, ncol = 4, dimnames = list(c(), c("att_com", "att_nice", "time", "tab_obj")))

# load table data and store time information
lapply(1:nrow(conf_tab), function(x){
  
  # load workbook and get sheet names
  sh_names <- excel_sheets(conf_tab[x, "Datapath"])
  
  # get sheet data of type 'oDa_time'
  lapply(1:length(sh_names), function(y){
    this_split <- strsplit(sh_names[y], "_", fixed = T)[[1]]
    if (length(this_split) > 0){
      if (this_split[1] == "oDa"){
        this_obj <- as.data.frame(read_excel(conf_tab[x, "Datapath"], sh_names[y]))
        
        # prepare nice attribute names
        nice_names <- gsub(x = colnames(this_obj), pattern = '\r', replacement = "", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '\n', replacement = "-", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = ' -', replacement = "-", fixed = T)
        nice_names <- gsub(x = nice_names, pattern = '  ', replacement = " ", fixed = T)
        
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



# join spatial and table data



# define colors
mycol_1 <- rgb(0, 176, 240, maxColorValue = 255)
mycol_2 <- rgb(237, 125, 49, maxColorValue = 255)


