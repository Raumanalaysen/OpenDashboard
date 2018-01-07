# OpenDashboard
# Author: Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# This script sets up the global variables for the OpenDashboard

# set working directory for debugging
# setwd("/media/sf_Raumanalysen_ChristianMueller/Projekte/OpenDashboard/OpenDashboard")


# call libraries
library(tcltk)
library(XLConnectJars)
library(XLConnect)


# load configurations
conf_path <- paste0(getwd(), "/OpenDashboard_Configurations.xlsx")
wb <- loadWorkbook(conf_path)
# dat <- readWorksheet(wb, sheet = sheetName, header = T)
    