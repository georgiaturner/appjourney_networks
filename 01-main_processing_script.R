################################################################################
################################################################################
##################  MAIN SCRIPT FOR ITACO DATA CLEANING ########################
##################  Zelda Brufal, July 2024             ########################
##################  Zelda@live.co.uk                    ########################
################################################################################
################################################################################

################## SESSION INFORMATION #########################################

# R version 4.3.3 (2024-02-29)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sonoma 14.2.1

################## Setting up environment  #####################################

#install.packages('dplyr')
#install.packages('tidyverse')
#install.packages('lubridate')
#install.packages('stringr')
#install.packages("here")


library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(here)

rm(list = ls())
seed <- 1

set.seed(seed)

################################################################################
############     setting up the directory correctly ############################
################################################################################


#### moving out of folder that is uploaded to github to where the data is 

here_dir   <- here()
parent_dir <- dirname(here_dir)
setwd(parent_dir)

### creating directory to data 

data_directory <- stringi::stri_join(parent_dir, "usable_participant_data", "", sep = "/")

################################################################################
##################  LOADING IN AND PROCESSING ITACO DATA           #############
################################################################################

source(paste0(here_dir,"/", "02-process_app_data.R"))

################################################################################
##################  LOADING IN AND PROCESSING MENTAL HEALTH  DATA  #############
################################################################################

# Read in the file with the mental health data

source(paste0(here_dir,"/", "03-process_qnr_data.R"))

################################################################################
##################  CREATING THE DIFFERENT LEVEL DATA FRAMES        ############
################################################################################

source(paste0(here_dir,"/", "04-make_summary_dfs.R"))



