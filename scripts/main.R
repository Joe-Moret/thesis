# User defined inputs ------------------------
# By double-clicking on the project file, the working directory (wd) should be automatically set to the directory where all files are located. 
# If this is not the case, you can set the working directory manually by changing the path in the setwd() function.
# Confirm your working directory.
getwd()

# Check if files exist in your wd ------------------------
sapply(c("scripts/functions.R", "scripts/data.R", "scripts/01_Earnings.R", 
         "scripts/02_FB.R", "scripts/03_FF.R", "scripts/04_ICC.R", 
         "scripts/05_MeV.R", "scripts/06_Risk.R"), file.exists)

# Install and load the Packages ---------------------------------
library(haven)
library(here)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(lmtest)
library(sandwich)
library(readxl) 
library(purrr)
library(readr)


# library(data.table) # install.packages("data.table", type = "source", repos = "http://Rdatatable.github.io/data.table") for Macbook
# library(purrr)
# library(broom)


# Run code chunks  ---------------------------------
source("scripts/functions.R")
source("scripts/data.R")
source("scripts/01_Earnings.R")
source("scripts/02_FB.R")
source("scripts/03_FF.R")
source("scripts/04_ICC.R")
source("scripts/05_MeV.R")
source("scripts/06_Risk.R")





