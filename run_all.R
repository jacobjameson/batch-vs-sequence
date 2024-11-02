#=========================================================================
# Purpose: This script loads all necessary packages for the project and 
#          runs all other scripts in the project. 
#
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) # Clear the workspace

library(tidyverse)
library(lfe)
library(lubridate)
library(caret)
library(sandwich)
library(xtable)
library(lmtest)
library(marginaleffects)
library(scales)  # For percent_format
library(stringr) # For str_wrap
library(ggeffects)
library(pals)
library(ggsci)
library(ggthemes)
library(texreg)
library(fixest)
library(pROC)
library(stargazer)

# Create log file
sink("outputs/logs/log.txt", append = FALSE, split = TRUE)

# Run all scripts
source("src/main.R", echo = TRUE)
source("src/figures.R", echo = TRUE)

# Close log file
sink()
