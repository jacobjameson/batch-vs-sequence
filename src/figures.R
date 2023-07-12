#=========================================================================
# Purpose: Generate Figures and Tables for DataWatch Piece
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

source('src/main.R')

library(grid)
library(shadowtext)
library(GGally)
library(network)
library(sna)
library(tidyverse)
library(ggnetwork)
library(intergraph)
library(igraph)
library(circlize)
library(caret)
library(sandwich)

#=========================================================================
# Figure 1
#   - Show that across physician, there is random assignment and patient
#     characteristics are balanced
#=========================================================================

dummy <- dummyVars(" ~ CHIEF_COMPLAINT", data=final)
one.hot <- data.frame(predict(dummy, newdata=final))
final <- cbind(final, one.hot)


complaints <-  data.frame(Df = numeric(), F = numeric(), 
                          Pr..F. = numeric(), complaint = character(),
                          stringsAsFactors = FALSE)

for (complaint in names(one.hot)){

  model_pos_1 <- lm(as.formula(paste(complaint, '~ 1')), final)
  model_pos_2 <- lm(as.formula(paste(complaint, '~ ED_PROVIDER')), final)
                    
  wald_pos <- lmtest::waldtest(model_pos_1, model_pos_2, 
                               vcov = vcovHC(model_pos_2, type = "HC1"))
  print(wald_pos)
  temp <- data.frame(wald_pos)[2, c(2,3,4)]
  temp$complaint <- complaint
  
  complaints <- rbind(complaints, temp)
  
}

