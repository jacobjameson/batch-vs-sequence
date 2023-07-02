#=========================================================================
# Purpose: Generate Figures and Tables for DataWatch Piece
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

source('src/main.R')

#=========================================================================
# Figure 1
#   - Variability in batching vs sequencing within most common complaints
#=========================================================================

ed_data_variability <- final %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarise(variance = var(any.batch, na.rm = TRUE)) %>%
  arrange(desc(variance)) %>%
  top_n(10, variance)

print(ed_data_variability)
