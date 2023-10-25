#=========================================================================
# Purpose: Generate Figures and Tables for DataWatch Piece
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

library(grid)
library(ggtext)
library(tidyverse)
library(caret)
library(sandwich)
library(xtable)
library(lmtest)
library(gtsummary)

data <- read_csv('final.csv')

#=========================================================================
# Table: Balance Table (Wald Test) across Physicians
# 
# Table reports the results of a Wald test which was conducted to assess 
# the balance of chief complaints across providers in our dataset. A
# balanced distribution implies that complaints and severity are evenly 
# distributed across providers, which we expect to be the case due to 
# randomization. The Wald F-statistic and p-value are reported.
# Robust standard errors (type HC1) were used to account for potential
# heteroscedasticity in the data.
#=========================================================================
#------------------------------
# Data Preprocessing
#------------------------------

# Copy the original dataset
patient_data <- data

# Get top 10 chief complaints based on frequency
chief_complaint_freq <- table(patient_data$CHIEF_COMPLAINT)
top_10_chief_complaints <- names(chief_complaint_freq)[order(chief_complaint_freq, decreasing = TRUE)][1:10]

# If chief complaint is not in top 10, label it as "DROP"
patient_data$CHIEF_COMPLAINT <- ifelse(patient_data$CHIEF_COMPLAINT %in% top_10_chief_complaints, 
                                       patient_data$CHIEF_COMPLAINT, "DROP")

# One-hot encoding for CHIEF_COMPLAINT
dummy_encoder <- dummyVars(" ~ CHIEF_COMPLAINT", data = patient_data)
one_hot_encoded_data <- data.frame(predict(dummy_encoder, newdata = patient_data))
relevant_vars <- setdiff(names(one_hot_encoded_data), 'CHIEF_COMPLAINTDROP')

# Add the one-hot encoded data to the main dataset
patient_data <- cbind(patient_data, one_hot_encoded_data)

# Create binary flags for ESI values
patient_data$ESI_1_or_2 <- ifelse(patient_data$ESI == 1 | patient_data$ESI == 2, 1, 0)
patient_data$ESI_3_or_4_or_5 <- ifelse(patient_data$ESI %in% c(3, 4, 5), 1, 0)

# Update the relevant variables list
relevant_vars <- c(relevant_vars, 'ESI_1_or_2', 'ESI_3_or_4_or_5', 
                   "US_PERF", "NON_CON_CT_PERF", "CON_CT_PERF", 
                   "LAB_PERF", "XR_PERF", "tachycardic", 
                   "tachypneic","febrile","hypotensive")

#-------------------------------------------------
# Modeling & Balance Statistics
#-------------------------------------------------

# Initialize balance dataframe
balance_stats <- data.frame(Df = numeric(), 
                            F = numeric(),
                            Pr_F = numeric(), 
                            dummy_var = character())

# Loop through each variable and collect balance statistics
for (dummy_var in relevant_vars) {
  baseline_model <- lm(as.formula(paste(dummy_var, '~ 1')), patient_data)
  extended_model <- lm(as.formula(paste(dummy_var, '~ batcher')), patient_data)
  
  wald_test_result <- lmtest::waldtest(baseline_model, extended_model, 
                               vcov = vcovHC(extended_model, type = "HC1"))
  
  temp_result <- data.frame(wald_test_result)[2, c(2,3,4)]
  temp_result$dummy_var <- dummy_var
  
  balance_stats <- rbind(balance_stats, temp_result)
}

row.names(balance_stats) <- NULL

# Print the results in LaTeX format
latex_output <- capture.output(print(xtable(balance_stats[,c(2,3,4)], 
                                            caption = "Wald Test Results", 
                                            digits = c(0,3,3,0)), type = "latex"))


latex_preamble <- "
\\documentclass{article}
\\usepackage{graphicx}
\\usepackage{pdflscape}
\\begin{document}
\\begin{landscape}
"

latex_postamble <- "
\\end{landscape}
\\end{document}
"

writeLines(print(c(latex_preamble, latex_output, latex_postamble), 
                 type = "latex", print.results=FALSE), 
           "manuscript/tables and figures/Balance_Table.tex")

# Compile the LaTeX file to produce a PDF
tinytex::pdflatex('manuscript/tables and figures/Balance_Table.tex')

#=========================================================================
# Table: Summary Statistics of Key Outcomes
# 
# This table shows how outcomes of interest vary across batchers and
# and non-batchers. Batcher is determined based on having a tendency to 
# batch that is greater than the average. More detail can be found in the
# the paper.
#=========================================================================

categorical <- c('dispo')
continuous <- c('nEDTests', "ED_LOS", "RTN_72_HR", "RTN_72_HR_ADMIT",
                "US_PERF", "NON_CON_CT_PERF", "CON_CT_PERF", 
                "LAB_PERF", "XR_PERF")

data$type <- ifelse(data$batch.tendency > 0, 'Batcher', 'Non-Batcher')

table <- data %>%
  select(all_of(c(categorical, continuous)), type) %>%
  tbl_summary(by = type, type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})"),
              missing_text = "(Missing)") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        test = list(all_continuous() ~ "t.test",
                    all_categorical() ~ "chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Summary Statistics for Batcher vs. Non-Batcher**") 


# Save the table to files
gt::gtsave(as_gt(table), "manuscript/tables and figures/Outcomes_Summary_Table.pdf")
gt::gtsave(as_gt(table), "manuscript/tables and figures/Outcomes_Summary_Table.png")


table.2 <- data %>%
  filter(nEDTests >= 2) %>%
  select(all_of(c(categorical, continuous)), type) %>%
  tbl_summary(by = type, type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})"),
              missing_text = "(Missing)") %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2),
        test = list(all_continuous() ~ "t.test",
                    all_categorical() ~ "chisq.test")) %>%
  add_overall() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Summary Statistics for Batcher vs. Non-Batcher**") 

# Save the table to files
gt::gtsave(as_gt(table.2), "manuscript/tables and figures/Outcomes_Summary_Table_2+tests.pdf")
gt::gtsave(as_gt(table.2), "manuscript/tables and figures/Outcomes_Summary_Table_2+test.png")

#=========================================================================
# Figure: Batching Rates for Top Chief Complaints
#
# Figure displays two statistics for the ten most common major chief 
# complaint categories observed in our ED visit sample: the un-adjusted 
# batching rate for lab-image batching and image-image batching.
#=========================================================================

# Data Preparation
#----------------------

# Calculate batching rates for each chief complaint
batching_rates <- data %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarise(total_cases = n(),
            lab_batch_rate = sum(lab_image_batch)/total_cases,
            image_batch_rate = sum(image_image_batch)/total_cases) %>%
  arrange(desc(total_cases))

# Filter for the top 10 chief complaints
top_complaints_df <- batching_rates %>%
  top_n(10, total_cases)

# Convert to long format for plotting
plot_data <- top_complaints_df %>%
  pivot_longer(cols = c(image_batch_rate, lab_batch_rate),
               names_to = "batch_type",
               values_to = "rate")


plot_data %>%
  mutate(CHIEF_COMPLAINT = ifelse(CHIEF_COMPLAINT == "Falls, Motor Vehicle Crashes, Assaults, and Trauma",
                                  "Assaults and Trauma",  CHIEF_COMPLAINT)) %>%
  ggplot(aes(x = CHIEF_COMPLAINT, y = rate, fill = batch_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Chief Complaint\n",
       y = 'Batch-Ordering Frequency\n') +
  scale_fill_manual(values = c("lab_batch_rate" = "#09264a", 
                               "image_batch_rate" = "#f99820"),
                    labels = c("Image + Image Batching Rate", "Lab + Image Batching Rate")) +
  theme_minimal() +  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 28),
        axis.title = element_blank(),
        axis.title.y = element_text(color = 'black', size = 28),
        plot.title = element_text(color = "black", size = 28, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = "black", size = 28))


# Save the plot to files
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.pdf", width = 20, height = 8)
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.png", width = 23, height = 12, bg = 'white')

#=========================================================================
# Figure: Systematic Variation in Tendency to Batch Visualization
# 
# Figure illuminates the marked differences among physicians 
# in their propensity to batch-order diagnostic tests. Physicians are
# mapped on the x-axis, revealing those with a systematically heightened 
# tendency to batch (in orange) compared to their peers who batch less 
# frequently (in blue).
#=========================================================================

data_for_plot <- data

complaints <- data %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarize(var.batch = var(any.batch)) %>%
  arrange(desc(var.batch)) %>% 
  head(4)

complaints <- complaints$CHIEF_COMPLAINT

data_for_plot <- data_for_plot %>%
  filter(CHIEF_COMPLAINT %in% complaints) %>%
  group_by(ED_PROVIDER, CHIEF_COMPLAINT) %>%
  summarize(batch_rate = mean(any.batch),
            avg.batch.tendency = mean(avg.batch.tendency)) %>% 
  ungroup() 

data_for_plot <- data_for_plot %>%
  mutate(
    type = case_when(
      avg.batch.tendency <= quantile(avg.batch.tendency, 0.25, na.rm = TRUE) ~ "low propensity",
      avg.batch.tendency >= quantile(avg.batch.tendency, 0.75, na.rm = TRUE) ~ "high propensity",
      TRUE ~ 'middle'
    )
  )

data_for_plot$type <- factor(data_for_plot$type, 
                             levels = c("low propensity", "middle", "high propensity"))

data_for_plot$ED_PROVIDER <- with(data_for_plot, 
                                  reorder(ED_PROVIDER, type))


data_for_plot %>%
  arrange(type) %>%
  ggplot(aes(x = fct_inorder(ED_PROVIDER), y = batch_rate, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~CHIEF_COMPLAINT, nrow = 1) +
  theme_bw() + 
  scale_fill_manual(values = c("low propensity" = "#2ab7ca", 
                               "high propensity" = "#fe4a49",
                               'middle' = 'grey80')) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.background = element_rect(fill = 'white'),
    axis.text.y = element_text(size = 16, color = 'black'), 
    axis.text.x = element_blank(),
    plot.title = element_text(size=18, face = 'bold', hjust = 0.5),
    plot.margin = unit(c(1, 0.2, 0.2, 0.2), "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    legend.position = 'none',
    axis.title.y = element_text(color = 'black', size = 18),
    axis.title.x = element_text(color = 'black', size = 18),
    strip.text.x = element_text(color = 'black', size = 12, face = "bold")
  ) +
  labs(
    x = '',
    y = 'Batch-Ordering Frequency\n',
    title = str_wrap("<span style = 'color: #2ab7ca;'>Physicians in Bottom 25% of Batchers</span> vs.
                      <span style = 'color: #fe4a49;'>Physicians in Top 25% of Batchers</span>")) +
  theme(plot.title = element_markdown())


# Save the plot to files
ggsave("manuscript/tables and figures/Physician Variation.pdf", width = 14, height = 5)
ggsave("manuscript/tables and figures/Physician Variation.png", width = 14, height = 5, bg = 'white')

#=========================================================================

# This figure displays the relationship between the standardized 
# batch-ordering rate and the average number of tests ordered per
# encounter for physicians within our study sample. Notably, physicians 
# with a standardized batch-ordering rate above 0 (i.e., a batch rate
# greater than the average of the sample) tend to order more diagnostic
# tests on average as compared to physicians with a standardized 
# batch-ordering rate below 0 (i.e., a batch rate below the 
# average of the sample)
#=========================================================================

data_for_plot <- data

data_for_plot.1 <- data_for_plot %>%
  select(ED_PROVIDER, avg_nEDTests, batch.tendency_li) %>%
  group_by(ED_PROVIDER) %>%
  summarise(avg_nEDTests = mean(avg_nEDTests),
            batch.tendency_li = mean(batch.tendency_li)) %>%
  mutate(score = as.vector(scale(batch.tendency_li)),
         group = 'Lab + Image Batch Tendency',
         tendency = ifelse(score > 0, 'high', 'low'))

data_for_plot.2 <- data_for_plot %>%
  select(ED_PROVIDER, avg_nEDTests, batch.tendency_ii) %>%
  group_by(ED_PROVIDER) %>%
  summarise(avg_nEDTests = mean(avg_nEDTests),
            batch.tendency_ii = mean(batch.tendency_ii)) %>%
  mutate(score = as.vector(scale(batch.tendency_ii)),
         group = 'Image + Image Batch Tendency',
         tendency = ifelse(score > 0, 'high', 'low'))

data_for_plot <- bind_rows(data_for_plot.1, data_for_plot.2) 

data_for_plot %>%
  ggplot()  +
  geom_point(aes(y=avg_nEDTests,x = score, 
                 color=tendency),size=5, stroke=1) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#fe4a49", alpha = 0.008) +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = "#2ab7ca", alpha = 0.008) +
  geom_smooth(aes(y=avg_nEDTests,x = score), method = "lm",se = T) +
  facet_wrap(~group, ncol = 2) + 
  theme_bw() + 
  scale_color_manual(values = c("low" = "#2ab7ca", 
                               "high" = "#fe4a49")) + 
  theme(plot.background=element_rect(fill='white'),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=20, color='black'), 
        axis.text.x  = element_text(size=20),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position = 'none',
        axis.title.y =  element_text(color = 'black',size = 20),
        axis.title.x = element_text(color = 'black',size = 20),
        strip.text.x = element_text(color = 'black', size = 20, face = "bold"),
        plot.title = element_text(color = "black", size = 30, 
                                  face = "bold", margin = margin(0,0,30,0), hjust = 0),
       plot.subtitle = element_text(color = "black", size = 14, 
                                    margin = margin(0,0,30,0),hjust = 0),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16, face = 'bold'),
        legend.background = element_rect(fill = "grey96", color = NA)) +
  labs(x='\nStandardized Batch-Ordering Rate',
       y='Average Number of Tests \nOrdered per Encounter\n')

# Save the plot to files
ggsave("manuscript/tables and figures/Batching and Avg Tests.pdf", width = 14, height = 7)
ggsave("manuscript/tables and figures/Batching and Avg Tests.png", width = 14, height = 7, bg = 'white')

#=========================================================================
# Figure: Optimal Testing Strategy Visualization
# 
# This figure visualizes when it's most optimal to either batch or sequence tests 
# based on the crowdedness and the Emergency Severity Index (ESI). We explore 
# outcomes such as the number of tests, length of stay, and 72-hour returns.
#=========================================================================
library(marginaleffects)

data_for_plot <- data %>%
  mutate(ESI_cat = case_when(
    ESI == 1 ~ 'ESI 1',
    ESI == 2 ~ 'ESI 2',
    ESI == 3 ~ 'ESI 3, 4, or 5',
    ESI == 4 ~ 'ESI 3, 4, or 5',
    ESI == 5 ~ 'ESI 3, 4, or 5'))

data_for_plot$ESI_cat <- as.factor(data_for_plot$ESI_cat)

# Create the crowdedness variable based on the quartiles
data_for_plot$crowdedness <- cut(data_for_plot$patients_in_hospital, 
                      breaks = quantile(data_for_plot$patients_in_hospital, 
                                        probs = c(0, 0.5, 0.75, 1)), 
                      labels = c("low", "medium", "high"), 
                      ordered = T,
                      include.lowest = TRUE)

complaints <- c('Upper Respiratory Symptoms', 'Dizziness/Lightheadedness/Syncope', 
                'Abdominal Complaints', 'Neurological Issue')

# Define a function to perform the modeling and averaging for a given complaint and outcome
model_for_complaint <- function(complaint, outcome_var) {
  formula <- as.formula(paste(outcome_var, "~ z_batch_rate*ESI_cat + z_batch_rate*crowdedness"))
  
  mod <- lm(formula, data = filter(data_for_plot, CHIEF_COMPLAINT == complaint))
  
  me <- avg_slopes(
    mod,
    by = c("ESI_cat", "crowdedness"),
    variables = "z_batch_rate")
  
  me$complaint <- complaint
  me$Outcome <- outcome_var
  return(me)
}

# Use lapply to apply the function to each complaint and then bind the results together
me_list.1 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'nEDTests'))
me_list.2 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'ln_ED_LOS'))
me_list.3 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'RTN_72_HR'))

me.1 <- bind_rows(me_list.1)
me.2 <- bind_rows(me_list.2)
me.3 <- bind_rows(me_list.3)

me <- bind_rows(me.1,me.2,me.3)


data.frame(me) %>%
  mutate(batch = ifelse(estimate < 0, 'Batcher', 'Non-Batcher'),
         batch = ifelse(p.value > 0.1, 'No Preference', batch)) %>%
  complete(ESI_cat, crowdedness, Outcome, complaint, fill = list(batch = 'No Preference')) %>%
  mutate(crowdedness = factor(crowdedness, levels = c("low", "medium", "high"))) %>%
  ggplot(aes(y=ESI_cat, x=crowdedness, fill=batch)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  facet_grid(complaint ~ Outcome,
             labeller = as_labeller(c(nEDTests = "Objective:\nDecrease Number of Tests",
                                      ln_ED_LOS = "Objective: \nDecrease Length of Stay",
                                      RTN_72_HR = "Objective: \nDecrease Liklihood of 72hr Return",
                                      `Abdominal Complaints` = 'Abdominal \nComplaints',
                                      `Dizziness/Lightheadedness/Syncope` = 'Dizziness/\nLightheadedness',
                                      `Upper Respiratory Symptoms` = 'Upper Respiratory \nSymptoms',
                                      `Neurological Issue` = 'Neurological \nIssue')) ) +
  scale_fill_manual(values = c('Non-Batcher' = "#2ab7ca", 
                               'Batcher' = "#fe4a49",
                               'No Preference' = 'grey60'), 
                    name = "Preferred Physician Type") +
  scale_x_discrete(labels = function(x) {
    case_when(
     x == "low" ~ str_wrap("Normal Operations", width = 5),
     x == "medium" ~ str_wrap("Minor Overcapacity", width = 5),
      x == "high" ~ str_wrap("Major Overcapacity", width = 5),
      TRUE ~ ""
    )
  }) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size=12, color='black'),
    axis.text.x = element_text(size=12, color='black'), 
    panel.grid.major = element_line(color = 'black', size = 0.3),
    legend.position = 'bottom',
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.text.x = element_text(color = 'black', size = 12, face = "bold"),
    strip.text.y = element_text(color = 'black', size = 12),
    legend.text = element_text(size = 12),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = 12, face = 'bold')
  ) 

# Save the plot to files
ggsave("manuscript/tables and figures/decision.pdf", width = 12, height = 8)
ggsave("manuscript/tables and figures/decision.png", width = 11.5, height = 8, bg = 'white')




