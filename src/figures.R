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
  extended_model <- lm(as.formula(paste(dummy_var, '~ ED_PROVIDER')), patient_data)
  
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
continuous <- c('nEDTests', "ED_LOS", "RTN_72_HR", "RTN_72_HR_ADMIT")
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
  ggplot(aes(x = CHIEF_COMPLAINT, y = rate, fill = batch_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Chief Complaint\n",
       y = 'Batch-Ordering Frequency\n',
       title = 'Batching Rates for Top Chief Complaints') +
  scale_fill_manual(values = c("lab_batch_rate" = "#ffcc5c", 
                               "image_batch_rate" = "#ff6f69"),
                    labels = c("Image + Image Batching Rate", "Lab + Image Batching Rate")) +
  theme_minimal() +  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title = element_blank(),
        axis.title.y = element_text(color = 'black', size = 24),
        plot.title = element_text(color = "black", size = 28, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(1, 0.95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(color = "black", size = 16, face = 'bold'),
        legend.key.height = unit(1.1, "cm"))


# Save the plot to files
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.pdf", width = 20, height = 8)
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.png", width = 20, height = 8, bg = 'white')

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

complaints <- c('Upper Respiratory Symptoms',
               'Abdominal Complaints',
               'Back or Flank Pain',
               'Gastrointestinal Issues')

chief_complaint_freq <- table(data$CHIEF_COMPLAINT)
top_10_chief_complaints <- names(chief_complaint_freq)[order(chief_complaint_freq, decreasing = TRUE)][1:10]

data_for_plot$CHIEF_COMPLAINT <- ifelse(data_for_plot$CHIEF_COMPLAINT %in% complaints, 
                                        data_for_plot$CHIEF_COMPLAINT, "DROP")

data_for_plot$CHIEF_COMPLAINT <- ifelse(data_for_plot$CHIEF_COMPLAINT == 'Falls, Motor Vehicle Crashes, Assaults, and Trauma', 
                                        'Assaults and Trauma', data_for_plot$CHIEF_COMPLAINT)

data_for_plot <- data_for_plot %>%
  filter(CHIEF_COMPLAINT != 'DROP') %>%
  group_by(ED_PROVIDER, CHIEF_COMPLAINT) %>%
  summarize(batch_rate = mean(any.batch),
            batch.tendency = mean(batch.tendency)) %>% 
  ungroup() %>%
  unique()

data_for_plot <- data_for_plot %>%
  mutate(
    type = case_when(
      batch.tendency <= quantile(batch.tendency, 0.20, na.rm = TRUE) ~ "low propensity",
      batch.tendency >= quantile(batch.tendency, 0.80, na.rm = TRUE) ~ "high propensity",
      TRUE ~ 'middle'
    )
  )

data_for_plot %>%
  ggplot(aes(x = ED_PROVIDER, y = batch_rate, fill = type)) +
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
    strip.text.x = element_text(color = 'black', size = 15, face = "bold")
  ) +
  labs(
    x = '',
    y = 'Batch-Ordering Frequency\n',
    title = str_wrap("Physicians with <span style = 'color: #fe4a49;'>High Tendency to Batch</span> vs 
                     Physicians with <span style = 'color: #2ab7ca;'>Low Tendency to Batch</span>")) +
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
         group = 'Lab Test + Imaging Test Batch',
         tendency = ifelse(score > 0, 'high', 'low'))

data_for_plot.2 <- data_for_plot %>%
  select(ED_PROVIDER, avg_nEDTests, batch.tendency_ii) %>%
  group_by(ED_PROVIDER) %>%
  summarise(avg_nEDTests = mean(avg_nEDTests),
            batch.tendency_ii = mean(batch.tendency_ii)) %>%
  mutate(score = as.vector(scale(batch.tendency_ii)),
         group = 'Imaging Test + Imaging Test Batch',
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
  filter(ESI != 5)

data_for_plot$ESI <- as.factor(data_for_plot$ESI)

# Create the crowdedness variable based on the quartiles
data_for_plot$crowdedness <- cut(data_for_plot$patients_in_hospital, 
                      breaks = quantile(data_for_plot$patients_in_hospital, 
                                        probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
                      labels = c("very low", "low", "medium", "high", 'very high'), 
                      include.lowest = TRUE)

complaints <- c('Abdominal Complaints', 'Back or Flank Pain', 
                'Upper Respiratory Symptoms', 'Gastrointestinal Issues')

# Define a function to perform the modeling and averaging for a given complaint and outcome
model_for_complaint <- function(complaint, outcome_var) {
  formula <- as.formula(paste(outcome_var, "~ batch.tendency*ESI + batch.tendency*crowdedness"))
  
  mod <- lm(formula, data = filter(data_for_plot, CHIEF_COMPLAINT == complaint))
  
  me <- avg_slopes(
    mod,
    by = c("ESI", "crowdedness"),
    variables = "batch.tendency")
  
  me$complaint <- complaint
  me$Outcome <- outcome_var
  return(me)
}

# Use lapply to apply the function to each complaint and then bind the results together
me_list.1 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'nEDTests'))
me_list.2 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'ln_ED_LOS'))
me_list.3 <- lapply(complaints, function(complaint) model_for_complaint(complaint, 'RTN_72_HR'))

me.1 <- bind_rows(me_list)
me.2 <- bind_rows(me_list)
me.3 <- bind_rows(me_list)

me <- bind_rows(me.1,me.2,me.3)

data.frame(me) %>%
  mutate(batch = ifelse(estimate < 0, 'Batcher', 'Non-Batcher'),
         batch = ifelse(p.value > 0.1, 'No Difference', batch)) %>%
  ggplot(aes(y=ESI, x=crowdedness, fill=batch)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  facet_grid(Outcome ~ complaint) +
             #labeller = as_labeller(c(nEDTests = "Decrease \nNumber of Tests",
            #                          ln_ED_LOS = "Decrease \nLength of Stay"
            #                          ,RTN_72_HR = "Decrease \nLiklihood of 72hr Return"))) +
  scale_fill_manual(values = c('Non-Batcher' = "#E18727", 
                               'Batcher' = "#0072B5",
                               'No Difference' = 'grey60'), 
                    name = "Optimal Testing Strategy") +
  coord_equal() +
 # scale_x_discrete(labels = function(x) {
  #  case_when(
   #   x == "Low" ~ str_wrap("Less Crowded ←", width = 5),
  #    x == "High" ~ str_wrap("More Crowded →", width = 5),
  #    TRUE ~ ""
  #  )
 # }) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size=18, color='black'),
    axis.text.x = element_text(size=18, color='black'), 
    panel.grid.major = element_line(color = 'black', size = 0.3),
    legend.position = 'bottom',
    axis.title.y = element_text(color = 'black', size = 20),
    axis.title.x = element_blank(),
    strip.text.x = element_text(color = 'black', size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = 18, face = 'bold')
  ) +
  labs(y = "Emergency Severity Index\n")














# Pre-processing data for plotting
data_for_plot <- data %>%
  mutate(crowdedness_category = cut(overlap_per_min, 
                                    breaks = quantile(overlap_per_min, 
                                                      probs = seq(0, 1, by = 0.2), 
                                                      na.rm = TRUE),
                                    labels = c("Very Low", "Low", "Medium", 
                                               "High", "Very High"),
                                    include.lowest = TRUE)) %>%
  filter(ESI != 5)


# Extract unique combinations of ESI and crowdedness_category
unique_combinations <- distinct(data_for_plot, ESI, crowdedness_category)

# List of outcomes
outcomes <- c("nEDTests", "ln_ED_LOS", "RTN_72_HR")

# Initialize an empty dataframe for results
results_df <- data.frame()

# Loop through outcomes and unique combinations to run regressions
for(outcome in outcomes) {
  for(i in seq_len(nrow(unique_combinations))) {
    
    subset_data <- filter(data_for_plot, 
                          ESI == unique_combinations$ESI[i], 
                          crowdedness_category == unique_combinations$crowdedness_category[i])
    
    model <- lm(as.formula(paste0(outcome, " ~ type")), data = subset_data)
    
    current_results <- data.frame(Outcome = outcome,
                                  ESI = unique_combinations$ESI[i],
                                  crowdedness_category = unique_combinations$crowdedness_category[i],
                                  coef_any_batch = coef(model)['typeNon-Batcher'],
                                  p_value_any_batch = summary(model)$coefficients['typeNon-Batcher', 'Pr(>|t|)'])
    
    results_df <- rbind(results_df, current_results)
  }
}
results_df
# Plot the results
results_df %>%
  mutate(batch = ifelse(coef_any_batch < 0, 'Non-Batcher', 'Batcher'),
         batch = ifelse(p_value_any_batch > 0.1, 'Unclear', batch)) %>%
  ggplot(aes(y=ESI, x=crowdedness_category, fill=batch)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  facet_wrap(~Outcome, nrow=1, 
             labeller = as_labeller(c(nEDTests = "Decrease \nNumber of Tests",
                                      ln_ED_LOS = "Decrease \nLength of Stay"
                                      , RTN_72_HR = "Decrease \nLiklihood of 72hr Return"))) +
  scale_fill_manual(values = c('Non-Batcher' = "#E18727", 
                               'Batcher' = "#0072B5"), 
                    name = "Optimal Testing Strategy") +
  coord_equal() +
  scale_x_discrete(labels = function(x) {
    case_when(
      x == "Low" ~ str_wrap("Less Crowded ←", width = 5),
      x == "High" ~ str_wrap("More Crowded →", width = 5),
      TRUE ~ ""
    )
  }) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size=18, color='black'),
    axis.text.x = element_text(size=18, color='black'), 
    panel.grid.major = element_line(color = 'black', size = 0.3),
    legend.position = 'bottom',
    axis.title.y = element_text(color = 'black', size = 20),
    axis.title.x = element_blank(),
    strip.text.x = element_text(color = 'black', size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_text(size = 18, face = 'bold')
  ) +
  labs(y = "Emergency Severity Index\n")

# Save the plot to files
ggsave("manuscript/figures/decision.pdf", width = 12, height = 10)
ggsave("manuscript/figures/decision.png", width = 12, height = 10, bg = 'white')




# Step 2: Compute the AMEs
library(marginaleffects)

ggpredict(mod, terms = c("batch.tendency", "ESI", "crowdedness_category"))


mod
ame_results <- margins(mod, variables = "type", 
                       at = list(ESI = levels(data_for_plot$ESI),
                                 crowdedness_category = levels(data_for_plot$crowdedness_category)))

# Step 3: Display the results
summary(ame_results)




