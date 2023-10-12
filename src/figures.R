#=========================================================================
# Purpose: Generate Figures and Tables for DataWatch Piece
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

library(grid)
library(shadowtext)
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

writeLines(print(c(latex_preamble, latex_output, latex_postamble), type = "latex", print.results=FALSE), 
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

# Plotting
#------------

plot_data %>%
  ggplot(aes(x = CHIEF_COMPLAINT, y = rate, fill = batch_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Chief Complaint\n",
       y = '\nBatching Rate',
       title = 'Batching Rates for Top Chief Complaints') +
  scale_fill_manual(values = c("lab_batch_rate" = "#00539C", 
                               "image_batch_rate" = "#EEA47F"),
                    labels = c("Image + Image Batching Rate", "Lab + Image Batching Rate")) +
  theme_minimal() +  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_text(color = "black", size = 20),
        axis.title = element_blank(),
        plot.title = element_text(color = "black", size = 28, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(1, 0.95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.text = element_text(color = "black", size = 16, face = 'bold'),
        legend.key.height = unit(1.1, "cm"))


# Save the plot to files
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.pdf", width = 20, height = 10)
ggsave("manuscript/tables and figures/Lab and Image Batch Rates.png", width = 20, height = 10, bg = 'white')


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
  summarize(batch_rate = mean(any.batch))



# Step 1: Compute the average batch_rate for each provider
provider_avg_rate <- data_for_plot %>%
  group_by(ED_PROVIDER) %>%
  summarize(avg_rate = mean(batch_rate, na.rm = TRUE)) %>%
  arrange(-avg_rate) # descending order

# Step 2: Select the top 4 and bottom 4 providers
top_providers <- head(provider_avg_rate$ED_PROVIDER, 4)
bottom_providers <- tail(provider_avg_rate$ED_PROVIDER, 4)

# Step 3: Filter the data for these providers and then compare their batch rates
top_data <- data_for_plot %>% filter(ED_PROVIDER %in% top_providers)
bottom_data <- data_for_plot %>% filter(ED_PROVIDER %in% bottom_providers)

all_complaints <- unique(data_for_plot$CHIEF_COMPLAINT)
top_higher_than_bottom <- all(sapply(all_complaints, function(complaint) {
  min_top <- min(top_data$batch_rate[top_data$CHIEF_COMPLAINT == complaint], na.rm = TRUE)
  max_bottom <- max(bottom_data$batch_rate[bottom_data$CHIEF_COMPLAINT == complaint], na.rm = TRUE)
  return(min_top > max_bottom)
}))

# Step 4: Create an indicator variable
data_for_plot <- data_for_plot %>%
  mutate(is_selected = case_when(
    (ED_PROVIDER %in% top_providers & top_higher_than_bottom) ~ 2,
    (ED_PROVIDER %in% bottom_providers & !top_higher_than_bottom) ~ 1,
    TRUE ~ 0
  ))


data_for_plot


data_for_plot %>%
  ggplot(., aes(x = ED_PROVIDER, y = batch_rate)) +
  geom_bar(stat = "identity", position = "dodge", fill="#CBC3E3") +
  facet_wrap(~CHIEF_COMPLAINT, nrow=1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.background=element_rect(fill='white'),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=14, color='black'), 
        axis.text.x  = element_blank(),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position = 'none',
        axis.title.y =  element_text(color = 'black', size = 14),
        axis.title.x = element_text(color = 'black', size = 18),
        strip.text.x = element_text(color = 'black', size = 12, face = "bold"),
        legend.title.align=0.5) +
  labs(x='',
       y='Batch-Ordering Frequency\n\n')

################################









#=========================================================================
# Figure: Optimal Testing Strategy Visualization
# 
# This figure visualizes when it's most optimal to either batch or sequence tests 
# based on the crowdedness and the Emergency Severity Index (ESI). We explore 
# outcomes such as the number of tests, length of stay, and 72-hour returns.
#=========================================================================

# Pre-processing data for plotting
data_for_plot <- final %>%
  mutate(crowdedness_category = cut(overlap_per_min, 
                                    breaks = quantile(overlap_per_min, 
                                                      probs = seq(0, 1, by = 0.2), 
                                                      na.rm = TRUE),
                                    labels = c("Very Low", "Low", "Medium", 
                                               "High", "Very High"),
                                    include.lowest = TRUE)) %>%
  filter(ESI != 5, nEDTests >0)

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
    
    model <- lm(as.formula(paste0(outcome, " ~ any.batch + CHIEF_COMPLAINT + age_groups")), data = subset_data)
    
    current_results <- data.frame(Outcome = outcome,
                                  ESI = unique_combinations$ESI[i],
                                  crowdedness_category = unique_combinations$crowdedness_category[i],
                                  coef_any_batch = coef(model)['any.batch'],
                                  p_value_any_batch = summary(model)$coefficients['any.batch', 'Pr(>|t|)'])
    
    results_df <- rbind(results_df, current_results)
  }
}

# Plot the results
results_df %>%
  mutate(batch = ifelse(coef_any_batch < 0, 'Batch', 'Sequence')) %>%
  ggplot(aes(y=ESI, x=crowdedness_category, fill=batch)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) + 
  facet_wrap(~Outcome, nrow=1, labeller = as_labeller(c(nEDTests = "Number of Tests", ln_ED_LOS = "Length of Stay", RTN_72_HR = "72 Hour Return"))) +
  scale_fill_manual(values = c("Batch" = "#E18727", "Sequence" = "#0072B5"), name = "Optimal Testing Strategy") +
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

fig.3 <- final

fig.3 %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarise(n = n(), 
            variability.any = var(any.batch, na.rm = TRUE),
            variability.img = var(image_image_batch, na.rm = TRUE),
            variability.lab = var(lab_image_batch, na.rm = TRUE)) %>%
  arrange(desc(variability.any))

fig.3 %>%
  group_by(ED_PROVIDER) %>%
  summarise(n = n(), 
            avg.tests = mean(nEDTests),
            variability.any = mean(any.batch, na.rm = TRUE),
            variability.img = mean(image_image_batch, na.rm = TRUE),
            variability.lab = mean(lab_image_batch, na.rm = TRUE),
            top.tester = as.factor(ifelse(avg.tests >= 1.699777, 1, 0)))  %>%
  ggplot()  +
  geom_segment(aes(x=variability.img, xend=variability.lab, y=reorder(ED_PROVIDER,variability.lab), 
                   yend=reorder(ED_PROVIDER,variability.lab)), size=1.5)  +
  geom_point(aes(y=reorder(ED_PROVIDER,variability.lab),x=variability.lab, 
                 color=avg.tests, shape="Lab + Image Batch"),size=8, stroke=2) +
  geom_point(aes(y=reorder(ED_PROVIDER,variability.lab),x=variability.img, color=avg.tests, shape="Image + Image Batch"),
             size=8, stroke=2) + 
  scale_color_gsea(name="Average number of tests \nordered per visit",
                   guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                        keywidth=unit(15, units = "mm"), 
                                        label.position = "bottom", 
                                        title.position = 'top', nrow=1)) +
  scale_shape_manual(values=c("Image + Image Batch" = 17, "Lab + Image Batch" = 19),
                     name = "Type of Batch",
                     guide = guide_legend(override.aes = list(size=4))) +
  theme_minimal() +
  theme(plot.background=element_rect(fill='white'),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=20, color='black'), 
        axis.text.x  = element_text(size=20),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position = c(.85, 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(
          color = 'black',
          size = 18),
        legend.title.align=0.5,
        plot.title = element_text(
          color = "black", 
          size = 70, 
          face = "bold",
          margin = margin(0,0,30,0),
          hjust = 0,
        ),
        plot.subtitle = element_text(
          color = "black", 
          size = 18,
          margin = margin(0,0,30,0),
          hjust = 0
        ),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16, face = 'bold'),
        legend.background = element_rect(fill = "grey96", color = NA),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "black", 
          size = 9,
          lineheight = 1.2, 
          hjust = 1,
          margin = margin(t = 30) 
        )) +
  labs(x='\nBatch-Ordering Rate',
       title="Batching Rates by Physician",
       fill = '',
       subtitle=str_wrap('The physicians with the highest propensity to batch are
                         also those that tend to order the greatest number of tests.\n', 160))



temp <- fig.3 %>%
  group_by(ED_PROVIDER) %>%
  summarise(n = n(), 
            avg.tests = mean(nEDTests),
            variability = mean(image_image_batch, na.rm = TRUE),
            group = 'Imaging Test + Imaging Test Batch') %>%
  ungroup() %>%
  mutate(score = (variability - mean(variability)) / sd(variability))


temp.2 <- fig.3 %>%
  group_by(ED_PROVIDER) %>%
  summarise(n = n(), 
            avg.tests = mean(nEDTests),
            variability = mean(lab_image_batch, na.rm = TRUE),
            group = 'Lab Test + Imaging Test Batch') %>%
  ungroup() %>%
  mutate(score = (variability - mean(variability)) / sd(variability))

bind_rows(temp, temp.2) %>%
  ggplot()  +
  geom_point(aes(y=avg.tests,x = score, fill='grey50'),size=5, stroke=2) +
  geom_smooth(aes(y=avg.tests,x = score), method = "lm",se = T) +
  facet_wrap(~group, ncol = 2) + 
  theme_minimal() +
  theme(plot.background=element_rect(fill='white'),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=20, color='black'), 
        axis.text.x  = element_text(size=20),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position = 'none',
        axis.title.y =  element_text(
          color = 'black',
          size = 18),
        axis.title.x = element_text(
          color = 'black',
          size = 18),
        strip.text.x = element_text(
          color = 'black',
          size = 18),
        legend.title.align=0.5,
        plot.title = element_text(
          color = "black", 
          size = 30, 
          face = "bold",
          margin = margin(0,0,30,0),
          hjust = 0,
        ),
        plot.subtitle = element_text(
          color = "black", 
          size = 14,
          margin = margin(0,0,30,0),
          hjust = 0
        ),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16, face = 'bold'),
        legend.background = element_rect(fill = "grey96", color = NA),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "black", 
          size = 9,
          lineheight = 1.2, 
          hjust = 1,
          margin = margin(t = 30) 
        )) +
  labs(x='\nStandardized Batch-Ordering Rate',
       y='Average Number of Tests Ordered per Encounter\n\n')
       #title="Relationship Between Batch Tendency and Over-Testing",
       #fill = '',
       #subtitle=str_wrap('The physicians with the highest propensity to batch are
                         #also those that tend to order the greatest number of tests.\n', 160))

  
bind_rows(temp, temp.2) %>%
  mutate(above_ii = ifelse(group == 'Imaging Test + Imaging Test Batch' & score > 0, 1, 0),
         above_li = ifelse(group == 'Lab Test + Imaging Test Batch' & score > 0, 1, 0)) %>%
  group_by(group, above_ii) %>%
  summarise(avgt = mean(avg.tests))

bind_rows(temp, temp.2) %>%
  mutate(above_ii = ifelse(group == 'Imaging Test + Imaging Test Batch' & score > 0, 1, 0),
         above_li = ifelse(group == 'Lab Test + Imaging Test Batch' & score > 0, 1, 0)) %>%
  group_by(group, above_li) %>%
  summarise(avgt = mean(avg.tests))

#=========================================================================
# Figure 3
#   - Show that across physician, there is variation in batching and 
#     propensity to test
#=========================================================================

fig.4 <- final

variation <- c('Upper Respiratory Symptoms',
                'Abdominal Complaints',
                'Back or Flank Pain',
               'Gastrointestinal Issues')

chief_complaint_freq <- table(fig.4$CHIEF_COMPLAINT)
top_10_chief_complaints <- names(chief_complaint_freq)[order(chief_complaint_freq, decreasing = TRUE)][1:10]
fig.4$CHIEF_COMPLAINT <- ifelse(fig.4$CHIEF_COMPLAINT %in% variation, fig.4$CHIEF_COMPLAINT, "DROP")

fig.4$CHIEF_COMPLAINT <- ifelse(fig.4$CHIEF_COMPLAINT == 'Falls, Motor Vehicle Crashes, Assaults, and Trauma', 'Assaults and Trauma', fig.4$CHIEF_COMPLAINT)
fig.4 %>%
  filter(CHIEF_COMPLAINT != 'DROP') %>%
  group_by(ED_PROVIDER, CHIEF_COMPLAINT) %>%
  summarize(batch_rate = mean(any.batch)) %>%
  ggplot(., aes(x = ED_PROVIDER, y = batch_rate)) +
  geom_bar(stat = "identity", position = "dodge", fill="#CBC3E3") +
  facet_wrap(~CHIEF_COMPLAINT, nrow=1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.background=element_rect(fill='white'),
        panel.border = element_blank(),
        axis.text.y  = element_text(size=14, color='black'), 
        axis.text.x  = element_blank(),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.grid.major=element_line(color='grey85',size=0.3),
        legend.position = 'none',
        axis.title.y =  element_text(
          color = 'black',
          size = 14),
        axis.title.x = element_text(
          color = 'black',
          size = 18),
        strip.text.x = element_text(
          color = 'black',
          size = 12,
          face = "bold"),
        legend.title.align=0.5,
        plot.title = element_text(
          color = "black", 
          size = 30, 
          face = "bold",
          margin = margin(0,0,30,0),
          hjust = 0,
        ),
        plot.subtitle = element_text(
          color = "black", 
          size = 14,
          margin = margin(0,0,30,0),
          hjust = 0
        ),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16, face = 'bold'),
        legend.background = element_rect(fill = "grey96", color = NA),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "black", 
          size = 9,
          lineheight = 1.2, 
          hjust = 1,
          margin = margin(t = 30) 
        )) +
  labs(x='',
       y='Batch-Ordering Frequency\n\n',
       #title="Variation in Batch-Ordering by Provider",
       fill = '')

################################

