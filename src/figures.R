#=========================================================================
# Purpose: Generate Figures and Tables for the Manuscript
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

library(tidyverse)
library(caret)
library(sandwich)
library(xtable)
library(lmtest)
library(gtsummary)
library(marginaleffects)
library(scales)  # For percent_format
library(stringr) # For str_wrap
library(lfe)
library(ggeffects)

data <- read_csv('outputs/data/final.csv')

##########################################################################
#=========================================================================
# Table 1: Balance Table (Wald Test) across Physicians                   #
                                                                         #
#Table 1 reports the results of a Wald test, which was conducted to      #
# assess the balance of chief complaints across physicians in our        #
# dataset. We created chief complaint categories before analysis         #
# by grouping similar presenting issues. Vital signs were categorized    #
# as follows: tachycardia (pulse more significant than 100),             #
# tachypnea (respiratory rate greater than 20),                          #
# fever (temperature greater than 38^∘  C), and hypotension              #
# (systolic blood pressure less than 90).  A balanced distribution       #
# implies that complaints and severity are evenly distributed across     #
# physicians, which we expect to be the case due to randomization.       #
# The Wald F-statistic and p-value are reported. Robust standard errors  #
# (type HC1) accounted for potential heteroscedasticity in the data.     #
#=========================================================================
##########################################################################

patient_data <- data

# Identify top 10 chief complaints based on frequency
top_10_chief_complaints <- patient_data %>%
  count(CHIEF_COMPLAINT) %>%
  top_n(10, n) %>%
  pull(CHIEF_COMPLAINT)

# Label chief complaints not in top 10 as "DROP"
patient_data <- patient_data %>%
  mutate(CHIEF_COMPLAINT = if_else(CHIEF_COMPLAINT %in% top_10_chief_complaints,
                                   CHIEF_COMPLAINT, "DROP"))

# One-hot encoding for CHIEF_COMPLAINT
dummy_encoder <- dummyVars(" ~ CHIEF_COMPLAINT", data = patient_data)
one_hot_encoded_data <- data.frame(predict(dummy_encoder, newdata = patient_data))
relevant_vars <- setdiff(names(one_hot_encoded_data), 'CHIEF_COMPLAINTDROP')

# Add the one-hot encoded data to the main dataset
patient_data <- cbind(patient_data, one_hot_encoded_data)

# Create binary flags for ESI values
patient_data <- patient_data %>%
  mutate(ESI_1_or_2 = as.integer(ESI %in% 1:2),
         ESI_3_or_4_or_5 = as.integer(ESI %in% 3:5))

# Update the relevant variables list
relevant_vars <- c(relevant_vars, 'ESI_1_or_2', 'ESI_3_or_4_or_5', 
                   "tachycardic", "tachypneic","febrile","hypotensive")


# get column mean and sum for each variable
patient_data %>%
  summarise(across(relevant_vars, mean, na.rm = TRUE),
            across(relevant_vars, sum, na.rm = TRUE)) %>%
  view()


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

# Save the results to a .txt file
sink("outputs/tables/Table 1.txt")

print(xtable(balance_stats[,c(2,3,4)], 
             caption = "Wald Test Results", 
             digits = c(0,3,3,0)), type = "latex")

sink()


##########################################################################
#=========================================================================
# Figure 1: Variation in Physician Batch Rate by Chief Complaint         #
#                                                                        #
# Figure 1 displays the variation in batch rate by chief complaint       #
# for each physician. The batch rate is the proportion of patients       #
# seen by a physician who had their tests batch ordered.                 #
#=========================================================================

data_for_plot <- data

complaints <- data_for_plot %>%
  count(CHIEF_COMPLAINT) %>%
  top_n(9, n) %>%
  pull(CHIEF_COMPLAINT)


data_for_plot <- data_for_plot %>% 
  filter(CHIEF_COMPLAINT %in% complaints) %>%
  group_by(ED_PROVIDER, CHIEF_COMPLAINT) %>%
  summarize(batch_rate = mean(batched)) %>% 
  ungroup() %>%
  mutate(CHIEF_COMPLAINT = ifelse(
    CHIEF_COMPLAINT == "Falls, Motor Vehicle Crashes, Assaults, and Trauma",
    "Assaults and Trauma",  CHIEF_COMPLAINT)) 


ggplot(data_for_plot, aes(x = CHIEF_COMPLAINT, y = batch_rate, 
                          fill = CHIEF_COMPLAINT)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = CHIEF_COMPLAINT), width = 0.2, alpha = 1, size=3) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal(base_size = 16) +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'none',
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) + 
  labs(
    title = "Variation in Physician Imaging Batch Rates",
    y = "\nPhysician Imaging Batch Rates", x = "Chief Complaint"
  ) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(-0.01, .5)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_segment(aes(x = 3.1, y = 0.3, xend = 2.1, yend = 0.07), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "black", size = 1) +
  geom_segment(aes(x = 3.1, y = 0.3, xend = 2.1, yend = 0.46), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "black", size = 1) +
  annotate("text", x = 3.8, y = 0.3, 
           label = "40.8 PP Difference\nin Physician Batch Rate", 
           size = 6, color = "black", hjust = 0.5, fontface='bold')

ggsave("outputs/figures/Figure 1.png", 
       width = 12, height = 7, dpi = 300, bg = 'white')

##########################################################################
#=========================================================================
# Figure 2: Relationship between Batch Tendency and Batching             #
#=========================================================================
##########################################################################


model <- glm(batched ~ batch.tendency + 
             dayofweekt + month_of_year +
             complaint_esi,
             family = binomial, data = data)

get_predictions <- function(model, predictor) {
  ggpredict(model, terms = c(paste(predictor, "[-2:2, by=.1]")), 
            vcov.type = "HC0", 
            vcov.args = list(cluster = data$ED_PROVIDER)) %>%
    as.data.frame() %>%
    dplyr::select(xvals = x, coef = predicted, 
                  lower = conf.low, upper = conf.high) %>%
    filter(!is.na(xvals))
}

preds <- get_predictions(model, "batch.tendency")

data.p <- data.frame(batch.tendency = preds$xvals, 
                     predicted_prob = preds$coef, 
                     lwr = preds$lower, upr = preds$upper)

data.p %>%
  ggplot(aes(x = batch.tendency, y = predicted_prob)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.8, fill = "#a6cee3") +
  geom_line(aes(y=predicted_prob), color = '#1f78b4',  size=2) + 
  geom_point(color = "#1f78b4", size=3) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.125), 
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0.075, ymax = 0.125, 
           fill = "white", color = "black", size = 0.5) +
  annotate("text", x = -1, y = 0.1, 
           label = str_wrap("Conditional on time, patient complaint, 
                   and severity, a physician with a batch tendency 
                   score of 2 is 4x as likely to batch order at a 
                   given patient encounter compared to a physician 
                   with a batch tendency score of -2",  45),
           size = 5.5, color = "black") +
  annotate("segment", x = -1, xend = -2, y = 0.075, yend = 0.02, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 0, xend = 1.95, y = 0.1, yend = 0.08, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_curve(aes(x = 1, y = 0.0125, xend = 0.0, yend = 0.036), 
             curvature = -0.2, arrow = arrow(type = "closed", length = unit(0.2, "cm")),
             color = "black", size = 0.5) +
  annotate("text", x = 1.55, y = 0.0125, 
           label = str_wrap("Batch Ordering Probability of a Physician with
                            the Average Batch Tendency Score",  30),
           size = 5, color = "black") +
  theme_minimal(base_size = 16) +  
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'none',
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) +
  labs(x = '\nBatch Tendency Score',
       y = 'Predicted Probability of Batch-Ordering\n', 
       title = 'Relationship between Batch Tendency and Batch-Ordering Probability')

ggsave("outputs/figures/Figure 2.png", width = 11.5, height = 7, bg = 'white')


##########################################################################
#=========================================================================
# Figure 3: Density plots of LOS                                         #
#=========================================================================
##########################################################################

data_for_plot <- data.frame(ED_LOS = data$ED_LOS)
data_for_plot$group <- 'ED_LOS'

data_for_plot <- rbind(data_for_plot, 
                       data.frame(ED_LOS = data$ln_ED_LOS, group = 'ln_ED_LOS'))

data_for_plot %>%
  mutate(group = ifelse(
    group == 'ED_LOS', 'ED Length of Stay', 
    'Log Transformed ED Length of Stay')) %>%
  ggplot(aes(x = ED_LOS, y = ..density..)) +  
  geom_histogram(aes(fill = group), color='black', bins = 30, alpha = 0.35) +
  scale_fill_brewer(palette = 'Set1') +  
  scale_color_brewer(palette = 'Set1') +  
  facet_wrap(~group, scales = 'free') +  
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.125), 
                     expand = expansion(mult = c(0, 0.05))) +
  geom_density(aes(color = group), size = 1.5) +
  labs(
    x = '\nED Length of Stay (minutes)', 
    y = 'Probability Density\n', 
    title = 'Distribution of ED Length of Stay Before and After Log Transformation\n'
  ) +
  scale_x_continuous(labels = scales::comma) +  
  scale_y_continuous(labels = scales::percent) +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'none',
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) 

ggsave("outputs/figures/Figure 3.png", width = 11.5, height = 7, bg = 'white')

################################################################################
#===============================================================================
# Figure 4: Relationship between Batch Tendency and LOS                        #
#===============================================================================
################################################################################

# Run the main regression models
model.1a <- felm(ln_ED_LOS ~ batch.tendency | 
                  dayofweekt + month_of_year + complaint_esi |0|
                  ED_PROVIDER, data = data)

model.2a <- felm(RTN_72_HR ~ batch.tendency |
                  dayofweekt + month_of_year + complaint_esi |0|
                  ED_PROVIDER, data = data)

model.3a <- felm(imgTests ~ batch.tendency  | 
                  dayofweekt + month_of_year + complaint_esi +
                  hypotensive + tachycardic + tachypneic + febrile|0|
                  ED_PROVIDER, data = data)


model.1b <- felm(ln_ED_LOS ~ batch.tendency | LAB_PERF +
                  dayofweekt + month_of_year + complaint_esi|0|
                  ED_PROVIDER, data = data)

model.2b <- felm(RTN_72_HR ~ batch.tendency | LAB_PERF +
                  dayofweekt + month_of_year + complaint_esi|0|
                  ED_PROVIDER, data = data)

model.3b <- felm(imgTests ~ batch.tendency  | LAB_PERF +
                  dayofweekt + month_of_year + complaint_esi|0|
                  ED_PROVIDER, data = data)

sink("outputs/tables/Main Results.txt")

stargazer::stargazer(model.1a, model.2a, model.3a, 
                     model.1b, model.2b, model.3b,
                     type = "text", title = "Regression Results", 
                     omit = "ED_PROVIDER", digits = 3)
sink()

################################################################################
#===============================================================================
# Figure 5: Forest plot                                                        #
#===============================================================================
################################################################################

data <- data %>%
  mutate(ESI_cat = case_when(
    ESI == 1 ~ 'ESI \n1 or 2',
    ESI == 2 ~ 'ESI \n1 or 2',
    ESI == 3 ~ 'ESI \n3, 4, or 5',
    ESI == 4 ~ 'ESI \n3, 4, or 5',
    ESI == 5 ~ 'ESI \n3, 4, or 5'))

data$ESI_cat <- as.factor(data$ESI_cat)

# Function to map specific complaints to general categories
map_complaints <- function(complaint) {
  if (complaint %in% c('Abdominal Complaints', 'Gastrointestinal Issues')) {
    return('Gastrointestinal/Abdominal')
  } else if (complaint %in% c('Chest Pain', 'Cardiac Arrhythmias')) {
    return('Cardiac/Chest-Related')
  } else if (complaint %in% c('Shortness of Breath', 'Upper Respiratory Symptoms')) {
    return('Respiratory-Related')
  } else if (complaint %in% c('Neurological Issue', 'Dizziness/Lightheadedness/Syncope')) {
    return('Neurological/Syncope')
  } else if (complaint %in% c('Extremity Complaints', 'Back or Flank Pain', 
                              'Falls, Motor Vehicle Crashes, Assaults, and Trauma')) {
    return('Musculoskeletal/Extremity')
  } else {
    return('General/Other Symptoms')
  }
}

data$GROUPED_COMPLAINT <- sapply(data$CHIEF_COMPLAINT, map_complaints)

complaints <- unique(data$GROUPED_COMPLAINT)

model_for_complaint <- function(complaint, esi, outcome_var) {
  
  formula <- as.formula(
    paste(outcome_var, 
          "~ batch.tendency | LAB_PERF + dayofweekt + month_of_year|0|ED_PROVIDER"))
  
  mod <- felm(formula, data = filter(data, 
                                   GROUPED_COMPLAINT == complaint,
                                   ESI_cat == esi))
  
  coef <- as.numeric(mod$beta[[1]])
  cse <- as.numeric(mod$cse[[1]])
  
  row <- c(coef, cse, complaint, esi, outcome_var)
  
  return(row)
}

results <- data.frame(
  coef = numeric(),
  cse = numeric(),
  complaint = character(),
  esi = character(),
  outcome_var = character()
)

r_number <- 1
for (complaint in complaints) {
  for (esi in c('ESI \n1 or 2', 'ESI \n3, 4, or 5')) {
    row <- model_for_complaint(complaint, esi, 'ln_ED_LOS')
    results[r_number,] <- row
    r_number <- r_number + 1
    row <- model_for_complaint(complaint, esi, 'imgTests')
    results[r_number,] <- row
    r_number <- r_number + 1
    row <- model_for_complaint(complaint, esi, 'RTN_72_HR')
    results[r_number,] <- row
    r_number <- r_number + 1
  }
}

results <- results %>%
  mutate(coef = as.numeric(coef),
         cse = as.numeric(cse),
         CI_lower = coef - 1.96 * cse,
         CI_upper = coef + 1.96 * cse)


results %>%
  filter(complaint != 'General/Other Symptoms') %>%
  mutate(
    outcome_var = case_when(
      outcome_var == 'ln_ED_LOS' ~ 'Log ED Length of Stay',
      outcome_var == 'imgTests' ~ 'Number of Imaging \nTests Ordered',
      outcome_var == 'RTN_72_HR' ~ 'Return to ED \nwithin 72 Hours'
    ),
    esi_group = as.numeric(factor(esi)),
    significance = ifelse(CI_lower > 0 | CI_upper < 0, "p < 0.05", "p > 0.05")) %>%
  ggplot(aes(x = complaint, y = coef, group = esi_group, color = significance)) +
  geom_pointrange(aes(ymin = CI_lower, ymax = CI_upper), 
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E31A1C") +
  facet_grid(esi ~ outcome_var, scales='free') + 
  coord_flip() +
  scale_color_manual(values = c("p < 0.05" = "black",
                                "p > 0.05" = "grey70")) +
  labs(
    x = "Complaint",
    y = "\nCoefficient (Effect Size)",
    color = "",
    title = "Regression Coefficients with Confidence Intervals for \nComplaint-Acuity Subgroup Regressions\n"
  ) +
  theme_minimal(base_size = 18) +  
  theme(
    panel.spacing.x = unit(2, "lines"),
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 18),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    strip.background.x = element_rect(fill = "grey85"),
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    strip.text.y = element_text(size = 18, color = 'black') ,
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(colour = "black")
  ) 

ggsave("outputs/figures/Figure 4.png", width = 14.5, height = 8, dpi = 300, bg = 'white')
