#=========================================================================
# Purpose: Generate Figures and Tables for the Manuscript
# Author: Jacob Jameson 
#=========================================================================

##########################################################################
#=========================================================================
# Table 1: Balance Table (Wald Test) across Physicians                   #
                                                                         #
# Table 1 reports the results of a Wald test, which was conducted to      #
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

complaints <- unique(patient_data$CHIEF_COMPLAINT)

# One-hot encoding for CHIEF_COMPLAINT
dummy_encoder <- dummyVars(" ~ CHIEF_COMPLAINT", data = patient_data)
one_hot_encoded_data <- data.frame(predict(dummy_encoder, newdata = patient_data))
relevant_vars <- setdiff(names(one_hot_encoded_data), 'CHIEF_COMPLAINTDROP')

# Add the one-hot encoded data to the main dataset
patient_data <- cbind(patient_data, one_hot_encoded_data)

patient_data$ESI1 <- ifelse(patient_data$ESI == 1, 1, 0)
patient_data$ESI2 <- ifelse(patient_data$ESI == 2, 1, 0)
patient_data$ESI3 <- ifelse(patient_data$ESI == 3, 1, 0)
patient_data$ESI4 <- ifelse(patient_data$ESI == 4, 1, 0)
patient_data$ESI5 <- ifelse(patient_data$ESI == 5, 1, 0)

# Update the relevant variables list
relevant_vars <- c(relevant_vars,  
                   'ESI1', 
                   'ESI2',
                   'ESI3',
                   'ESI4',
                   'ESI5',
                   "tachycardic", 
                   "tachypneic", 
                   "febrile",
                   "hypotensive")


# get column mean and sum for each variable
patient_data %>%
  select(relevant_vars) %>%
  summarise(across(everything(), list(mean = mean, sum = sum))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") 
  
# Initialize balance dataframe
balance_stats <- data.frame(Df = numeric(), 
                            F = numeric(),
                            Pr_F = numeric(), 
                            dummy_var = character())

p_values <- c()

# Loop through each variable and collect balance statistics
for (dummy_var in relevant_vars) {
  # Fit the baseline and extended models
  baseline_model <- lm(as.formula(paste(dummy_var, '~ 1')), patient_data)
  extended_model <- lm(as.formula(paste(dummy_var, '~ ED_PROVIDER')), patient_data)
  
  # Perform the Wald test
  wald_test_result <- lmtest::waldtest(
    baseline_model, 
    extended_model, 
    vcov = vcovHC(extended_model, type = "HC1")
  )
  
  # Convert the result to a data frame
  temp_result <- data.frame(wald_test_result)[2, ]
  
  # Add the variable name to the results
  temp_result$dummy_var <- dummy_var
  
  # Extract the p-value and collect it
  p_value <- temp_result$`Pr..F.`
  p_values <- c(p_values, p_value)
  
  # Append the result to the balance_stats data frame
  balance_stats <- rbind(balance_stats, temp_result)
}

# Apply the Bonferroni adjustment to the collected p-values
adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

# Add the adjusted p-values to the balance_stats data frame
balance_stats$adj_p_value <- adjusted_p_values

# Optional: Rearrange or select specific columns for clarity
balance_stats <- balance_stats[, c("dummy_var", "Df", "F", "Pr..F.", "adj_p_value")]
row.names(balance_stats) <- NULL

# Save the results to a .txt file
sink("outputs/tables/Table 1.txt")

patient_data %>%
  select(relevant_vars) %>%
  summarise(across(everything(), list(mean = mean, sum = sum))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  print(n=100)


balance_stats
sink()




##########################################################################
#=========================================================================
# Figure 2: Variation in Physician Batch Rate by Chief Complaint         #
#                                                                        #
# Figure 2 illuminates the marked differences among physicians in their
# propensity to batch order imaging tests. The 24 physicians are 
# represented with points, revealing that specific complaint areas 
# have more variance than others regarding differing batch rates 
# among physicians              
#=========================================================================

data_for_plot <- data

data_for_plot <- data_for_plot %>% 
  mutate(CHIEF_COMPLAINT = ifelse(
    CHIEF_COMPLAINT == "Falls, Motor Vehicle Crashes, Assaults, and Trauma",
    "Assaults and Trauma",  CHIEF_COMPLAINT)) %>%
  group_by(ED_PROVIDER, CHIEF_COMPLAINT) %>%
  summarize(batch_rate = mean(batched)) %>% 
  ungroup() 


ggplot(data = data_for_plot, aes(x = reorder(CHIEF_COMPLAINT, -batch_rate), 
                                 y = batch_rate,
                          fill = CHIEF_COMPLAINT)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA, color = "black") +
  geom_jitter(aes(color = CHIEF_COMPLAINT), width = 0.1, alpha = 1, size=3.5) +
  scale_fill_manual(values = gdocs_pal()(24)) +
  scale_color_manual(values = gdocs_pal()(24)) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 20),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'black', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'none',
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 20, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) + 
  labs(
    title = "Physician Variation in Batching by Complaint\n",
    y = "\nPhysician Batch Rates", x = "Chief Complaint"
  ) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(-0.01, .5)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))


ggsave("outputs/figures/Figure 1.png", 
       width = 13, height = 15, dpi = 300, bg = 'white')


##########################################################################
#=========================================================================
# Figure 2: Relationship between Batch Tendency and Batching             #
#=========================================================================
##########################################################################

model <- glm(batched ~ batch.tendency + 
               dayofweekt + month_of_year + complaint_esi + LAB_PERF + 
               hypotensive + tachycardic + tachypneic + febrile,
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
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.075), 
                     expand = expansion(mult = c(0, 0.05))) +
  annotate("rect", xmin = -2, xmax = 0, ymin = 0.035, ymax = 0.075, 
           fill = "white", color = "black", size = 0.5) +
  annotate("text", x = -1, y = 0.055, 
           label = str_wrap("Conditional on time, patient complaint, 
                   and severity, a physician with a batch tendency 
                   score of 2 is 4x as likely to batch order imaging tests at a 
                   given patient encounter compared to a physician 
                   with a batch tendency score of -2",  42),
           size = 5.5, color = "black") +
  annotate("segment", x = -1, xend = -2, y = 0.035, yend = 0.012, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 0, xend = 1.95, y = 0.055, yend = 0.053, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_curve(aes(x = 1, y = 0.0125, xend = 0.0, yend = 0.025), 
             curvature = -0.2, arrow = arrow(type = "closed", length = unit(0.2, "cm")),
             color = "black", size = 0.5) +
  annotate("text", x = 1.55, y = 0.0125, 
           label = str_wrap("Batch Ordering Probability of a Physician with
                            the Average Batch Tendency",  30),
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
    #axis.title.y = element_blank(),
    strip.text.x = element_text(size = 18, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) +
  labs(x = '\nBatch Tendency',
       y = 'Predicted Probability of Batch Ordering \n', 
       title = 'Impact of Physician Batch Tendency \non Batch Ordering Probability')

ggsave("outputs/figures/Figure 2.png", width = 11.5, height = 9, bg = 'white')

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
sink("outputs/tables/Main Results.txt")

library(texreg)

# Run the main regression models -----------------------------------------------
model.1a <- felm(ln_ED_LOS ~ batch.tendency  | 
                   dayofweekt + month_of_year |0|
                   ED_PROVIDER, data = data)

model.1b <- felm(ln_ED_LOS ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF |0|
                   ED_PROVIDER, data = data)

model.1c <- felm(ln_ED_LOS ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF + 
                   hypotensive + tachycardic + tachypneic + febrile |0|
                   ED_PROVIDER, data = data)

screenreg(list(model.1a, model.1b, model.1c), 
          custom.model.names = c('Log ED LOS', 'Log ED LOS', 'Log ED LOS'),
          ci.force = TRUE, ci.force.level = 0.95, 
          omit.coef = 'patients_in_hospital',
          digits = 3, single.row = TRUE, 
          include.rsquared = TRUE,
          include.nobs = T)

stargazer::stargazer(model.1a, model.1b, model.1c, type = 'text', 
                      ci.level = 0.95, single.row = TRUE, 
                      omit = 'patients_in_hospital', 
                      digits = 3)


model.2a <- felm(RTN_72_HR_ADMIT ~ batch.tendency | 
                   dayofweekt + month_of_year |0|
                   ED_PROVIDER, data = data)

model.2b <- felm(RTN_72_HR_ADMIT ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF |0|
                   ED_PROVIDER, data = data)

model.2c <- felm(RTN_72_HR_ADMIT ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF + 
                   hypotensive + tachycardic + tachypneic + febrile + dispo |0|
                   ED_PROVIDER, data = data)

screenreg(list(model.2a, model.2b, model.2c), 
          custom.model.names = c('RTN 72HR ADMIT', 'RTN 72HR ADMIT', 
                                 'RTN 72HR ADMIT'),
          ci.force = TRUE, ci.force.level = 0.95, 
          omit.coef = 'patients_in_hospital',
          digits = 3, single.row = TRUE, 
          include.rsquared = TRUE,
          include.nobs = T)

stargazer::stargazer(model.2a, model.2b, model.2c, type = 'text', 
                     ci.level = 0.95, single.row = TRUE, 
                     omit = 'patients_in_hospital', 
                     digits = 3)


model.3a <- felm(imgTests ~ batch.tendency | 
                   dayofweekt + month_of_year |0|
                   ED_PROVIDER, data = data)

model.3b <- felm(imgTests ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF |0|
                   ED_PROVIDER, data = data)

model.3c <- felm(imgTests ~ batch.tendency + patients_in_hospital | 
                   dayofweekt + month_of_year + complaint_esi + LAB_PERF + 
                   hypotensive + tachycardic + tachypneic + febrile |0|
                   ED_PROVIDER, data = data)


screenreg(list(model.3a, model.3b, model.3c), 
          custom.model.names = c('N Images', 'N Images', 
                                 'N Images'),
          ci.force = TRUE, ci.force.level = 0.95, 
          omit.coef = 'patients_in_hospital',
          digits = 3, single.row = TRUE, 
          include.rsquared = TRUE,
          include.nobs = T)

stargazer::stargazer(model.3a, model.3b, model.3c, type = 'text', 
                     ci.level = 0.95, single.row = TRUE, 
                     omit = 'patients_in_hospital', 
                     digits = 3)

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

complaints <- unique(data$CHIEF_COMPLAINT)

model_for_complaint <- function(complaint, esi, outcome_var) {
  formula <- as.formula(paste(outcome_var, "~ batch.tendency | LAB_PERF + dayofweekt + month_of_year|0|ED_PROVIDER"))
  mod <- felm(formula, data = filter(data, CHIEF_COMPLAINT == complaint, ESI_cat == esi))
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
total_tests <- length(complaints) * 2 * 3  # 2 ESI categories, 3 outcome variables
adjusted_alpha <- 0.05 / total_tests
z_value <- qnorm(1 - adjusted_alpha / 2)  # Calculate z-value for the adjusted alpha

for (complaint in complaints) {
  for (esi in c('ESI \n1 or 2', 'ESI \n3, 4, or 5')) {
    for (outcome in c('ln_ED_LOS', 'imgTests', 'RTN_72_HR_ADMIT')) {
      row <- model_for_complaint(complaint, esi, outcome)
      results[r_number,] <- row
      r_number <- r_number + 1
    }
  }
}

results <- results %>%
  mutate(
    coef = as.numeric(coef),
    cse = as.numeric(cse),
    CI_lower_b = coef - z_value * cse,
    CI_upper_b = coef + z_value * cse,
    CI_lower = coef - 1.96 * cse,
    CI_upper = coef + 1.96 * cse
  )


results %>%
  mutate(
    complaint = ifelse(
      complaint == "Falls, Motor Vehicle Crashes, Assaults, and Trauma",
      "Assaults and Trauma",  complaint),
    outcome_var = case_when(
      outcome_var == 'ln_ED_LOS' ~ 'Log ED Length of Stay',
      outcome_var == 'imgTests' ~ 'Number of Imaging \nTests Ordered',
      outcome_var == 'RTN_72_HR_ADMIT' ~ 'Return with Admission \nwithin 72 Hours'
    ),
    esi_group = as.numeric(factor(esi)),
    significance = ifelse(CI_lower > 0 | CI_upper < 0, "p < 0.05", "p > 0.05")) %>%
  ggplot(aes(x = complaint, y = coef, group = esi_group, color = significance)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, linetype = "Regular 95% CI"), 
                  position = position_dodge(width = 0), size=0.6) +
  geom_errorbar(aes(ymin = CI_lower_b, ymax = CI_upper_b,  linetype = "Bonferroni-adjusted 95% CI"), 
                  position = position_dodge(width = 0.5),size = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_grid(esi~ outcome_var, scales='free') + 
  coord_flip() +
  scale_color_manual(values = c("p < 0.05" = "black",
                               "p > 0.05" = "#74767899")) +
  labs(
    x = "Complaint",
    y = "\nCoefficient (Effect Size)",
    color = "Significance",
    linetype = "CI Type",
    title = "Regression Coefficients with Confidence Intervals for \nComplaint-Acuity Subgroup Regressions\n"
  ) +
  theme_minimal(base_size = 18) +  
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5), # Add border around each panel
    plot.background = element_rect(fill = 'white', color = NA), 
    axis.text = element_text(color = 'black', size = 20),  
    plot.title = element_text(size = 22, face = 'bold', hjust = 0.5, color = 'black'),
    plot.margin = margin(t = 1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
    panel.grid.major = element_line(color = 'grey85', size = 0.3),
    panel.grid.minor = element_blank(),  
    strip.background.x = element_rect(fill = "grey90"),
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 19, face = "bold", color = 'black') ,
    strip.text.y = element_text(size = 19, color = 'black') ,
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(colour = "black")
  ) 

ggsave("outputs/figures/Figure 4.png", width = 16, height = 14, dpi = 300, bg = 'white')
