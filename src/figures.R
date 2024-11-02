#=========================================================================
# Purpose: Generate Figures and Tables for the Manuscript
# Author: Jacob Jameson 
#=========================================================================

##########################################################################
#=========================================================================
# S2A: Balance Table (Wald Test) across Physicians                       #
#=========================================================================
##########################################################################

patient_data <- data

create_dummy_variables <- function(data, categorical_var) {
  dummy_encoder <- dummyVars(paste("~", categorical_var), data = data)
  encoded_data <- data.frame(predict(dummy_encoder, newdata = data))
  drop_col <- paste0(categorical_var, "DROP")
  
  list(
    encoded_data = encoded_data,
    relevant_vars = setdiff(names(encoded_data), drop_col)
  )
}

create_esi_dummies <- function(data) {
  esi_vars <- paste0("ESI", 1:5)
  for (i in 1:5) {
    data[[esi_vars[i]]] <- as.integer(data$ESI == i)
  }
  list(data = data, new_vars = esi_vars)
}

# Performs Wald test for balance checking across physicians
# Returns test results and p-value for multiple comparison adjustment
perform_wald_test <- function(data, var) {
  baseline_model <- lm(as.formula(paste(var, "~ 1")), data)
  extended_model <- lm(as.formula(paste(var, "~ ED_PROVIDER")), data)
  
  wald_result <- waldtest(
    baseline_model,
    extended_model,
    vcov = vcovHC(extended_model, type = "HC1")
  )
  
  result_row <- data.frame(wald_result)[2, ]
  result_row$dummy_var <- var
  
  list(result = result_row, p_value = result_row$"Pr..F.")
}

generate_balance_table <- function(data, 
                                   output_file,
                                   clinical_vars = c("tachycardic", "tachypneic", 
                                                     "febrile", "hypotensive")) {
  # Generate dummy variables and combine relevant variables
  complaint_encoding <- create_dummy_variables(data, "CHIEF_COMPLAINT")
  data <- cbind(data, complaint_encoding$encoded_data)
  
  esi_result <- create_esi_dummies(data)
  data <- esi_result$data
  
  relevant_vars <- c(complaint_encoding$relevant_vars,
                     esi_result$new_vars,
                     clinical_vars)
  
  # Perform Wald tests and collect results
  balance_stats <- data.frame(Df = numeric(), F = numeric(),
                              Pr_F = numeric(), dummy_var = character())
  p_values <- c()
  
  for (var in relevant_vars) {
    test_results <- perform_wald_test(data, var)
    balance_stats <- rbind(balance_stats, test_results$result)
    p_values <- c(p_values, test_results$p_value)
  }
  
  balance_stats$adj_p_value <- p.adjust(p_values, method = "bonferroni")
  balance_stats <- balance_stats[, c("dummy_var", "Df", "F", "Pr..F.", "adj_p_value")]
  rownames(balance_stats) <- NULL
  
  # Generate output
  sink(output_file)
  
  data %>%
    select(all_of(relevant_vars)) %>%
    summarise(across(everything(), list(mean = mean, sum = sum))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    print(n = 100)
  
  print(balance_stats)
  sink()
  
  invisible(list(balance_stats = balance_stats))
}

results <- generate_balance_table(
  data = patient_data,
  output_file = "outputs/tables/S2A.txt"
)

##########################################################################
#=========================================================================
# Figure 1: Variation in Physician Batch Rate by Chief Complaint         #
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
    panel.grid.major = element_line(color = 'grey20', size = 0.3),
    panel.grid.minor = element_blank(),  
    legend.position = 'none',
    axis.title = element_text(size = 20, color = 'black'),  
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 20, face = "bold", color = 'black') ,
    axis.line = element_line(colour = "black")
  ) + 
  labs(
    title = "",
    y = "\nPhysician Batch Rates", x = "Chief Complaint"
  ) +
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(-0.01, .5)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1)))


ggsave("outputs/figures/Figure 1.pdf", 
       width = 15, height = 13, dpi = 300, bg = 'white')


##########################################################################
#=========================================================================
# S2A: Relationship between Batch Tendency and Batching                  #
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
           size = 5.75, color = "black") +
  annotate("segment", x = -1, xend = -2, y = 0.035, yend = 0.012, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 0, xend = 1.95, y = 0.055, yend = 0.053, linetype = "dashed",
           size = 0.5, color = "black", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_curve(aes(x = 0.9, y = 0.0125, xend = 0.0, yend = 0.025), 
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
       title = '')

ggsave("outputs/figures/S2A.pdf", width = 10.5, height = 8, bg = 'white')

##########################################################################
#=========================================================================
# S3A: Density plots of LOS                                              #
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

ggsave("outputs/figures/S3A.pdf", width = 11.5, height = 7, bg = 'white')

################################################################################
#===============================================================================
# Table 1: Relationship between Batch Tendency and LOS                        #
#===============================================================================
################################################################################

# Helper function to run regression models with different specifications
run_regression_models <- function(dependent_var, data, 
                                  base_formula = "~ batch.tendency",
                                  additional_controls = list(
                                    c("patients_in_hospital"),
                                    c("patients_in_hospital", "hypotensive", "tachycardic", 
                                      "tachypneic", "febrile"))) {
  
  # Base fixed effects and clustering
  base_fe <- "dayofweekt + month_of_year"
  cluster <- "ED_PROVIDER"
  
  # Function to construct formula
  build_formula <- function(dep_var, predictors, fe) {
    as.formula(paste(dep_var, "~", 
                     paste(predictors, collapse = " + "), "|",
                     fe, "|0|", cluster))
  }
  
  # Run models
  models <- list()
  
  # Model a (base)
  models[[1]] <- felm(build_formula(dependent_var, "batch.tendency", base_fe), 
                      data = data)
  
  # Model b (+ basic controls)
  predictors_b <- c("batch.tendency", additional_controls[[1]], 
                    "complaint_esi", "LAB_PERF")
  models[[2]] <- felm(build_formula(dependent_var, paste(predictors_b, collapse = " + "), 
                                    base_fe), data = data)
  
  # Model c (+ clinical controls)
  predictors_c <- c(predictors_b, additional_controls[[2]])
  models[[3]] <- felm(build_formula(dependent_var, paste(predictors_c, collapse = " + "), 
                                    base_fe), data = data)
  
  return(models)
}

# Function to generate regression tables
generate_tables <- function(models, outcome_name, output_file, 
                            omit_vars = "patients_in_hospital") {
  # Screen output
  screenreg(models,
            custom.model.names = rep(outcome_name, 3),
            ci.force = TRUE,
            ci.force.level = 0.95,
            omit.coef = omit_vars,
            digits = 3,
            single.row = TRUE,
            include.rsquared = TRUE,
            include.nobs = TRUE)
  
  # Stargazer output
  stargazer(models,
            type = 'text',
            ci.level = 0.95,
            single.row = TRUE,
            omit = omit_vars,
            digits = 3)
}

# Main analysis
main_analysis <- function(data, output_file) {
  sink(output_file)
  
  # 1. LOS Analysis
  los_models <- run_regression_models("ln_ED_LOS", data)
  generate_tables(los_models, "Log ED LOS", output_file)
  
  # 2. Return Analysis
  # Note: Additional control for disposition
  rtn_models <- run_regression_models("RTN_72_HR_ADMIT", data,
                                      additional_controls = list(
                                        c("patients_in_hospital"),
                                        c("patients_in_hospital", "hypotensive", 
                                          "tachycardic", "tachypneic", "febrile", "dispo")))
  generate_tables(rtn_models, "RTN 72HR ADMIT", output_file)
  
  # 3. Imaging Analysis
  img_models <- run_regression_models("imgTests", data)
  generate_tables(img_models, "N Images", output_file)
  
  sink()
}

# Run the analysis
main_analysis(data, "outputs/tables/Table 1.txt")

################################################################################
#===============================================================================
# Figure 2: Forest plot                                                        #
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
    linetype = "CI Type"
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

ggsave("outputs/figures/Figure 2.pdf", width = 16, height = 15.5, dpi = 300, bg = 'white')
