## ---- output-bayesian-model-stats

# ---- output-descriptives

mean_v1_vars <- with(data_imputed_long[data_imputed_long$co_creation_method==0, ], 
                expr = c("mean_progress" = mean(progress_score), 
                         "mean_average_time_spent" = mean(average_time_spent), 
                         "mean_comm_mean_post" = mean(comm_mean_post), 
                         "mean_safe_mean_post" = mean(safe_mean_post)))

sd_vars <- with(data_imputed_long, 
                expr = c("sd_progress" = stats::sd(progress_score), 
                         "sd_average_time_spent" = stats::sd(average_time_spent), 
                         "sd_comm_mean_post" = stats::sd(comm_mean_post), 
                         "sd_safe_mean_post" = stats::sd(safe_mean_post)))

# ---- model-freq

# model-estimates
brm_model_freq_output <- 
  posterior_summary(model_freq, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_freq_output

# statistical-significance-estimates
brm_model_freq_pd_values <- p_direction(model_freq)

print(brm_model_freq_pd_values)

# practical-significance-estimates
model_freq_rope_values <- p_rope(model_freq)

print(model_freq_rope_values)

# explanatory-power
bayes_R2(model_freq, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_freq)

# ---- model-progress

# model-estimates
brm_model_progress_output <- 
  posterior_summary(model_progress, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_progress_output

# statistical-significance-estimates
brm_model_progress_pd_values <- p_direction(model_progress)

print(brm_model_progress_pd_values)

# practical-significance-estimates
model_progress_rope_values <- p_rope(model_progress)

print(model_progress_rope_values)

# explanatory-power
bayes_R2(model_progress, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_progress)

(withPool_MI(sd_vars["sd_progress"])*
    summary(model_progress)$fixed$Estimate[2])/
  withPool_MI(mean_v1_vars["mean_progress"])

# ---- model-items-completed

# model-estimates
brm_model_items_output <- 
  posterior_summary(model_items, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_items_output

# statistical-significance-estimates
brm_model_items_pd_values <- p_direction(model_items)

print(brm_model_items_pd_values)

# practical-significance-estimates
model_items_rope_values <- p_rope(model_items)

print(model_items_rope_values)

# explanatory-power
bayes_R2(model_items, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_items)

# ---- model-action-plan

# model-estimates
brm_model_act_plan_output <- 
  posterior_summary(model_act_plan, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_act_plan_output

# statistical-significance-estimates
brm_model_act_plan_pd_values <- p_direction(model_act_plan)

print(brm_model_act_plan_pd_values)

# practical-significance-estimates
model_act_plan_rope_values <- p_rope(model_act_plan)

print(model_act_plan_rope_values)

# explanatory-power
bayes_R2(model_act_plan, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_act_plan)

# ---- model-time-spent

# model-estimates
brm_model_time_spent_output <- 
  posterior_summary(model_time_spent, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_time_spent_output

# statistical-significance-estimates
brm_model_time_spent_pd_values <- p_direction(model_time_spent)

print(brm_model_time_spent_pd_values)

# practical-significance-estimates
model_time_spent_rope_values <- p_rope(model_time_spent)

print(model_time_spent_rope_values)

# explanatory-power
bayes_R2(model_time_spent, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_time_spent)

(withPool_MI(sd_vars["sd_average_time_spent"])*
    summary(model_time_spent)$fixed$Estimate[2])/
  withPool_MI(mean_v1_vars["mean_average_time_spent"])

# ---- model-days-between

# model-estimates
brm_model_days_between_output <- 
  posterior_summary(model_days_between, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_in_app_age_grp.L", 
                                 "b_in_app_age_grp.Q"))

brm_model_days_between_output

# statistical-significance-estimates
brm_model_days_between_pd_values <- p_direction(model_days_between)

print(brm_model_days_between_pd_values)

# practical-significance-estimates
model_days_between_rope_values <- p_rope(model_days_between)

print(model_days_between_rope_values)

# explanatory-power
bayes_R2(model_days_between, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_days_between)

# ---- model-outcome-comms

# model-estimates
brm_model_comm_output <- 
  posterior_summary(model_comm, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_comm_mean_pre_scaled"))

brm_model_comm_output

# statistical-significance-estimates
brm_model_comm_pd_values <- p_direction(model_comm)

print(brm_model_comm_pd_values)

# practical-significance-estimates
model_comm_rope_values <- p_rope(model_comm)

print(model_comm_rope_values)

# explanatory-power
bayes_R2(model_comm, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_comm)

(withPool_MI(sd_vars["sd_comm_mean_post"])*
    summary(model_comm)$fixed$Estimate[2])/
  withPool_MI(mean_v1_vars["mean_comm_mean_post"])

# ---- model-outcome-safe

# model-estimates
brm_model_safe_output <- 
  posterior_summary(model_safe, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_co_creation_method1", 
                                 "b_safe_mean_pre_scaled"))

brm_model_safe_output

# statistical-significance-estimates
brm_model_safe_pd_values <- p_direction(model_safe)

print(brm_model_safe_pd_values)

# practical-significance-estimates
model_safe_rope_values <- p_rope(model_safe)

print(model_safe_rope_values)

# explanatory-power
bayes_R2(model_safe, summary = TRUE, ndraws = 1000)

# general-summary
summary(model_safe)

(withPool_MI(sd_vars["sd_safe_mean_post"])*
    summary(model_safe)$fixed$Estimate[2])/
  withPool_MI(mean_v1_vars["mean_safe_mean_post"])  

## ---- output-table


