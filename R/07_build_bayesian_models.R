## ---- bayes-model-input

data_imputed_long <- do.call(rbind, data_imputed_output)

## ---- bayes-model-joint

# formulate-priors-intervention-effect

priors_progress <- 
  set_prior(sprintf("normal(%f, %f)", prior_progress_pe, prior_progress_sd),
            class = "b", coef = "co_creation_method1")

priors_items <- 
  set_prior(sprintf("normal(%f, %f)", prior_items_pe, prior_items_sd),
            class = "b", coef = "co_creation_method1")

priors_action_plan <- 
  set_prior(sprintf("normal(%f, %f)", prior_action_plan_pe, prior_action_plan_sd),
            class = "b", coef = "co_creation_method1")

priors_time_spent <- 
  set_prior(sprintf("normal(%f, %f)", prior_time_spent_pe, prior_time_spent_sd),
            class = "b", coef = "co_creation_method1")

priors_comms <- 
  set_prior(sprintf("normal(%f, %f)", prior_comms_pe, prior_comms_sd),
            class = "b", coef = "co_creation_method1")

priors_safe <- 
  set_prior(sprintf("normal(%f, %f)", prior_safety_pe, prior_safety_sd),
            class = "b", coef = "co_creation_method1")


# bayesian-model-specify

bf_freq <- bf(
  total_log_in ~ co_creation_method + in_app_age_grp,
  family = negbinomial()
)

bf_prog <- bf(
  progress_score_scaled ~ co_creation_method + in_app_age_grp,
  family = student()
)

bf_items <- bf(
  proportion_items_completed_scaled ~ co_creation_method + in_app_age_grp,
  family = Beta()
)

bf_act <- bf(
  action_plan ~ co_creation_method + in_app_age_grp,
  family = bernoulli(link = "logit")
)

bf_time_spent <- bf(
  average_time_spent_scaled ~ co_creation_method + in_app_age_grp,
  family = student()
)

bf_days_between <- bf(
  days_between ~ co_creation_method + in_app_age_grp,
  family = negbinomial()
)

bf_comm <- bf(
  comm_mean_post_scaled ~ co_creation_method + comm_mean_pre_scaled + (1 | id),
  family = gaussian()
)

bf_safe <- bf(
  safe_mean_post_scaled ~ co_creation_method + safe_mean_pre_scaled + (1 | id),
  family = gaussian()
)

# bayesian-model-build

model_freq <- brm(
  bf_freq,
  data = data_imputed_long, 
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_freq_summary <- summary(model_freq)

saveRDS(model_freq, file = paste0("output/model_freq_", Sys.Date(), ".rds"))

sink(paste0("output/model_freq_", Sys.Date(), ".txt"))
print(summary(model_freq))
sink()

model_progress <- brm(
  bf_prog,
  data = data_imputed_long, 
  prior = priors_progress,
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_progress_summary <- summary(model_progress)

saveRDS(model_progress, file = paste0("output/model_progress_", Sys.Date(), ".rds"))

sink(paste0("output/model_progress_", Sys.Date(), ".txt"))
print(summary(model_progress))
sink()

model_items <- brm(
  bf_items,
  data = data_imputed_long, 
  prior = priors_items,
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_items_summary <- summary(model_items)

saveRDS(model_items, file = paste0("output/model_items_", Sys.Date(), ".rds"))

sink(paste0("output/model_items_", Sys.Date(), ".txt"))
print(summary(model_items))
sink()

model_act_plan <- brm(
  bf_act,
  data = data_imputed_long, 
  prior = priors_action_plan,
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_act_plan_summary <- summary(model_act_plan)

saveRDS(model_act_plan, file = paste0("output/model_act_plan_", Sys.Date(), ".rds"))

sink(paste0("output/model_act_plan_", Sys.Date(), ".txt"))
print(summary(model_act_plan))
sink()

model_time_spent <- brm(
  bf_time_spent,
  data = data_imputed_long, 
  prior = priors_time_spent,
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_time_spent_summary <- summary(model_time_spent)

saveRDS(model_time_spent, file = paste0("output/model_time_spent_", Sys.Date(), ".rds"))

sink(paste0("output/model_time_spent_", Sys.Date(), ".txt"))
print(summary(model_time_spent))
sink()

model_days_between <- brm(
  bf_days_between,
  data = data_imputed_long, 
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_days_between_summary <- summary(model_days_between)

saveRDS(model_days_between, file = paste0("output/model_days_between_", Sys.Date(), ".rds"))

sink(paste0("output/model_days_between_", Sys.Date(), ".txt"))
print(summary(model_days_between))
sink()

model_comm <- brm(
  bf_comm,
  data = data_imputed_long, 
  prior = priors_comms, 
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_comm_summary <- summary(model_comm)

saveRDS(model_comm, file = paste0("output/model_comm_", Sys.Date(), ".rds"))

sink(paste0("output/model_comm_", Sys.Date(), ".txt"))
print(summary(model_comm))
sink()

model_safe <- brm(
  bf_safe,
  data = data_imputed_long, 
  prior = priors_safe, 
  seed = 555,
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15))

model_safe_summary <- summary(model_safe)

saveRDS(model_safe, file = paste0("output/model_safe_", Sys.Date(), ".rds"))

sink(paste0("output/model_safe_", Sys.Date(), ".txt"))
print(summary(model_safe))
sink()

