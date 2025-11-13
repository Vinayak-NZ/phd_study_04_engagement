## ---- bayes-model-input

data_imputed_long <- do.call(rbind, data_imputed_output)

## ---- bayes-model-joint

# formulate-priors-intervention-effect

priors_progress <- 
  set_prior(sprintf("normal(%f, %f)", prior_progress_pe, prior_progress_sd),
            class = "b", coef = "co_creation_method1")

priors_action_plan <- 
  set_prior(sprintf("normal(%f, %f)", prior_action_plan_pe, prior_action_plan_sd),
            class = "b", coef = "co_creation_method1")

priors_time_spent <- 
  set_prior(sprintf("normal(%f, %f)", prior_time_spent_pe, prior_time_spent_sd),
            class = "b", coef = "co_creation_method1")

priors_comms <- 
  set_prior(sprintf("normal(%f, %f)", prior_comms_pe, prior_comms_sd),
            class = "b", coef = "co_creation_method1:time2")

priors_safe <- 
  set_prior(sprintf("normal(%f, %f)", prior_safety_pe, prior_safety_sd),
            class = "b", coef = "co_creation_method1:time2")

priors_outcomes <- c(priors_comms, priors_safe)


# bayesian-model-specify

bf_freq <- bf(
  total_log_in ~ co_creation_method + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1,
  family = negbinomial()
)

bf_prog <- bf(
  progress_score_scaled ~ co_creation_method + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1,
  family = student()
)

bf_act <- bf(
  action_plan ~ co_creation_method + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1,
  family = bernoulli(link = "logit")
)

bf_time_spent <- bf(
  average_time_spent_scaled ~ co_creation_method + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1,
  family = student()
)

bf_days_between <- bf(
  days_between ~ co_creation_method + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1,
  family = negbinomial()
)

bf_comm <- bf(
  comm_mean_scaled ~ co_creation_method * time + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1 + (1 | id),
  family = gaussian()
)

bf_safe <- bf(
  safe_mean_scaled ~ co_creation_method * time + hapa1_t1 + hapa2_t1 + hapa3_t1 + hapa4_t1 + hapa5_t1 + (1 | id),
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




fit_outcomes <- brm(
  bf_comm + bf_safe + set_rescor(FALSE),
  data = data_imputed_long,
  prior = priors_outcomes, 
  seed = 555, 
  chains  = 1, 
  cores   = 1,
  iter    = 500,         
  warmup  = 250,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)




bayesian_model_progress <- brm(progress_score_scaled ~ co_creation_method, 
                           data = data_imputed_long, 
                           family = student(),
                           prior = priors_progress, 
                           seed = 555,
                           chains = 4, 
                           cores = 4, 
                           iter = 4000, 
                           warmup = 500, 
                           backend = "cmdstanr", 
                           control = list(adapt_delta = 0.95, max_treedepth = 15))

bayesian_model_progress_summary <- summary(bayesian_model_progress)

saveRDS(bayesian_model_progress, file = paste0("output/bayesian_model_progress_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_progress_", Sys.Date(), ".txt"))
print(summary(bayesian_model_progress))
sink()


model <- brm(action_plan ~ co_creation_method, 
                               data = data_imputed_long, 
                              family = bernoulli(link = "logit"),
                               prior = priors_action_plan, 
                               seed = 555,
                               chains = 4, 
                               cores = 4, 
                               iter = 4000, 
                               warmup = 500, 
                               backend = "cmdstanr", 
                               control = list(adapt_delta = 0.95, max_treedepth = 15))

model_summary <- summary(model)

saveRDS(bayesian_model_progress, file = paste0("output/bayesian_model_progress_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_progress_", Sys.Date(), ".txt"))
print(summary(bayesian_model_progress))
sink()



model <- brm(comm_mean_scaled ~ co_creation_method * time + age + education + fam_comp + (1 | id), 
             data = data_imputed_long, 
             family = gaussian(),
             prior = priors_comms_o, 
             seed = 555,
             chains = 4, 
             cores = 4, 
             iter = 4000, 
             warmup = 500, 
             backend = "cmdstanr", 
             control = list(adapt_delta = 0.95, max_treedepth = 15))

model_summary <- summary(model)

saveRDS(bayesian_model_progress, file = paste0("output/bayesian_model_progress_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_progress_", Sys.Date(), ".txt"))
print(summary(bayesian_model_progress))
sink()



fit_joint <- brm(
  bf_comm + bf_safe + set_rescor(FALSE),
  data = data_imputed_long,
  seed = 555, 
  chains  = 1, 
  cores   = 1,
  iter    = 500,         
  warmup  = 250,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(fit_joint)


## ---- bayes-model-progress

# formulate-priors-intervention-effect

priors_progress <- c(
  set_prior(sprintf("normal(%f, %f)", sentiment_delta_log, prior_sd),
            class = "b", coef = "co_creation_method1"),
  set_prior("normal(0, 2)", class = "Intercept"))

# bayesian-model-progress

bayesian_model_progress <- brm(progress_score ~ co_creation_method, 
                               data = data_imputed_long, 
                               family = Gamma(link = "log"),
                               prior = priors_progress, 
                               seed = 555,
                               chains = 4, 
                               cores = 4, 
                               iter = 4000, 
                               warmup = 500, 
                               backend = "cmdstanr", 
                               control = list(adapt_delta = 0.95, max_treedepth = 15))

bayesian_model_progress_summary <- summary(bayesian_model_progress)

saveRDS(bayesian_model_progress, file = paste0("output/bayesian_model_progress_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_progress_", Sys.Date(), ".txt"))
print(summary(bayesian_model_progress))
sink()






bf_prog <- bf(
  progress_score ~ co_creation_method + (1 | p | id),
  family = gaussian()
)

bf_act <- bf(
  action_plan ~ co_creation_method + (1 | p | id),
  family = bernoulli(link = "logit")
)

bf_comm <- bf(
  comm_mean ~ co_creation_method * time + (1 | p | id),
  family = gaussian()
)

bf_safe <- bf(
  safe_mean ~ co_creation_method * time + (1 | p | id),
  family = gaussian()
)

get_prior(bf_prog + bf_act + bf_comm + bf_safe, data = data_imputed_long)

priors <- c(
  # --- Informed prior (progress only) ---
  prior(normal(sent_mu, sent_sd),
        class = "b", coef = "co_creation_method1", resp = "progressscore"),
  
  # --- Weakly informative priors for co-creation effects (other models) ---
  prior(normal(0, 0.5), class = "b", coef = "co_creation_method1", resp = "commmean"),
  prior(normal(0, 0.5), class = "b", coef = "co_creation_method1", resp = "safemean"),
  prior(normal(0, 1.5), class = "b", coef = "co_creation_method1", resp = "actionplan"), # logit scale
  
  # --- Interactions and time effects (comm/safe) ---
  prior(normal(0, 0.5), class = "b", coef = "time2", resp = "commmean"),
  prior(normal(0, 0.5), class = "b", coef = "co_creation_method1:time2", resp = "commmean"),
  prior(normal(0, 0.5), class = "b", coef = "time2", resp = "safemean"),
  prior(normal(0, 0.5), class = "b", coef = "co_creation_method1:time2", resp = "safemean"),
  
  # --- Intercepts ---
  prior(normal(0, 1), class = "Intercept", resp = "progressscore"),
  prior(normal(0, 1), class = "Intercept", resp = "commmean"),
  prior(normal(0, 1), class = "Intercept", resp = "safemean"),
  prior(normal(0, 2.5), class = "Intercept", resp = "actionplan"),
  
  # --- Residual SDs (for Gaussian models only) ---
  prior(student_t(3, 0, 2.5), class = "sigma", resp = "progressscore"),
  prior(student_t(3, 0, 2.5), class = "sigma", resp = "commmean"),
  prior(student_t(3, 0, 2.5), class = "sigma", resp = "safemean")
)

fit_joint <- brm(
  bf_prog + bf_act + bf_comm + bf_safe,   # without set_rescor(TRUE)
  data    = data_imputed_long,          # list of multiply imputed datasets
  chains  = 4, cores = 4, iter = 4000, warmup = 1000,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

# Gaussian block with residual correlations enabled
bf_comm <- bf(
  comm_mean ~ co_creation_method * time + (1 | p | id),
  family = gaussian()
)

bf_safe <- bf(
  safe_mean ~ co_creation_method * time + (1 | p | id),
  family = gaussian()
)

fit_gauss <- brm(
  bf_comm + bf_safe + set_rescor(TRUE),
  data = data_imputed_long,
  backend = "cmdstanr",
  chains  = 1,           # only one chain
  iter    = 500,         # very short
  warmup  = 250,
  cores   = 1,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)


summary(fit_gauss)
