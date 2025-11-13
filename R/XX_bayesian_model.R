
app_all_sentiment_input <- setDT(app_all_sentiment)

app_all_sentiment_input <- 
  app_all_sentiment_input[, proportion:=NULL]

app_all_sentiment_input <- dcast(app_all_sentiment_input, 
                           version ~ sentiment, 
                           value.var = "count")


app_all_sentiment_input[["other"]] <- 
  app_all_sentiment_input[["neutral"]] +  app_all_sentiment_input[["negative"]]

app_all_sentiment_input[["delta"]] <- 
  app_all_sentiment_input[["positive"]] / app_all_sentiment_input[["other"]]

sentiment_delta_log <- 
  as.numeric(
    log(app_all_sentiment_input[version == "2.0", "delta"] / 
        app_all_sentiment_input[version == "1.0", "delta"]))

sentiment_se <- 
  as.numeric(sqrt(1/app_all_sentiment_input[version == "1.0", "positive"] + 
  1/app_all_sentiment_input[version == "1.0", "other"] + 
  1/app_all_sentiment_input[version == "2.0", "positive"] + 
  1/app_all_sentiment_input[version == "2.0", "other"]))

# model

library(brms)
library(bayesplot)
library(posterior)

model_input <- post_imputed

model_input$co_creation_method <- 
  as.factor(ifelse(model_input$version == "Version 1", 0, 1))

model_input <- model_input[, c("id", 
                               "total_log_in", 
                               "progress_score", 
                               "action_plan", 
                               "average_time_spent", 
                               "days_between", 
                               "hapa2_t1", 
                               "hapa1_t1", 
                               "co_creation_method")]

model_input <- model_input[complete.cases(model_input), ]

model_input$average_time_spent.numeric <- 
  as.numeric(model_input$average_time_spent)

model_input$days_between.numeric <- 
  as.numeric(model_input$days_between)

a0 <- 0.5
base_sd <- 0.5

prior_sd <- sqrt((sentiment_se^2 / a0) + base_sd^2)
prior_sd

priors <- c(
  set_prior(sprintf("normal(%f, %f)", sentiment_delta_log, prior_sd),
            class = "b", coef = "co_creation_method1"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd")
)

fit_gamma <- brm(
  progress_score ~ co_creation_method + (1 | id),
  data   = model_input,
  family = Gamma(link = "log"),
  prior  = priors,
  chains = 4, iter = 4000, cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

summary(fit_gamma)

draws_prog <- as_draws_df(fit_gamma)
beta_progress <- draws_prog$b_co_creation_method1
mean_beta  <- mean(beta_progress)
sd_beta    <- sd(beta_progress)

priors_ap <- c(
  set_prior(sprintf("normal(%f,%f)", mean_beta, sd_beta),
            class = "b", coef = "co_creation_method1"),
  set_prior("normal(0, 2)", class = "Intercept"),
  set_prior("student_t(3, 0, 1)", class = "sd")
)

fit_ap <- brm(
  action_plan ~ co_creation_method + (1 | id),
  data   = model_input,
  family = bernoulli(),
  prior  = priors_ap,
  chains = 4, iter = 4000, cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

summary(fit_ap)
