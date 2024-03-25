
model_bayes <- stan_glm(progress_score ~ 
                          hapa2_t1 + 
                          hapa1_t1 + 
                          co_creation_method, 
                        data = post_imputed,
                        seed = 111)

print(model_bayes, digits = 3)

mcmc_dens(model_bayes, pars = c("co_creation_method")) + 
  vline_at(7.583, col = "red")

describe_posterior(model_bayes)

model <- total_log_in | progress_score ~ hapa2_t1 + hapa1_t1 + co_creation_method

bform1 <-
  bf(mvbind(total_log_in, progress_score) ~ hapa2_t1)


fit1 <- brm(bform1, data = post_imputed, chains = 2, cores = 2)

remove.packages(c("rstan", "StanHeaders"))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

Sys.setenv(BINPREF = "C:/RBuildTools/4.2/x86_64-w64-mingw32.static.posix/bin/")

example(stan_model, package = "rstan", run.dontrun = TRUE)
