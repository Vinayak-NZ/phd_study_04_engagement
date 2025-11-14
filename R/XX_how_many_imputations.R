## ---- confirm-number-impute

imp2 <- 
  miceadds::datalist2mids(data_imputed_output)

fit_lmer <- with(
  imp2,
  lm(safe_mean_scaled ~ co_creation_method * time)
)

pooled <- pool(fit_lmer)
summ   <- summary(pooled)
summ$fmi

how_many_imputations(fit_lmer)
how_many_imputations(fit_lmer, cv = .05)
