## ---- prepare-data-modelling

post_imputed$co_creation_method <- 
  ifelse(post_imputed$version == "Version 1", 0, 1)

post_imputed <- post_imputed[, c("total_log_in", 
                                 "progress_score", 
                                 "action_plan", 
                                 "average_time_spent", 
                                 "days_between", 
                                 "hapa2_t1", 
                                 "hapa1_t1", 
                                 "co_creation_method")]

post_imputed <- post_imputed[complete.cases(post_imputed), ]

post_imputed$average_time_spent.numeric <- as.numeric(post_imputed$average_time_spent)

post_imputed$days_between.numeric <- as.numeric(post_imputed$days_between)

## ---- model-intercept

model_1 <- lm(cbind(total_log_in, 
                    progress_score, 
                    action_plan, 
                    average_time_spent, 
                    days_between) ~ 
                1, 
              data = post_imputed)

## ---- model-risk

model_2 <- lm(cbind(total_log_in, 
                    progress_score, 
                    action_plan, 
                    average_time_spent, 
                    days_between) ~ 
                hapa2_t1, 
              data = post_imputed)

anova(model_1, model_2)

## ---- model-usefulness-risk

model_3 <- update(model_2, . ~ . + hapa1_t1)

anova(model_2, model_3)

## ---- model-co-creation-usefulness-risk

model_4 <- update(model_3, . ~ . + co_creation_method)

anova(model_3, model_4)

Anova(model_4)

model_summary <- summary(model_4)

## ---- individual-models

lm_total_log_in <- lm(total_log_in ~ hapa2_t1 + hapa1_t1 + co_creation_method, 
           data = post_imputed)

lm_progress_score <- lm(progress_score ~ hapa2_t1 + hapa1_t1 + co_creation_method, 
                      data = post_imputed)

lm_action_plan <- lm(action_plan ~ hapa2_t1 + hapa1_t1 + co_creation_method, 
                      data = post_imputed)

lm_average_time_spent <- lm(average_time_spent.numeric ~ hapa2_t1 + hapa1_t1 + co_creation_method, 
                     data = post_imputed)

lm_days_between <- lm(days_between.numeric ~ hapa2_t1 + hapa1_t1 + co_creation_method, 
                            data = post_imputed)

eta_total_log_in <- eta_squared(lm_total_log_in)

eta_progress_score <- eta_squared(lm_progress_score)

eta_action_plan <- eta_squared(lm_action_plan)

eta_average_time_spent <- eta_squared(lm_average_time_spent)

eta_days_between <- eta_squared(lm_days_between)
