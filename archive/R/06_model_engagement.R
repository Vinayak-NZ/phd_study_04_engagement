## ---- prepare-data-modelling

post_imputed$co_creation_method <- 
  ifelse(post_imputed$version == "Version 1", 0, 1)

post_imputed <- post_imputed[complete.cases(post_imputed), ]

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

model_summary
