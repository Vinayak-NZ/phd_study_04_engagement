## ---- process-non-imputed-data

data_no_impute <- 
  distinct(final_data_all_vars, version, id, .keep_all = TRUE)

data_no_impute$progress_score <- 
  ((data_no_impute$lessons_viewed/10)*100 + 
     (data_no_impute$proportion_pages_viewed))/2

data_no_impute$comm_mean_pre <- rowMeans(
  data_no_impute[, paste0("comm", 1:7, "_t1")],
  na.rm = TRUE
)

data_no_impute$comm_mean_post <- rowMeans(
  data_no_impute[, paste0("comm", 1:7, "_t4")],
  na.rm = TRUE
)

data_no_impute$safe_mean_pre <- rowMeans(
  data_no_impute[, paste0("safe", 1:2, "_t1")],
  na.rm = TRUE
)

data_no_impute$safe_mean_post <- rowMeans(
  data_no_impute[, paste0("safe", 1:2, "_t2")],
  na.rm = TRUE
)

data_no_impute$in_app_age_grp <- 
  ifelse(data_no_impute$in_app_age_grp == "bis25", 
         1, ifelse(data_no_impute$in_app_age_grp == "bis40", 
                   2, ifelse(data_no_impute$in_app_age_grp == "bis55", 3, NA)))

data_no_impute$in_app_age_grp <- 
  factor(data_no_impute$in_app_age_grp, 
         order = TRUE, 
         levels = c(1, 2, 3))

data_no_impute$ux <- 
  factor(data_no_impute$ux, 
         order = TRUE, 
         levels = c(1, 2, 3, 4))

data_no_impute$content <- 
  factor(data_no_impute$content, 
         order = TRUE, 
         levels = c(1, 2, 3, 4))

data_no_impute$utility <- 
  factor(data_no_impute$utility, 
         order = TRUE, 
         levels = c(1, 2, 3, 4))

data_no_impute$progress_score_scaled <- 
  scale(data_no_impute$progress_score)[,1]

data_no_impute$proportion_items_completed <- 
  data_no_impute$proportion_items_completed/100

data_no_impute$proportion_items_completed_scaled <- 
  (data_no_impute$proportion_items_completed * 
     (nrow(data_no_impute) - 1) + 0.5) / nrow(data_no_impute)

data_no_impute$comm_mean_pre_scaled <- 
  scale(data_no_impute$comm_mean_pre)[,1]

data_no_impute$comm_mean_post_scaled <- 
  scale(data_no_impute$comm_mean_post)[,1]

data_no_impute$safe_mean_pre_scaled <- 
  scale(data_no_impute$safe_mean_pre)[,1]

data_no_impute$safe_mean_post_scaled <- 
  scale(data_no_impute$safe_mean_post)[,1]

data_no_impute$action_plan <- 
  as.factor(data_no_impute$action_plan)

data_no_impute$co_creation_method <- 
  as.factor(ifelse(data_no_impute$version == "Version 1", 0, 1))

data_no_impute$average_time_spent <- 
  as.numeric(data_no_impute$average_time_spent)

data_no_impute$days_between <- 
  as.numeric(data_no_impute$days_between)

data_no_impute$average_time_spent_scaled <- 
  scale(data_no_impute$average_time_spent)[,1]

data_no_impute <- 
  data_no_impute[, c("id",
                     "co_creation_method", 
                     "UserCode", 
                     "age", 
                     "education", 
                     "fam_comp",
                     "in_app_age_grp", 
                     "in_app_occupation",
                     "item_response", 
                     "total_log_in", 
                     "progress_score",
                     "progress_score_scaled", 
                     "average_time_spent_scaled", 
                     "days_between", 
                     "proportion_pages_viewed",
                     "lessons_viewed",
                     "proportion_items_completed",
                     "proportion_items_completed_scaled",
                     "action_plan", 
                     "ux", 
                     "content", 
                     "utility",
                     "hapa1_t1", 
                     "hapa2_t1", 
                     "hapa3_t1", 
                     "hapa4_t1", 
                     "hapa5_t1",
                     "comm_mean_pre",
                     "comm_mean_post",  
                     "safe_mean_pre", 
                     "safe_mean_post",  
                     "comm_mean_pre_scaled",  
                     "comm_mean_post_scaled", 
                     "safe_mean_pre_scaled", 
                     "safe_mean_post_scaled")]

