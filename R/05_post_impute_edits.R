## ---- prepare-mice-output

data_imputed_output <- list()

for (i in 1:data_imputed$m){
  
  data_imputed_output[[i]] <- complete(data_imputed, i)
  
}

## ---- post-imputation-edit

for (i in 1:length(data_imputed_output)){
  
  data_imputed_output[[i]] <- 
    distinct(data_imputed_output[[i]], version, id, .keep_all = TRUE)
  
  data_imputed_output[[i]]$progress_score <- 
    ((data_imputed_output[[i]]$lessons_viewed/10)*100 + 
    (data_imputed_output[[i]]$proportion_pages_viewed))/2
  
  data_imputed_output[[i]]$comm_mean_pre <- rowMeans(
    data_imputed_output[[i]][, paste0("comm", 1:7, "_t1")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$comm_mean_post <- rowMeans(
    data_imputed_output[[i]][, paste0("comm", 1:7, "_t4")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$safe_mean_pre <- rowMeans(
    data_imputed_output[[i]][, paste0("safe", 1:2, "_t1")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$safe_mean_post <- rowMeans(
    data_imputed_output[[i]][, paste0("safe", 1:2, "_t2")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$in_app_age_grp <- 
    ifelse(data_imputed_output[[i]]$in_app_age_grp == "bis25", 
           1, ifelse(data_imputed_output[[i]]$in_app_age_grp == "bis40", 
                     2, ifelse(data_imputed_output[[i]]$in_app_age_grp == "bis55", 3, NA)))
  
  data_imputed_output[[i]]$in_app_age_grp <- 
    factor(data_imputed_output[[i]]$in_app_age_grp, 
           order = TRUE, 
           levels = c(1, 2, 3))
  
  data_imputed_output[[i]]$ux <- 
    factor(data_imputed_output[[i]]$ux, 
           order = TRUE, 
           levels = c(1, 2, 3, 4))
  
  data_imputed_output[[i]]$content <- 
    factor(data_imputed_output[[i]]$content, 
           order = TRUE, 
           levels = c(1, 2, 3, 4))
  
  data_imputed_output[[i]]$utility <- 
    factor(data_imputed_output[[i]]$utility, 
           order = TRUE, 
           levels = c(1, 2, 3, 4))
  
  data_imputed_output[[i]]$progress_score_scaled <- 
    scale(data_imputed_output[[i]]$progress_score)[,1]
  
  data_imputed_output[[i]]$proportion_items_completed <- 
    data_imputed_output[[i]]$proportion_items_completed/100
  
  data_imputed_output[[i]]$proportion_items_completed_scaled <- 
    (data_imputed_output[[i]]$proportion_items_completed * 
       (nrow(data_imputed_output[[i]]) - 1) + 0.5) / nrow(data_imputed_output[[i]])
  
  data_imputed_output[[i]]$comm_mean_pre_scaled <- 
    scale(data_imputed_output[[i]]$comm_mean_pre)[,1]
  
  data_imputed_output[[i]]$comm_mean_post_scaled <- 
    scale(data_imputed_output[[i]]$comm_mean_post)[,1]
  
  data_imputed_output[[i]]$safe_mean_pre_scaled <- 
    scale(data_imputed_output[[i]]$safe_mean_pre)[,1]
  
  data_imputed_output[[i]]$safe_mean_post_scaled <- 
    scale(data_imputed_output[[i]]$safe_mean_post)[,1]
  
  data_imputed_output[[i]]$action_plan <- 
    as.factor(data_imputed_output[[i]]$action_plan)
  
  data_imputed_output[[i]]$co_creation_method <- 
    as.factor(ifelse(data_imputed_output[[i]]$version == "Version 1", 0, 1))
  
  data_imputed_output[[i]]$average_time_spent <- 
    as.numeric(data_imputed_output[[i]]$average_time_spent)
  
  data_imputed_output[[i]]$days_between <- 
    as.numeric(data_imputed_output[[i]]$days_between)
  
  data_imputed_output[[i]]$average_time_spent_scaled <- 
    scale(data_imputed_output[[i]]$average_time_spent)[,1]
  
  data_imputed_output[[i]]$.id <- data_imputed_output[[i]]$id
  
  data_imputed_output[[i]]$.imp <- i 
  
  data_imputed_output[[i]] <- 
    data_imputed_output[[i]][, c(".imp", 
                                 ".id", 
                                 "id",
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
                                 "average_time_spent",
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
  
}
