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
    (data_imputed_output[[i]]$lessons_viewed*10)*0.5 + 
    (data_imputed_output[[i]]$proportion_pages_viewed*0.3) + 
    (data_imputed_output[[i]]$proportion_items_completed*0.2)
  
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
  
  setDT(data_imputed_output[[i]])
  
  data_imputed_output[[i]] <- melt(data_imputed_output[[i]], 
                                 id.vars = c("id",
                                              "version", 
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
                                              "days_between", 
                                              "proportion_pages_viewed",
                                              "lessons_viewed",
                                              "proportion_items_completed",
                                              "action_plan", 
                                              "hapa1_t1", 
                                              "hapa2_t1", 
                                              "hapa3_t1", 
                                              "hapa4_t1", 
                                              "hapa5_t1",
                                               "ux", 
                                               "content", 
                                               "utility"), 
                                   measure.vars = list(c("comm_mean_pre", "comm_mean_post"), 
                                                       c("safe_mean_pre", "safe_mean_post")),
                                   variable.name = "time", 
                                   value.name = c("comm_mean", 
                                                  "safe_mean"))
  
  data_imputed_output[[i]]$time <- 
    factor(data_imputed_output[[i]]$time, 
           order = FALSE, 
           levels = c(1, 2))
  
  data_imputed_output[[i]]$progress_score_scaled <- 
    scale(data_imputed_output[[i]]$progress_score)[,1]
  
  data_imputed_output[[i]]$action_plan <- 
    as.factor(data_imputed_output[[i]]$action_plan)
  
  data_imputed_output[[i]]$comm_mean_scaled <- 
    scale(data_imputed_output[[i]]$comm_mean)[,1]
  
  data_imputed_output[[i]]$safe_mean_scaled <- 
    scale(data_imputed_output[[i]]$safe_mean)[,1]
  
  data_imputed_output[[i]]$co_creation_method <- 
    as.factor(ifelse(data_imputed_output[[i]]$version == "Version 1", 0, 1))
  
  data_imputed_output[[i]]$average_time_spent <- 
    as.numeric(data_imputed_output[[i]]$average_time_spent)
  
  data_imputed_output[[i]]$days_between <- 
    as.numeric(data_imputed_output[[i]]$days_between)
  
  data_imputed_output[[i]]$average_time_spent_scaled <- 
    scale(data_imputed_output[[i]]$average_time_spent)[,1]
  
  data_imputed_output[[i]] <- as.data.frame(data_imputed_output[[i]])
  
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
                                 "progress_score_scaled", 
                                 "average_time_spent_scaled", 
                                 "days_between", 
                                 "proportion_pages_viewed",
                                 "lessons_viewed",
                                 "proportion_items_completed",
                                 "action_plan", 
                                 "ux", 
                                 "content", 
                                 "utility",
                                 "time", 
                                 "hapa1_t1", 
                                 "hapa2_t1", 
                                 "hapa3_t1", 
                                 "hapa4_t1", 
                                 "hapa5_t1",
                                 "safe_mean",  
                                 "comm_mean",
                                 "safe_mean_scaled",  
                                 "comm_mean_scaled")]
  
}
