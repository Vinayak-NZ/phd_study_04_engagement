## ---- output-end-user-profiles

(summary(data_no_impute[data_no_impute$co_creation_method == 0, ]$in_app_age_grp)/
  nrow(data_no_impute[data_no_impute$co_creation_method == 0, ]))*100

(summary(data_no_impute[data_no_impute$co_creation_method == 1, ]$in_app_age_grp)/
    nrow(data_no_impute[data_no_impute$co_creation_method == 1, ]))*100

hapa_output_v1 <- list()

hapa_output_v2 <- list()
  
  for (i in 1:5){
    
    hapa_output_v1[[i]] <- 
      c(paste0("HAPA", " ", i), 
        mean(data_no_impute[
        data_no_impute$co_creation_method == 0][[paste0("hapa", i, "_t1")]], 
        na.rm = TRUE), 
        sd(data_no_impute[
          data_no_impute$co_creation_method == 0][[paste0("hapa", i, "_t1")]], 
          na.rm = TRUE), 
        (sum(is.na(data_no_impute[
          data_no_impute$co_creation_method == 0][[paste0("hapa", i, "_t1")]]))/
           nrow(data_no_impute[
             data_no_impute$co_creation_method == 0]))*100)
    
    hapa_output_v2[[i]] <- 
      c(paste0("HAPA", " ", i), 
        mean(data_no_impute[
          data_no_impute$co_creation_method == 1][[paste0("hapa", i, "_t1")]], 
          na.rm = TRUE), 
        sd(data_no_impute[
          data_no_impute$co_creation_method == 1][[paste0("hapa", i, "_t1")]], 
          na.rm = TRUE), 
        (sum(is.na(data_no_impute[
          data_no_impute$co_creation_method == 1][[paste0("hapa", i, "_t1")]]))/
          nrow(data_no_impute[
            data_no_impute$co_creation_method == 1]))*100)
        
  }

comm_output <- list()

safety_output <- list()

for (i in 0:1){
  
  comm_output[[i+1]] <- 
    c(paste0("Version", " ", i+1, " ","comm_pre"), 
      mean(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_pre")]], 
        na.rm = TRUE), 
      sd(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_pre")]], 
        na.rm = TRUE), 
      (sum(is.na(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_pre")]]))/
         nrow(data_no_impute[
           data_no_impute$co_creation_method == i]))*100, 
      paste0("Version", " ", i+1, " ", "comm_post"), 
      mean(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]], 
        na.rm = TRUE), 
      sd(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]], 
        na.rm = TRUE), 
      (sum(is.na(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]]))/
         nrow(data_no_impute[
           data_no_impute$co_creation_method == i]))*100)
  
  safety_output[[i+1]] <- 
    c(paste0("Version", " ", i+1, " ", "safe_pre"), 
      mean(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("safe_mean_pre")]], 
        na.rm = TRUE), 
      sd(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("safe_mean_pre")]], 
        na.rm = TRUE), 
      (sum(is.na(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("safe_mean_pre")]]))/
         nrow(data_no_impute[
           data_no_impute$co_creation_method == i]))*100, 
      paste0("Version", " ", i+1, " ", "safe_post"), 
      mean(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]], 
        na.rm = TRUE), 
      sd(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]], 
        na.rm = TRUE), 
      (sum(is.na(data_no_impute[
        data_no_impute$co_creation_method == i][[paste0("comm_mean_post")]]))/
         nrow(data_no_impute[
           data_no_impute$co_creation_method == i]))*100)
  
}
