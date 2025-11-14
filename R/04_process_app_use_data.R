## ---- impute-data

unique_cases <- 
  distinct(final_data_all_vars, version, id, .keep_all = TRUE)

unique_cases <- 
  as.data.frame(unique_cases)

# variables-impute
vars_impute <- c(
  "hapa1_t1","hapa1_t2","hapa1_t3","hapa1_t4",
  "hapa2_t1","hapa2_t2","hapa2_t3","hapa2_t4",
  "hapa3_t1","hapa3_t2","hapa3_t3","hapa3_t4",
  "hapa4_t1","hapa4_t2","hapa4_t3","hapa4_t4",
  "hapa5_t1","hapa5_t2","hapa5_t3","hapa5_t4",
  "safe1_t1","safe2_t1","safe1_t2","safe2_t2",
  "comm1_t1","comm1_t2","comm1_t3","comm1_t4",
  "comm2_t1","comm2_t2","comm2_t3","comm2_t4",
  "comm3_t1","comm3_t2","comm3_t3","comm3_t4",
  "comm4_t1","comm4_t2","comm4_t3","comm4_t4",
  "comm5_t1","comm5_t2","comm5_t3","comm5_t4",
  "comm6_t1","comm6_t2","comm6_t3","comm6_t4",
  "comm7_t1","comm7_t2","comm7_t3","comm7_t4", 
  "ux", 
  "content", 
  "utility"
)

# Variables NOT to impute
no_impute <- c(
  "id",
  "version", 
  "UserCode", 
  "age", 
  "education", 
  "fam_comp",
  "in_app_age_grp", 
  "in_app_occupation",
  "proportion_pages_viewed", 
  "lessons_viewed", 
  "proportion_items_completed", 
  "action_plan", 
  "item_response", 
  "total_log_in", 
  "average_time_spent", 
  "days_between"
)

# Initialize MICE setup
pred <- make.predictorMatrix(unique_cases)
meth <- make.method(unique_cases)

# Prevent Timestamp from informing others (but allow others to be imputed)
meth[names(meth) %in% no_impute] <- ""    
pred[, colnames(pred) %in% no_impute] <- 0 

# Prevent imputation for given variables
meth[!names(vars_impute) %in% no_impute] <- ""

# Run imputation
data_imputed <- mice(
  unique_cases,
  m = 127,            
  maxit = 30,       
  seed = 555,
  predictorMatrix = pred,
  method = meth
)

saveRDS(data_imputed, file = "output/imputed_data_2025_11_14.rds")
