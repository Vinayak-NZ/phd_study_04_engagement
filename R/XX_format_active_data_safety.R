## ---- subset-active-data-safety

# Version 1
v1_safety_variable_list <- 
  c("id", 
    "^VUEIC1_v1", 
    "^VUEIC2_v1")

app_v1_active_subset_safe <- 
  app_v1_active_data[, grep(paste(v1_safety_variable_list, collapse="|"), 
                                  names(app_v1_active_data), value = TRUE)]

names(app_v1_active_subset_safe)[2:ncol(app_v1_active_subset_safe)] <- 
  tolower(names(app_v1_active_subset_safe)[2:ncol(app_v1_active_subset_safe)])

# Version 2
v2_safety_variable_list <- 
  c("id", 
    "^VUEIC1_v1", 
    "^VUEIC2_v1")

app_v2_active_subset_safe <- 
  app_v2_active_data[, grep(paste(v2_safety_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_subset_safe)[2:ncol(app_v2_active_subset_safe)] <- 
  tolower(names(app_v2_active_subset_safe)[2:ncol(app_v2_active_subset_safe)])

## ---- rename-covariates-safety

app_v1_active_subset_safety <- safety_rename(app_v1_active_subset_safe)

app_v2_active_subset_safety <- safety_rename(app_v2_active_subset_safe)

## ---- remove-redundancies-safety

app_v1_safety_constructs <- remove_redundancy(app_v1_active_subset_safety, 
                                            safey_constructs)

app_v2_safety_constructs <- remove_redundancy(app_v2_active_subset_safety, 
                                              safey_constructs)

## ---- adopt-most-recent-score-safety
safety_con_list <- c("safe1", "safe2")

app_v1_safety_list <- lapply(safety_con_list, 
                           tidy_con, 
                           data = app_v1_safety_constructs, 
                           time = 1)

app_v1_safety_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                                 app_v1_safety_list) 

app_v2_safety_list <- lapply(safety_con_list, 
                           tidy_con,
                           data = app_v2_safety_constructs, 
                           time = 1)

app_v2_safety_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                                app_v2_safety_list) 

## ---- clean-scores
safety_con_list <- c("safe1", "safe2")

app_v1_safety_list_rev <- lapply(safety_con_list, 
                                 score_edit_multiple, 
                               data = app_v1_safety_modified, 
                               time = 1)

app_v1_safety_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v1_safety_list_rev) 

app_v2_safety_list_rev <- lapply(safety_con_list, 
                                 score_edit_multiple, 
                               data = app_v2_safety_modified, 
                               time = 1)

app_v2_safety_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v2_safety_list_rev) 
