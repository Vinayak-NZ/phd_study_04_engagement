## ---- subset-active-data-app-feedback

# Version 1
v1_feedback_variable_list <- 
  c("id", 
    "^L10Nuterfreundlichkeit", 
    "^L10Inhalt", 
    "^L10Nutzen")

app_v1_active_data_subset_feedback_var <- 
  app_v1_active_data[, grep(paste(v1_feedback_variable_list, collapse="|"), 
                            names(app_v1_active_data), value = TRUE)]

names(app_v1_active_data_subset_feedback_var)[2:ncol(app_v1_active_data_subset_feedback_var)] <- 
  tolower(names(app_v1_active_data_subset_feedback_var)[2:ncol(app_v1_active_data_subset_feedback_var)])

# Version 2
v2_feedback_variable_list <- 
  c("id", 
    "^L10Nuterfreundlichkeit", 
    "^L10Inhalt", 
    "^L10Nutzen")

app_v2_active_data_subset_feedback_var <- 
  app_v2_active_data[, grep(paste(v2_feedback_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_data_subset_feedback_var)[2:ncol(app_v2_active_data_subset_feedback_var)] <- 
  tolower(names(app_v2_active_data_subset_feedback_var)[2:ncol(app_v2_active_data_subset_feedback_var)])

## ---- rename-feedback-vars

app_v1_active_data_subset_feedback_var <- feedback_rename(app_v1_active_data_subset_feedback_var)

app_v2_active_data_subset_feedback_var <- feedback_rename(app_v2_active_data_subset_feedback_var)

## ---- remove-redundancies-feedback
app_v1_feedback <- remove_redundancy(app_v1_active_data_subset_feedback_var, 
                                            feedback_vars)

app_v2_feedback <- remove_redundancy(app_v2_active_data_subset_feedback_var, 
                                            feedback_vars)
