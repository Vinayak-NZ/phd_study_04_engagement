## ---- rename-app-id-active

names(app_v1_active_data)[names(app_v1_active_data) == "UUID"] <- "id"

names(app_v2_active_data)[names(app_v2_active_data) == "UUID"] <- "id"

# Remove any rows where the same id appears more than once
app_v1_active_data <- 
  app_v1_active_data[!app_v1_active_data$id %in% 
                       app_v1_active_data$id[duplicated(app_v1_active_data$id)], 
                     ]

app_v2_active_data <- 
  app_v2_active_data[!app_v2_active_data$id %in% 
                       app_v2_active_data$id[duplicated(app_v2_active_data$id)], 
                     ]