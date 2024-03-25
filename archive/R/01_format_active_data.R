## ---- rename-app-id-active

names(app_v1_active_data)[names(app_v1_active_data) == "UUID"] <- "id"

names(app_v2_active_data)[names(app_v2_active_data) == "UUID"] <- "id"
