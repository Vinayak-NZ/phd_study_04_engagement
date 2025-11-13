## ---- app-demographics

app_v1_demographics <- app_v1_active_data[, c("id", "AgeGroup", "Occupation")]

app_v2_demographics <- app_v2_active_data[, c("id", "AgeGroup", "Occupation")]

names(app_v1_demographics)[names(app_v1_demographics) == "AgeGroup"] <- 
  "in_app_age_grp"

names(app_v1_demographics)[names(app_v1_demographics) == "Occupation"] <- 
  "in_app_occupation"

names(app_v2_demographics)[names(app_v2_demographics) == "AgeGroup"] <- 
  "in_app_age_grp"

names(app_v2_demographics)[names(app_v2_demographics) == "Occupation"] <- 
  "in_app_occupation"