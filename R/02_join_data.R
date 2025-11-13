## ---- merge-clinic-records

app_v1_clinic_exclusions <- merge(app_v1_passive_data, 
                              app_v1_clinic, 
                              by = "UserCode", 
                              all.x = TRUE)

app_v1_clinic_exclusions <- app_v1_clinic_exclusions[
  !(app_v1_clinic_exclusions$clinic %in% c("Frankfurt", "Ulm")), ]

app_v1_clinic_exclusions <- app_v1_clinic_exclusions[, 
  !names(app_v1_clinic_exclusions) == "clinic"]

## ---- merge-aux-data

app_v1_all_aux <- merge(app_v1_item_page_match, 
                        app_v1_page_count, 
                        by = "page_id", 
                        all.x = TRUE)

app_v1_passive_data_clean <- merge(app_v1_clinic_exclusions, 
                              app_v1_all_aux, 
                              by = "codename_id", 
                              all.x = TRUE)

app_v2_all_aux <- merge(app_v2_item_page_match, 
                        app_v2_page_count, 
                        by = "page_id", 
                        all.x = TRUE)

app_v2_passive_data_clean <- merge(app_v2_passive_data, 
                              app_v2_all_aux, 
                              by = "codename_id", 
                              all.x = TRUE)
## ---- merge-active-data

app_v1_outcomes <- merge(app_v1_comm_clean, 
                         app_v1_safety_clean, 
                         by = "id")

app_v2_outcomes <- merge(app_v2_comm_clean, 
                         app_v2_safety_clean, 
                         by = "id")

app_v1_active_data_clean <- merge(app_v1_outcomes, 
                         app_v1_hapa_clean, 
                         by = "id")

app_v2_active_data_clean <- merge(app_v2_outcomes, 
                         app_v2_hapa_clean, 
                         by = "id")

app_v1_feedback_added <- merge(app_v1_active_data_clean, 
                         app_v1_feedback, 
                         by = "id")

app_v2_feedback_added <- merge(app_v2_active_data_clean, 
                         app_v2_feedback, 
                         by = "id")

app_v1_complete_data <- merge(app_v1_passive_data_clean, 
                              app_v1_feedback_added, 
                              by = "id")

app_v2_complete_data <- merge(app_v2_passive_data_clean, 
                              app_v2_feedback_added, 
                              by = "id")

## ---- merge-baseline-data

app_v1_complete_data <- merge(app_v1_complete_data, 
                              app_v1_baseline_subset, 
                                  by = "id", 
                              all.x = TRUE)

app_v2_complete_data <- merge(app_v2_complete_data, 
                              app_v2_baseline_subset, 
                              by = "UserCode", 
                              all.x = TRUE)

app_v1_complete_data <- merge(app_v1_complete_data, 
                              app_v1_baseline_demo_j, 
                              by = "id", 
                              all.x = TRUE)

app_v2_complete_data <- merge(app_v2_complete_data, 
                              app_v2_baseline_demo_j, 
                              by = "UserCode", 
                              all.x = TRUE)

## ---- merge-in-app-demographics

app_v1_complete_data <- merge(app_v1_complete_data, 
                              app_v1_demographics, 
                              by = "id")

app_v2_complete_data <- merge(app_v2_complete_data, 
                              app_v2_demographics, 
                              by = "id")

## ---- subset-join-data

app_v1_key <- 
  app_v1_complete_data[, 
                       c("id",
                         "version",
                         "UserCode", 
                         "Timestamp", 
                         "age", 
                         "education", 
                         "fam_comp",
                         "in_app_age_grp", 
                         "in_app_occupation",
                         "pages_completed", 
                         "lessons_completed", 
                         "open_ended_item", 
                         "item_response", 
                         "hapa1_t1", "hapa1_t2", "hapa1_t3", "hapa1_t4",
                         "hapa2_t1", "hapa2_t2", "hapa2_t3", "hapa2_t4",
                         "hapa3_t1", "hapa3_t2", "hapa3_t3", "hapa3_t4",
                         "hapa4_t1", "hapa4_t2", "hapa4_t3", "hapa4_t4",
                         "hapa5_t1", "hapa5_t2", "hapa5_t3", "hapa5_t4",
                         "safe1_t1", "safe2_t1", "safe1_t2", "safe2_t2", 
                         "comm1_t1", "comm1_t2", "comm1_t3", "comm1_t4",
                         "comm2_t1", "comm2_t2", "comm2_t3", "comm2_t4",
                         "comm3_t1", "comm3_t2", "comm3_t3", "comm3_t4",
                         "comm4_t1", "comm4_t2", "comm4_t3", "comm4_t4",
                         "comm5_t1", "comm5_t2", "comm5_t3", "comm5_t4",
                         "comm6_t1", "comm6_t2", "comm6_t3", "comm6_t4",
                         "comm7_t1", "comm7_t2", "comm7_t3", "comm7_t4",
                         "ux", 
                         "content", 
                         "utility")]

app_v2_key <- 
  app_v2_complete_data[, 
                       c("id",
                         "version", 
                         "UserCode", 
                         "Timestamp", 
                         "age", 
                         "education", 
                         "fam_comp",
                         "in_app_age_grp", 
                         "in_app_occupation",
                         "pages_completed", 
                         "lessons_completed", 
                         "open_ended_item", 
                         "item_response", 
                         "hapa1_t1", "hapa1_t2", "hapa1_t3", "hapa1_t4",
                         "hapa2_t1", "hapa2_t2", "hapa2_t3", "hapa2_t4",
                         "hapa3_t1", "hapa3_t2", "hapa3_t3", "hapa3_t4",
                         "hapa4_t1", "hapa4_t2", "hapa4_t3", "hapa4_t4",
                         "hapa5_t1", "hapa5_t2", "hapa5_t3", "hapa5_t4",
                         "safe1_t1", "safe2_t1", "safe1_t2", "safe2_t2", 
                         "comm1_t1", "comm1_t2", "comm1_t3", "comm1_t4",
                         "comm2_t1", "comm2_t2", "comm2_t3", "comm2_t4",
                         "comm3_t1", "comm3_t2", "comm3_t3", "comm3_t4",
                         "comm4_t1", "comm4_t2", "comm4_t3", "comm4_t4",
                         "comm5_t1", "comm5_t2", "comm5_t3", "comm5_t4",
                         "comm6_t1", "comm6_t2", "comm6_t3", "comm6_t4",
                         "comm7_t1", "comm7_t2", "comm7_t3", "comm7_t4",
                         "ux", 
                         "content", 
                         "utility")]

## ---- append-app-data

app_key_vars <- rbind(app_v1_key, app_v2_key)

