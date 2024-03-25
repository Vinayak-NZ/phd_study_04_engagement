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

app_v1_active_data_clean <- merge(app_v1_hapa_clean, 
                                  app_v1_safety_clean, 
                                  by = "id")

app_v2_active_data_clean <- merge(app_v2_hapa_clean, 
                                  app_v2_safety_clean, 
                                  by = "id")

app_v1_complete_data <- merge(app_v1_passive_data_clean, 
                              app_v1_active_data_clean, 
                              by = "id")

app_v2_complete_data <- merge(app_v2_passive_data_clean, 
                              app_v2_active_data_clean, 
                              by = "id")

## ---- merge-baseline-data

app_v1_complete_data <- merge(app_v1_complete_data, 
                              app_v1_baseline_subset, 
                                  by = "id", 
                              all.x = TRUE)

## ---- subset-join-data

app_v1_key <- 
  app_v1_complete_data[, 
                       c("id",
                         "version",
                         "UserCode", 
                         "Timestamp", 
                         "pages_completed", 
                         "lessons_completed", 
                         "open_ended_item", 
                         "item_response", 
                         "hapa1_t1", 
                         "hapa2_t1", 
                         "hapa3_t1", 
                         "hapa4_t1", 
                         "hapa5_t1", 
                         "safe1_t1", 
                         "safe2_t1", 
                         "hapa2_t0", 
                         "hapa3_t0",
                         "hapa4_t0", 
                         "hapa5_t0",
                         "safe1_t0", 
                         "safe2_t0")]

app_v2_key <- 
  app_v2_complete_data[, 
                       c("id",
                         "version", 
                         "UserCode", 
                         "Timestamp", 
                         "pages_completed", 
                         "lessons_completed", 
                         "open_ended_item", 
                         "item_response", 
                         "hapa1_t1", 
                         "hapa2_t1", 
                         "hapa3_t1", 
                         "hapa4_t1", 
                         "hapa5_t1", 
                         "safe1_t1", 
                         "safe2_t1")]

app_v2_key$hapa2_t0 <- mean(app_v2_key$hapa2_t1, na.rm = TRUE)

app_v2_key$hapa3_t0 <- mean(app_v2_key$hapa3_t1, na.rm = TRUE)

app_v2_key$hapa4_t0 <- mean(app_v2_key$hapa4_t1, na.rm = TRUE)

app_v2_key$hapa5_t0 <- mean(app_v2_key$hapa5_t1, na.rm = TRUE)

app_v2_key$safe1_t0 <- mean(app_v2_key$safe1_t1, na.rm = TRUE)

app_v2_key$safe2_t0 <- mean(app_v2_key$safe2_t1, na.rm = TRUE)

## ---- append-app-data

app_key_vars <- rbind(app_v1_key, app_v2_key)

