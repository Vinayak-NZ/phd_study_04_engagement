## ---- pages-viewed

setDT(final_data_key_vars)

final_data_key_vars[, 
  pages_viewed := max(pages_completed, na.rm = TRUE), 
  by = c("version", "id")]

final_data_key_vars$proportion_pages_viewed <- ifelse(final_data_key_vars$version == "Version 1", 
                                (final_data_key_vars$pages_viewed/141)*100,
                                (final_data_key_vars$pages_viewed/112)*100)

## ---- lessons-viewed

final_data_key_vars[, 
  lessons_viewed := max(lessons_completed, na.rm = TRUE), 
  by = c("version", "id")]

## ---- open_ended_items
final_data_key_vars$open_ended_items_completed <- 
  ifelse(final_data_key_vars$open_ended_item == 0 | is.na(final_data_key_vars$item_response), 0, 
         ifelse(
           final_data_key_vars$open_ended_item == 1 & 
             nchar(final_data_key_vars$item_response) > 10, 1, 0)       
         )

final_data_key_vars[, 
  items_available := sum(open_ended_item, na.rm = TRUE), 
  by = c("version", "id")]

final_data_key_vars[, 
  total_open_items := sum(open_ended_items_completed, na.rm = TRUE), 
  by = c("version", "id")]

final_data_key_vars$proportion_items_completed <- 
  ifelse(final_data_key_vars$items_available == 0, 0,
  ifelse(final_data_key_vars$version == "Version 1", 
         (final_data_key_vars$total_open_items/final_data_key_vars$items_available)*100, 
         (final_data_key_vars$total_open_items/final_data_key_vars$items_available)*100))

## ---- action-plan

final_data_key_vars$action_plan <- 
  ifelse(final_data_key_vars$lessons_viewed > 8, 1, 0)

## ---- average-time

final_data_key_vars[, 
                    average_time_spent := mean(time_spent_visit), 
                    by = c("version", "id")]

## ---- user-ratings

final_data_key_vars[, ux := as.numeric(ux)]

final_data_key_vars[, content := as.numeric(content)]

final_data_key_vars[, utility := as.numeric(utility)]

## ---- compile-data

final_data_all_vars <- 
  final_data_key_vars[, 
                       c("id",
                         "version", 
                         "UserCode", 
                         "age", 
                         "education", 
                         "fam_comp",
                         "in_app_age_grp", 
                         "in_app_occupation",
                         "item_response", 
                         "total_log_in", 
                         "average_time_spent", 
                         "days_between", 
                         "proportion_pages_viewed",
                         "lessons_viewed",
                         "proportion_items_completed",
                         "action_plan",
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
