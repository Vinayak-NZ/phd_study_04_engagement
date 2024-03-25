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

## ---- derive-progress-score

# weighted-average

unique_cases <- 
  distinct(final_data_key_vars, version, id, .keep_all = TRUE)

unique_cases$progress_score <- 
  (unique_cases$lessons_completed*10)*0.5 + 
  (unique_cases$proportion_pages_viewed*0.3) + 
  (unique_cases$proportion_items_completed*0.2)

unique_cases_subset <- 
  unique_cases[, c("version", "id", "progress_score")]

final_data_all_vars <- merge(final_data_key_vars, 
                             unique_cases_subset, 
                             by = c("version", "id"))
