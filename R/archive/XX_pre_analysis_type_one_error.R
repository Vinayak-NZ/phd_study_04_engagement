## ---- derive-weighted-average

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
