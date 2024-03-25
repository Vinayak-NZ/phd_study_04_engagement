## ---- pages-viewed

setDT(final_data)
final_data[, pages_viewed := max(pages_completed), UserCode]

## ---- lessons-viewed

final_data[, lessons_viewed := max(lessons_completed), UserCode]

## ---- open_ended_items
final_data$open_ended_items_completed <- ifelse(final_data$open_ended_item == 1 & 
                                                     !is.na(final_data$Value), 1, 0)

final_data[, total_open_items := sum(open_ended_items_completed), UserCode]

## ---- action-plan

final_data$action_plan <- ifelse(final_data$lessons_viewed > 8, 1, 0)
