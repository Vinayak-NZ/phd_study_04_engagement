## ---- remove-test-cases
remove_test <- complete_data[
  !(grepl("TEST", complete_data$UserCode, fixed = TRUE)),]

## ---- create-log-in-marker

test <- remove_test
test$time_stamp <- as.POSIXct(test$Timestamp, format = "%Y-%m-%dT%H:%M%OS")

setDT(test)
test <- test[order(UserCode, time_stamp),]
test[ , diff := as.numeric(time_stamp - shift(time_stamp)), by = UserCode]
test$log_in_marker <- ifelse(is.na(test$diff), 1, ifelse(test$diff > 1800, 1, 0))

test[ , log_ins := sum(log_in_marker), by=UserCode]

## ---- create-log-in-count
test_subset <- test[test$log_in_marker == 1, 
                    c("UserCode", "time_stamp", "log_in_marker")]
test_subset[, log_in_count := cumsum(log_in_marker), UserCode]
test_subset <- test_subset[, c("UserCode", "time_stamp", "log_in_count")]

test <- as.data.frame(test)
test_subset <- as.data.frame(test_subset)

test_whole <- merge(test, test_subset, by = c("UserCode", "time_stamp"), all.x = TRUE)

setDT(test_whole)
test_whole <- test_whole[order(UserCode, time_stamp),]

test_whole <- test_whole %>% 
  dplyr::group_by(UserCode) %>% 
  fill(log_in_count)

## ---- create-time-per-visit
time_visit <- test_whole %>% 
  group_by(UserCode, log_in_count) %>% 
  summarise(First = first(time_stamp),
            Last = last(time_stamp) , 
            time_spent_visit = difftime(last(time_stamp), first(time_stamp), unit='mins'))

time_visit <- time_visit[, c("UserCode", "log_in_count", "time_spent_visit")]

test_visit_time <- merge(test_whole, time_visit, 
                         by = c("UserCode", "log_in_count"), all.x = TRUE)

## ---- create-days-between

days_between <- test_whole %>% 
  group_by(UserCode) %>% 
  summarise(First = first(time_stamp),
            Last = last(time_stamp) , 
            days_between = difftime(last(time_stamp), first(time_stamp), unit='days'))

days_between$days_between <- round(days_between$days_between, digits = 0)

days_between <- days_between[, c("UserCode", "days_between")]

final_data <- merge(test_visit_time, days_between, 
                         by = "UserCode", all.x = TRUE)





