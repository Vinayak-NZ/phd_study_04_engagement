test2 <- test[test$UserCode == "TEST01", ]

test3 <- test2[test2$log_in_marker > 0, c("UserCode", "time_stamp", "log_in_marker")]
test3$log_in_count <- cumsum(test3$log_in_marker)
test3 <- test3[, c("UserCode", "time_stamp", "log_in_count")]

test4 <- merge(test2, test3, by = c("UserCode", "time_stamp"), all.x = TRUE)
test4 <- test4[order(UserCode, time_stamp),]

test5 <- test4 %>% fill(log_in_count)