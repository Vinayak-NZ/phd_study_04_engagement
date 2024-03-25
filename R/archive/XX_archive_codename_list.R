
passive_data <- read.csv("data/app_v2_passive.csv")

test <- passive_data[passive_data$Audience == "A1", ]
test$time_stamp <- as.POSIXct(test$Timestamp, format = "%Y-%m-%dT%H:%M%OS")

setDT(test)
test[ , diff := as.numeric(time_stamp - shift(time_stamp)), by = UserCode]
test$log_in_marker <- ifelse(is.na(test$diff), 1, ifelse(test$diff > 1800, 1, 0))

test[ , log_ins := sum(log_in_marker), by=UserCode]

test <- test[test$UserCode == "TEST01", ]
test2 <- test[order(test$time_stamp), ]

test3 <- test2[!duplicated(test2[ ,c('Codename')]),]
test4 <- test3[, "Codename"]

write.csv(test4, "data/codename_from_data.csv", row.names = FALSE)

test3 <- app_v1_all_aux[!duplicated(app_v1_all_aux[ ,c('page_id')]),]
test4 <- test3[, "page_id"]

write.csv(test4, "data/codename_from_data.csv", row.names = FALSE)
