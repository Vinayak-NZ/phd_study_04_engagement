## ---- remove-test-cases
app_v1_active_data <- app_v1_active_data[
  !(grepl("TEST", app_v1_active_data$UserCode, fixed = TRUE)),
]

to_match <- c("UUID", "^RiskPer_v", "^OE_", "^CSE_v", "^Int_v", "^PL_v")

test <- app_v1_active_data[, grep(paste(to_match, collapse="|"), 
                               names(app_v1_active_data), value = TRUE)]

names(test)[2:34] <- tolower(names(test)[2:34])

names(test)[names(test) == "UUID"] <- "id"

## ---- remove-test-cases
app_v2_active_data <- app_v2_active_data[
  !(grepl("TEST", app_v2_active_data$UserCode, fixed = TRUE)),
]

to_match <- c("UUID", "^RiskPer_v", "^OE_", "^CSE_v", "^Int_v", "^PL_v")

test <- app_v2_active_data[, grep(paste(to_match, collapse="|"), 
                                  names(app_v2_active_data), value = TRUE)]

names(test)[2:45] <- tolower(names(test)[2:45])

names(test)[names(test) == "UUID"] <- "id"
