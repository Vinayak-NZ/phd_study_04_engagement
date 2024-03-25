## ---- remove-test-cases
app_key_vars_valid <- app_key_vars[
  !(grepl("TEST", app_key_vars$UserCode, fixed = TRUE)),
]

## ---- create-log-in-marker

app_key_vars_valid$time_stamp <- as.POSIXct(app_key_vars_valid$Timestamp,
  format = "%Y-%m-%dT%H:%M%OS"
)

setDT(app_key_vars_valid)

app_key_vars_valid <- app_key_vars_valid[order(version, id, time_stamp), ]

app_key_vars_valid[,
  diff := as.numeric(time_stamp - shift(time_stamp)),
  by = c("version", "id")
]

app_key_vars_valid$log_in_marker <- ifelse(is.na(app_key_vars_valid$diff),
  1,
  ifelse(app_key_vars_valid$diff > 1800,
    1,
    0
  )
)

app_key_vars_valid[, 
  total_log_in := sum(log_in_marker), 
  by = c("version", "id")]

## ---- create-log-in-count
unique_log_in <- app_key_vars_valid[
  app_key_vars_valid$log_in_marker == 1,
  c("version", "id", "time_stamp", "log_in_marker")
]

unique_log_in[, 
  log_in_count := cumsum(log_in_marker), 
  by = c("version", "id")]

unique_log_in <- unique_log_in[, c("version",
                                   "id", 
                                   "time_stamp", 
                                   "log_in_count")]

app_key_vars_valid <- as.data.frame(app_key_vars_valid)

unique_log_in <- as.data.frame(unique_log_in)

app_key_vars_log_ins <- merge(app_key_vars_valid,
  unique_log_in,
  by = c("version", 
         "id", 
         "time_stamp"), all.x = TRUE
)

setDT(app_key_vars_log_ins)

app_key_vars_log_ins <- app_key_vars_log_ins[order(version, 
                                                   id, 
                                                   time_stamp), ]

app_key_vars_log_ins <- app_key_vars_log_ins %>%
  dplyr::group_by(version, id) %>%
  fill(log_in_count)

## ---- create-time-per-visit
time_visit <- app_key_vars_log_ins %>%
  group_by(version, id, log_in_count) %>%
  summarise(
    First = first(time_stamp),
    Last = last(time_stamp),
    time_spent_visit = difftime(last(time_stamp),
      first(time_stamp),
      unit = "mins"
    )
  )

time_visit <- time_visit[, c(
  "version",
  "id",
  "log_in_count",
  "time_spent_visit"
)]

app_key_vars_time <- merge(app_key_vars_log_ins,
  time_visit,
  by = c("version", "id", "log_in_count"),
  all.x = TRUE
)

## ---- create-days-between

days_between <- app_key_vars_log_ins %>%
  group_by(version, id) %>%
  summarise(
    First = first(time_stamp),
    Last = last(time_stamp),
    days_between = difftime(last(time_stamp), first(time_stamp), unit = "days")
  )

days_between$days_between <- round(days_between$days_between, digits = 0)

days_between <- days_between[, c("version", "id", "days_between")]

final_data <- merge(app_key_vars_time, days_between,
  by = c("version", "id"), all.x = TRUE
)

final_data_key_vars <- final_data[, c("id",
                                      "version", 
                                      "UserCode", 
                                      "log_in_count",
                                      "pages_completed", 
                                      "lessons_completed", 
                                      "open_ended_item", 
                                      "total_log_in", 
                                      "time_spent_visit", 
                                      "days_between", 
                                      "item_response", 
                                      "hapa2_t0",
                                      "hapa3_t0", 
                                      "hapa4_t0", 
                                      "hapa5_t0",
                                      "safe1_t0", 
                                      "safe2_t0",
                                      "hapa1_t1", 
                                      "hapa2_t1", 
                                      "hapa3_t1", 
                                      "hapa4_t1", 
                                      "hapa5_t1", 
                                      "safe1_t1", 
                                      "safe2_t1")]
