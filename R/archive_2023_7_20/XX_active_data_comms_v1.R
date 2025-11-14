## ---- process-in-app-data

# rename-variables-id

v1_comm_variable_list <- 
  c("id", 
    "^SACCIASU_v", 
    "^SACCIAAC_v", 
    "^SACCIAAC1_v", 
    "^SACCIACA_v", 
    "^SACCIACO1_v", 
    "^SACCIACO2_v", 
    "^SACCIAIA1_v", 
    "^SACCIAIA2_v")

app_v1_active_subset_comm <- 
  app_v1_active_data[, grep(paste(v1_comm_variable_list, collapse="|"), 
                            names(app_v1_active_data), value = TRUE)]

names(app_v1_active_subset_comm)[2:ncol(app_v1_active_subset_comm)] <- 
  tolower(names(app_v1_active_subset_comm)[2:ncol(app_v1_active_subset_comm)])

subset_t2t3t4 <- app_v1_active_subset_comm[
  , grep("_t2|_t3|_t4", names(app_v1_active_subset_comm), value = TRUE)
]

# replace known non-numeric placeholders with NA

app_v1_active_subset_comm[app_v1_active_subset_comm == "" |
                            app_v1_active_subset_comm == " " |
                            app_v1_active_subset_comm == "." |
                            app_v1_active_subset_comm == "NA" |
                            app_v1_active_subset_comm == "-1x"] <- NA

# remove entries that look like dates (contain '.' and 4 digits)

app_v1_active_subset_comm[] <- lapply(app_v1_active_subset_comm, function(x) {
  x[grepl("\\d{2}\\.\\d{2}\\.\\d{4}", x)] <- NA
  x
})

# now convert all non-id columns to numeric

app_v1_active_subset_comm[, -1] <-
  lapply(app_v1_active_subset_comm[, -1], function(x) as.numeric(trimws(x)))

# rename-variables-comm

app_v1_active_subset_comm <- comm_rename_v1(app_v1_active_subset_comm)


app_v1_comm_constructs <- remove_redundancy(app_v1_active_subset_comm, 
                                            comm_constructs)

comm_con_list <- c("comm1", 
                   "comm2", 
                   "comm3", 
                   "comm4", 
                   "comm5", 
                   "comm6", 
                   "comm7")

app_v1_comm_list <- lapply(comm_con_list, 
                           tidy_con, 
                           data = app_v1_comm_constructs, 
                           time = 4)

app_v1_comm_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                               app_v1_comm_list) 

app_v1_comm_list_rev <- lapply(comm_con_list, 
                               score_edit_multiple, 
                               data = app_v1_comm_modified, 
                               time = 4)

app_v1_comm_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v1_comm_list_rev)

# rename-variables-safety

v1_safety_variable_list <- 
  c("id", 
    "^VUEIC1_v", 
    "^VUEIC2_v")

app_v1_active_subset_safe <- 
  app_v1_active_data[, grep(paste(v1_safety_variable_list, collapse="|"), 
                            names(app_v1_active_data), value = TRUE)]

names(app_v1_active_subset_safe)[2:ncol(app_v1_active_subset_safe)] <- 
  tolower(names(app_v1_active_subset_safe)[2:ncol(app_v1_active_subset_safe)])

# replace known non-numeric placeholders with NA

app_v1_active_subset_safe[app_v1_active_subset_safe == "" |
                            app_v1_active_subset_safe == " " |
                            app_v1_active_subset_safe == "." |
                            app_v1_active_subset_safe == "NA" |
                            app_v1_active_subset_safe == "-1x"] <- NA

# remove entries that look like dates (contain '.' and 4 digits)

app_v1_active_subset_safe[] <- lapply(app_v1_active_subset_safe, function(x) {
  x[grepl("\\d{2}\\.\\d{2}\\.\\d{4}", x)] <- NA
  x
})

# now convert all non-id columns to numeric

app_v1_active_subset_safe[, -1] <-
  lapply(app_v1_active_subset_safe[, -1], function(x) as.numeric(trimws(x)))

app_v1_active_subset_safety <- safety_rename(app_v1_active_subset_safe)

app_v1_safety_constructs <- remove_redundancy(app_v1_active_subset_safety, 
                                              safey_constructs)

safety_con_list <- c("safe1", "safe2")

app_v1_safety_list <- lapply(safety_con_list, 
                             tidy_con,
                             data = app_v1_safety_constructs, 
                             time = 2)

app_v1_safety_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                                 app_v1_safety_list)

app_v1_safety_list_rev <- lapply(safety_con_list, 
                                 score_edit_multiple, 
                                 data = app_v1_safety_modified, 
                                 time = 2)

app_v1_safety_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                              app_v1_safety_list_rev) 

