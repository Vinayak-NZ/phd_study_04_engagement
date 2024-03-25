## ---- subset-active-data-hapa

# Version 1
v1_hapa_variable_list <- 
  c("id", 
    "^RiskPer_v", 
    "^OE_", 
    "^CSE_v", 
    "^Int_v", 
    "^PL_v")

app_v1_active_subset_hapa <- 
  app_v1_active_data[, grep(paste(v1_hapa_variable_list, collapse="|"), 
                            names(app_v1_active_data), value = TRUE)]

names(app_v1_active_subset_hapa)[2:ncol(app_v1_active_subset_hapa)] <- 
  tolower(names(app_v1_active_subset_hapa)[2:ncol(app_v1_active_subset_hapa)])

# Version 2
v2_hapa_variable_list <- 
  c("id", 
    "^RiskPer_v", 
    "^OE_", 
    "^CSE_v", 
    "^Int_v", 
    "^PL_v")

app_v2_active_subset_hapa <- 
  app_v2_active_data[, grep(paste(v2_hapa_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_subset_hapa)[2:ncol(app_v2_active_subset_hapa)] <- 
  tolower(names(app_v2_active_subset_hapa)[2:ncol(app_v2_active_subset_hapa)])

## ---- rename-hapa

app_v1_active_subset_hapa <- hapa_rename(app_v1_active_subset_hapa)

app_v2_active_subset_hapa <- hapa_rename(app_v2_active_subset_hapa)

## ---- remove-redundancies-hapa
app_v1_hapa_constructs <- remove_redundancy(app_v1_active_subset_hapa, 
                                            hapa_constructs)

app_v2_hapa_constructs <- remove_redundancy(app_v2_active_subset_hapa, 
                                            hapa_constructs)

## ---- adopt-most-recent-score-hapa
hapa_con_list <- c("hapa1", "hapa2", "hapa3", "hapa4", "hapa5")

app_v1_hapa_list <- lapply(hapa_con_list, 
                           tidy_con, 
                           data = app_v1_hapa_constructs, 
                           time = 4)

app_v1_hapa_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                               app_v1_hapa_list) 

app_v2_hapa_list <- lapply(hapa_con_list, 
                           tidy_con, 
                           data = app_v2_hapa_constructs, 
                           time = 4)

app_v2_hapa_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                               app_v2_hapa_list) 

## ---- clean-scores-hapa
hapa_con_list <- c("hapa1", "hapa2", "hapa3", "hapa4", "hapa5")

app_v1_hapa_list_rev <- lapply(hapa_con_list, 
                               score_edit_multiple, 
                               data = app_v1_hapa_modified, 
                               time = 4)

app_v1_hapa_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v1_hapa_list_rev) 

app_v2_hapa_list_rev <- lapply(hapa_con_list, 
                               score_edit_multiple, 
                               data = app_v2_hapa_modified, 
                               time = 4)

app_v2_hapa_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v2_hapa_list_rev)
