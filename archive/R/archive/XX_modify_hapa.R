## ---- rename-hapa-vars
phase_three_hapa_constructs <- hapa_rename(test)

## ---- remove-redundancy-hapa
hapa_constructs <- remove_redundancy(phase_three_hapa_constructs, hapa_constructs)

## ---- edit-hapa-outcomes
hapa_con_list <- c("hapa1", "hapa2", "hapa3", "hapa4", "hapa5")

modified_hapa_vars <- lapply(hapa_con_list, tidy_con, data = hapa_constructs)

hapa_constructs_edited <- Reduce(function(x, y) merge(x, y, by = "id"), modified_hapa_vars) 

## ---- edit-hapa-scores
hapa_con_list <- c("hapa1", "hapa2", "hapa3", "hapa4", "hapa5")

modified_hapa_scores <- lapply(hapa_con_list, hapa_edit_multiple, data = hapa_constructs_edited)

hapa_scores_edited <- Reduce(function(x, y) merge(x, y, by = "id"), modified_hapa_scores) 







