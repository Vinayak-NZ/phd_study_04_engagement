## ---- remove-redundancy
remove_redundancy <- function(data, output){
  
  t2_vars <- grep("t2", names(data), value=TRUE)
  t3_vars <- grep("t3", names(data), value=TRUE)
  
  output <- data[,!(names(data) %in% c(t2_vars, t3_vars))]
  
  return(output)
  
}

## ---- rename-hapa
hapa_rename <- function(data){
  
  names(data) <- gsub("riskper", "hapa1", names(data))
  names(data) <- gsub("oe", "hapa2", names(data))
  names(data) <- gsub("cse", "hapa3", names(data))
  names(data) <- gsub("int", "hapa4", names(data))
  names(data) <- gsub("pl", "hapa5", names(data))
  
  return(data)
  
}

## ---- rename-safety
safety_rename <- function(data){
  
  names(data) <- gsub("vueic1", "safe1", names(data))
  names(data) <- gsub("vueic2", "safe2", names(data))
  
  return(data)
  
}

## ---- modify-repeated-vars
tidy_rep_var <- function(var, data){
  
  var_relabel <- substr(var, 1, 5)
  
  t <- substr(var, 8, 8)
  
  var_list <- grep(var, names(data), value=TRUE)
  
  if(length(grep("_a2", var_list)) > 0) {
    
    data[[paste0(var_relabel, "_t", t)]] <- ifelse(!is.na(data[[paste0(var, "_t1", "_a2")]]), 
                                                   data[[paste0(var, "_t1", "_a2")]], 
                                                   data[[paste0(var, "_t1", "_a1")]])
    
  } else {
    
    data[[paste0(var_relabel, "_t", t)]] <- data[[paste0(var, "_t1","_a1")]]
    
  }
  
  data[[paste0(var_relabel, "_t", t)]] <- as.numeric(data[[paste0(var_relabel, "_t", t)]])
  
  data <- data[, c("id", paste0(var_relabel, "_t", t))]
  
  return(data)  
  
}

## ---- modify-construct
tidy_con <- function(con, data, time){
  
  time_points <- paste0("_v", 1:time)
  
  con_list <- paste0(con, time_points)
  
  modified_con <- lapply(con_list, tidy_rep_var, data = data)
  
  data_output <- Reduce(function(x, y) merge(x, y, by = "id", ), modified_con) 
  
  return(data_output)
  
}

## ---- edit-scores
score_edit <- function(var, data){
  
  data[[paste0(var)]] <- ifelse(data[[paste0(var)]] < 0, 0, data[[paste0(var)]])
  
  data <- data[, c("id", paste0(var))]
  
  return(data)
  
}

## ---- edit-multiple-scores
score_edit_multiple <- function(con, data, time){
  
  time_points <- paste0("_t", 1:time)
  
  con_list <- paste0(con, time_points)
  
  modified_con <- lapply(con_list, score_edit, data = data)
  
  data_output <- Reduce(function(x, y) merge(x, y, by = "id"), modified_con) 
  
  return(data_output)
  
}
