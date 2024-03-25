## ---- impute-predictors

final_data_key_vars$hapa1_t1 <- ifelse(is.na(final_data_key_vars$hapa1_t1), 
                                       final_data_key_vars$hapa3_t0, 
                                       final_data_key_vars$hapa1_t1)

final_data_key_vars$hapa2_t1 <- ifelse(is.na(final_data_key_vars$hapa2_t1), 
                                       final_data_key_vars$hapa2_t0, 
                                       final_data_key_vars$hapa2_t1)

final_data_key_vars$hapa3_t1 <- ifelse(is.na(final_data_key_vars$hapa3_t1), 
                                       final_data_key_vars$hapa3_t0, 
                                       final_data_key_vars$hapa3_t1)

final_data_key_vars$safe1_t1 <- ifelse(is.na(final_data_key_vars$safe1_t1), 
                                       final_data_key_vars$safe1_t0, 
                                       final_data_key_vars$safe1_t1)

final_data_key_vars$safe2_t1 <- ifelse(is.na(final_data_key_vars$safe2_t1), 
                                       final_data_key_vars$safe2_t0, 
                                       final_data_key_vars$safe2_t1)