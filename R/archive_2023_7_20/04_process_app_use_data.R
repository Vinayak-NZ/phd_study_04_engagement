
unique_cases <- 
  distinct(final_data_all_vars, version, id, .keep_all = TRUE)

unique_cases <- 
  as.data.frame(unique_cases)

variables_impute <- c("hapa2_t0", 
               "hapa3_t0", 
               "hapa4_t0", 
               "hapa5_t0", 
               "hapa1_t1", 
               "hapa2_t1")

remainder <- unique_cases[, !(names(unique_cases) %in% variables_impute)]

to_impute <- unique_cases[, c("version",
                           "id", 
                           variables_impute)]

init <- mice(to_impute, maxit = 0)
meth <- init$method
predM <- as.matrix(init$predictorMatrix)

predM[, "version"] <- 0
predM[, "id"] <- 0
predM[, "hapa1_t1"] <- 0
predM[, "hapa2_t1"] <- 0


meth[c("version", "id", "hapa2_t0", "hapa3_t0", "hapa4_t0", "hapa5_t0")] = ""

imputed <- mice(to_impute, 
                method=meth, 
                predictorMatrix=predM, 
                m=5, 
                print = FALSE)
#long <- complete(imputed, action = 'long', include = TRUE)
imputed <- complete(imputed)

post_imputed <- merge(remainder, imputed, by = c("version", "id"))
