## ---- derive-index-score

unique_cases <- 
  distinct(final_data_key_vars, version, id, .keep_all = TRUE)

pca_input <- unique_cases[, c("proportion_pages_viewed", 
                              "lessons_viewed", 
                              "proportion_items_completed")]

data_normalised <- scale(pca_input)
head(data_normalised)

corr_matrix <- cor(data_normalised)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1]

fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

pca.results <- princomp(pca_input)
unique_cases$progress_scores <- pca.results$scores[1]