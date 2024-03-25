## ---- derive-index-score

pca_input <- model_input[, c("proportion_pages_viewed", 
                             "lessons_viewed", 
                             "proportion_items_completed")]

pca_results <- principal(pca_input, nfactors = 1)

model_input$progress_score <- rescale(pca_results$scores, to = c(0, 1))
