# Scratch Code--Likely Trash 

imps <- mice(df_w_mis, m = 10, maxit = 50, method = "pmm", print = FALSE) %>%
  complete("all")

subsamples <- vector("list", length = {length(imps)})

for(i in 1:length(imps)) {
  for (z in 1:nrow(imps[[i]])) {
    subsamples[[i]][[z]] <- imps[[i]][-z,]
  }
}

analysis_vector <- c()
for(i in 1:length(imps)) {
  for (z in 1:nrow(imps[[i]])) {
  analysis_vector[[i]]  <- subsamples[[i]][[z]] %>%
      lm(outcome_variable ~ V1 + V2 + V3, data = .) %>%
      summary() %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  }
}


mean(unlist(analysis_vector))
