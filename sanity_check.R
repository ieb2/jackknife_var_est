small_sanity_check <- lapply(c(1:100), function(x) {
  df <- list_of_sim_data[[x]]
  
  imps <- bootImpute::bootMice(df, nBoot = 300, nImp = 5, nCores = 6, seed = 123, print = FALSE)
  
  estimated_var <- sapply(imps, function(x)
    lm(formula = outcome_variable ~ V1 + V2 + V3, data = x) %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  )
  estimated_var_vec <- as.vector(estimated_var)
  
  var_point_estimate <- mean(estimated_var_vec)
  
  UB <- quantile(estimated_var_vec, 0.975)
  LB <- quantile(estimated_var_vec, 0.025)
  
  
  true_var <- list_of_sim_data_for_comp[[x]] %>%
    lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
    broom::tidy() %>%
    dplyr::filter(term == "V1") %>%
    dplyr::select(estimate) %>%
    unlist()
  
  temp_df <- data.frame("point_estimate" = var_point_estimate, 
                        "UB" = UB, 
                        "LB" = LB, 
                        "true_variance" = true_var) %>% tibble::remove_rownames()
  
  final_df <- temp_df %>%
    mutate("is_between" = ifelse(true_variance > LB & true_variance < UB, TRUE, FALSE))
  
  return(final_df)
})

small_sanity_check_df <- do.call(rbind, small_sanity_check)

CI_coverage <- sum(small_sanity_check_df$is_between)/nrow(small_sanity_check_df) * 100


# Try larger samples 
